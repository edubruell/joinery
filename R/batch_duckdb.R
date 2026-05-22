if (!rlang::is_installed(c("duckdb", "DBI", "dplyr", "dbplyr"))) {
  return(invisible(NULL))
}


#' Create a Batch Plan for DuckDB Table Processing
#'
#' Analyzes a DuckDB table and generates a batch plan (data.table) that defines
#' how to split the table into atomic processing units. Each row of the plan
#' represents one batch with row counts, optional row-number windows, and block
#' identifiers (if blocking is used).
#'
#' The function supports three chunking strategies:
#' - `"even"`: Simple row-number chunking, ignores blocks
#' - `"block_first"`: Each batch = one block (or sub-chunks if block > target_batch_size)
#' - `"block_consolidated"`: Consolidates small blocks to minimize batch count (default)
#'
#' @param db_tbl A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)
#' @param id Character. Column name(s) to use as record identifier(s). Not used for batching
#'   but validated to exist in the table.
#' @param target_batch_size Positive integer. Target number of rows per batch.
#'   Default: 1e6 (1 million rows).
#' @param min_batch_size Positive integer. Minimum table size to trigger batching.
#'   If total rows < min_batch_size, returns single batch. Default: 1e5 (100k rows).
#' @param chunk_strategy Character. One of `"even"`, `"block_first"`, or `"block_consolidated"`.
#'   Default: `"block_consolidated"`.
#' @param block_by Optional character vector. Column name(s) to use for semantic blocking.
#'   If specified, batches respect block boundaries. Supports multiple columns (e.g., c("region", "year")).
#'
#' @return A `data.table` with columns:
#'   - `batch_id`: integer, sequential batch identifier (1, 2, 3, ...)
#'   - `row_count`: integer, number of rows in this batch
#'   - `row_start`: integer (or NA), window start for row-number-based batches; NA for block-based
#'   - `row_end`: integer (or NA), window end for row-number-based batches; NA for block-based
#'   - Additional columns (if `block_by` specified): one per blocking variable, containing block values
#'
#' @details
#' **Small tables**: If total rows < `min_batch_size`, returns a single batch regardless
#' of strategy. With blocking, still respects blocks.
#'
#' **Row-number windows**: For unblocked or large-block sub-chunking, `row_start` and
#' `row_end` define window boundaries (1-based, inclusive). For block-based batches
#' (small blocks), these are NA.
#'
#' **Consolidation**: `"block_consolidated"` (default) combines multiple small blocks
#' into single batches up to `target_batch_size` to reduce overhead. Each batch may
#' contain zero, one, or multiple blocks (depending on sizes and consolidation).
#'
#' **Row ordering**: To ensure `row_start` and `row_end` windows are consecutive and
#' can be reliably sliced from the DB, the function sorts by the `id` column before
#' computing row numbers. This ensures reproducible, deterministic batch boundaries.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' DBI::dbWriteTable(con, "data", data.frame(id = 1:1e6, region = sample(LETTERS, 1e6, TRUE)))
#' tbl_ref <- dplyr::tbl(con, "data")
#'
#' # Unblocked, even row-number chunking
#' plan1 <- duckdb_batch_plan(
#'   tbl_ref, id = "id",
#'   target_batch_size = 1e5, chunk_strategy = "even"
#' )
#'
#' # Blocked, consolidated strategy (default, respects regions)
#' plan2 <- duckdb_batch_plan(
#'   tbl_ref, id = "id",
#'   target_batch_size = 1e5, block_by = "region"
#' )
#' }
#'
#' @export
duckdb_batch_plan <- function(db_tbl,
                              id,
                              target_batch_size = NULL,
                              min_batch_size = NULL,
                              chunk_strategy = "block_consolidated",
                              block_by = NULL) {
  
  # -----------------------------------------------
  # Validate user input (NULL allowed for tuning)
  # -----------------------------------------------
  c(
    "db_tbl must be a dplyr lazy table" =
      inherits(db_tbl, c("tbl_duckdb_connection", "tbl_dbi", "tbl_lazy")),
    "id must be a character vector" =
      is.character(id),
    "target_batch_size must be NULL or positive" =
      is.null(target_batch_size) || (is.numeric(target_batch_size) && target_batch_size > 0),
    "min_batch_size must be NULL or positive" =
      is.null(min_batch_size) || (is.numeric(min_batch_size) && min_batch_size > 0),
    "chunk_strategy must be one of 'even', 'block_first', or 'block_consolidated'" = 
      chunk_strategy %in% c("even", "block_first", "block_consolidated"),
    "block_by must be NULL or a character vector" =
      is.null(block_by) || is.character(block_by)
  ) |> validate_inputs()
  
  # -----------------------------------------------
  # Auto-tune if missing
  # -----------------------------------------------
  if (is.null(target_batch_size) || is.null(min_batch_size)) {
    con <- dbplyr::remote_con(db_tbl)
    tbl_name <- db_tbl$lazy_query$x
    
    params <- suggest_batch_params(
      con   = con,
      table = tbl_name,
      block_by = block_by
    )
    
    if (is.null(target_batch_size)) target_batch_size <- params$target_batch_size
    if (is.null(min_batch_size))     min_batch_size     <- params$min_batch_size
    
    pnum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
    cli::cli_alert_info(
      "Auto-tuned batch sizes: target {pnum(target_batch_size)}, min {pnum(min_batch_size)}"
    )
    cat("\n")
  }
  
  # -----------------------------------------------
  # Compute total rows
  # -----------------------------------------------
  total_rows <- db_tbl |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::pull(n)
  
  # -----------------------------------------------
  # Small-table fast-path ONLY when unblocked
  # -----------------------------------------------
  if (total_rows < min_batch_size && is.null(block_by)) {
    if (total_rows == 0L) {
      return(data.table::data.table(
        batch_id  = integer(),
        row_count = integer(),
        row_start = integer(),
        row_end   = integer()
      ))
    }
    return(data.table::data.table(
      batch_id  = 1L,
      row_count = as.integer(total_rows),
      row_start = 1L,
      row_end   = as.integer(total_rows)
    ))
  }
  
  # -----------------------------------------------
  # NO BLOCKS → even row-number batching
  # -----------------------------------------------
  if (is.null(block_by) || length(block_by) == 0) {
    return(
      .chunk_even(
        db_tbl            = db_tbl,
        id_col            = id[1],
        target_batch_size = target_batch_size
      )
    )
  }
  
  # -----------------------------------------------
  # WITH BLOCKING → dispatch strategy
  # -----------------------------------------------
  switch(
    chunk_strategy,
    
    "even" =
      .chunk_even(
        db_tbl            = db_tbl,
        id_col            = id[1],
        target_batch_size = target_batch_size
      ),
    
    "block_first" =
      .chunk_block_first(
        db_tbl            = db_tbl,
        block_by          = block_by,
        target_batch_size = target_batch_size,
        id_col            = id[1]
      ),
    
    "block_consolidated" =
      .chunk_block_consolidated(
        db_tbl            = db_tbl,
        block_by          = block_by,
        target_batch_size = target_batch_size,
        id_col            = id[1]
      )
  )
}


#' Compute Block Statistics from DuckDB Table
#'
#' Queries the database to count rows per block (defined by block_by columns).
#' Returns a data.table with one row per unique block combination and a column `n`
#' containing the row count for that block.
#'
#' @param db_tbl A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)
#' @param block_by Character vector. Column name(s) defining blocks.
#'
#' @return A `data.table` with columns from `block_by` plus `n` (integer row counts).
#'   Returns NULL if `block_by` is NULL or empty.
#'
#' @noRd
.compute_block_stats <- function(db_tbl, block_by) {
  if (is.null(block_by) || length(block_by) == 0) {
    return(NULL)
  }
  
  block_cols <- rlang::syms(block_by)
  
  db_tbl |>
    dplyr::group_by(!!!block_cols) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::collect() |>
    data.table::as.data.table()
}


#' Create Even Row-Number Batches
#'
#' Divides a table into batches of approximately equal size using row-number windows.
#' Ignores any blocking structure and creates simple sequential chunks.
#'
#' @param total_rows Integer. Total number of rows in the table.
#' @param target_batch_size Positive integer. Target number of rows per batch.
#'
#' @return A `data.table` with columns:
#'   - `batch_id`: integer, sequential batch identifier
#'   - `row_count`: integer, number of rows in this batch
#'   - `row_start`: integer, 1-based inclusive start of row window
#'   - `row_end`: integer, 1-based inclusive end of row window
#'
#' @details
#' The final batch may contain fewer rows than `target_batch_size` if `total_rows`
#' is not evenly divisible. All batches except the last contain exactly
#' `target_batch_size` rows.
#'
#' @noRd
.chunk_even <- function(db_tbl, id_col, target_batch_size) {
  # get connection
  con <- dbplyr::remote_con(db_tbl)
  
  # quoted identifiers
  id_q <- DBI::dbQuoteIdentifier(con, id_col)
  
  # inner query
  inner_sql <- dbplyr::sql_render(db_tbl, con = con)
  
  # assign row numbers deterministically
  sql_rn <- glue::glue("
    SELECT
      rn,
      1 AS dummy
    FROM (
      SELECT
        ROW_NUMBER() OVER (ORDER BY {id_q}) AS rn
      FROM ({inner_sql}) AS x
    ) AS t
    ORDER BY rn
  ")
  
  # pull rn vector to compute batch windows
  rn_dt <- DBI::dbGetQuery(con, sql_rn)
  total_rows <- nrow(rn_dt)
  
  if (total_rows == 0) {
    return(data.table::data.table(
      batch_id = integer(),
      row_count = integer(),
      row_start = integer(),
      row_end = integer()
    ))
  }
  
  # number of batches
  n_batches <- ceiling(total_rows / target_batch_size)
  
  batch_id <- seq_len(n_batches)
  row_start <- (batch_id - 1L) * target_batch_size + 1L
  row_end <- pmin(batch_id * target_batch_size, total_rows)
  row_count <- row_end - row_start + 1L
  
  data.table::data.table(
    batch_id = batch_id,
    row_count = row_count,
    row_start = row_start,
    row_end = row_end
  )
}



#' Compute Row-Number Windows for Each Block
#'
#' For each unique block (defined by block_by columns), computes the minimum and
#' maximum row numbers after sorting the table by block columns and ID. These
#' windows define the contiguous row-number range that each block occupies in the
#' sorted table, enabling reliable slicing via row-number filters.
#'
#' @param db_tbl A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)
#' @param block_by Character vector. Column name(s) defining blocks.
#' @param id_col Character. Column name to use for deterministic ordering within blocks.
#'
#' @return A `data.table` with columns from `block_by` plus:
#'   - `.min_rn`: integer, minimum row number for this block (1-based)
#'   - `.max_rn`: integer, maximum row number for this block (1-based)
#'
#' @details
#' The function sorts by block columns first, then by `id_col`, assigns row numbers,
#' and aggregates to find each block's row-number boundaries. This ensures that
#' each block corresponds to a predictable, consecutive range of row numbers.
#'
#' @noRd
.compute_block_row_windows <- function(db_tbl, block_by, id_col) {
  # connection behind the lazy table
  con <- dbplyr::remote_con(db_tbl)
  
  # quoted identifiers
  block_idents     <- DBI::dbQuoteIdentifier(con, block_by)
  block_cols_sql   <- paste(block_idents, collapse = ", ")
  order_vars       <- c(block_by, id_col)
  order_cols_sql   <- paste(DBI::dbQuoteIdentifier(con, order_vars), collapse = ", ")
  
  # inner query from dbplyr
  inner_sql <- dbplyr::sql_render(db_tbl, con = con)
  
  sql_query <- glue::glue("
    SELECT
      {block_cols_sql},
      MIN(rn) AS \"min_rn\",
      MAX(rn) AS \"max_rn\"
    FROM (
      SELECT
        *,
        ROW_NUMBER() OVER (
          ORDER BY {order_cols_sql}
        ) AS rn
      FROM ({inner_sql}) AS x
    ) AS t
    GROUP BY {block_cols_sql}
    ORDER BY {block_cols_sql}
  ")
  
  DBI::dbGetQuery(con, sql_query) |>
    data.table::as.data.table()
}


.get_block_metadata <- function(db_tbl, block_by, id_col) {
  block_stats <- .compute_block_stats(db_tbl, block_by)
  row_windows <- .compute_block_row_windows(db_tbl, block_by, id_col)
  
  block_stats[row_windows, on = block_by]
}


.split_block_into_subbatches <- function(row, block_cols, target_batch_size) {
  size <- row$n
  
  n_sub  <- ceiling(size / target_batch_size)
  sub_sz <- ceiling(size / n_sub)
  
  j         <- seq_len(n_sub)
  start     <- (j - 1L) * sub_sz + 1L
  end       <- pmin(j * sub_sz, size)
  abs_start <- row$min_rn + start - 1L
  abs_end   <- row$min_rn + end   - 1L
  
  data.table::data.table(
    row_count = end - start + 1L,
    row_start = abs_start,
    row_end   = abs_end,
    row[, ..block_cols]
  )
}




#' Create Block-First Batches (One Block Per Batch, Sub-Chunk Large Blocks)
#'
#' Treats each block as a separate batch. Blocks smaller than or equal to
#' `target_batch_size` become single batches. Blocks larger than `target_batch_size`
#' are split into multiple sub-batches of approximately equal size.
#'
#' @param db_tbl A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)
#' @param block_by Character vector. Column name(s) defining blocks.
#' @param target_batch_size Positive integer. Target number of rows per batch.
#' @param id_col Character. Column name to use for deterministic ordering. Default: "id".
#'
#' @return A `data.table` with columns:
#'   - `batch_id`: integer, sequential batch identifier
#'   - `row_count`: integer, number of rows in this batch
#'   - `row_start`: integer, 1-based inclusive start of row window
#'   - `row_end`: integer, 1-based inclusive end of row window
#'   - Additional columns from `block_by`: values identifying the block
#'
#' @details
#' This strategy maximizes block isolation: each small block is processed independently,
#' and large blocks are evenly subdivided. Sub-batches within a large block are sized
#' to minimize variance (e.g., a 250k-row block with target 100k yields 3 batches of
#' ~83k rows each, not 2×100k + 1×50k).
#'
#' All batches have row-number windows, computed after sorting by block columns and ID.
#'
#' @noRd
.chunk_block_first <- function(db_tbl, block_by, target_batch_size, id_col = "id") {
  blocks <- .get_block_metadata(db_tbl, block_by, id_col)
  block_cols <- setdiff(names(blocks), c("n", "min_rn", "max_rn"))
  
  small <- blocks[n <= target_batch_size]
  large <- blocks[n >  target_batch_size]
  
  # small blocks: one batch per block
  small_batches <- NULL
  if (nrow(small)) {
    small_batches <- small[, .(
      row_count  = n,
      row_start  = min_rn,
      row_end    = max_rn,
      block_size = n
    ), by = block_cols]
  }
  
  # large blocks: split into sub-batches
  large_batches <- NULL
  if (nrow(large)) {
    large_batches <- data.table::rbindlist(
      lapply(seq_len(nrow(large)), function(i) {
        row <- large[i]
        subs <- .split_block_into_subbatches(row, block_cols, target_batch_size)
        subs[, block_size := row$n]
        subs
      }),
      fill = TRUE
    )
  }
  
  plan <- data.table::rbindlist(list(small_batches, large_batches), fill = TRUE)
  plan[, batch_id := seq_len(.N)]
  data.table::setcolorder(plan, c("batch_id", "row_count", "row_start", "row_end", block_cols, "block_size"))
  
  plan[]
}





#' Create Block-Consolidated Batches (Minimize Batch Count, Respect Blocks)
#'
#' Consolidates multiple small blocks into single batches to minimize overhead
#' while respecting block boundaries. Small blocks are combined up to
#' `target_batch_size`; large blocks are split into sub-batches.
#'
#' @param db_tbl A DuckDB table reference (result of `dplyr::tbl(con, "table_name")`)
#' @param block_by Character vector. Column name(s) defining blocks.
#' @param target_batch_size Positive integer. Target number of rows per batch.
#' @param id_col Character. Column name to use for deterministic ordering. Default: "id".
#'
#' @return A `data.table` with columns:
#'   - `batch_id`: integer, sequential batch identifier
#'   - `row_count`: integer, number of rows in this batch
#'   - `row_start`: integer, 1-based inclusive start of row window
#'   - `row_end`: integer, 1-based inclusive end of row window
#'   - Additional columns from `block_by`: values identifying the block (for single-block batches only)
#'
#' @details
#' This strategy balances processing efficiency with block semantics. Small blocks
#' (≤ `target_batch_size`) are greedily accumulated into consolidated batches until
#' adding the next block would exceed the target. Large blocks (> `target_batch_size`)
#' trigger a flush of any pending consolidation, then are evenly subdivided.
#'
#' Consolidated batches spanning multiple blocks do not include block column values
#' in the plan (filled with NA), as they represent heterogeneous block groups.
#' Single-block batches (whether small or sub-chunked) retain block identifiers.
#'
#' All batches have row-number windows, computed after sorting by block columns and ID.
#'
#' @noRd
.chunk_block_consolidated <- function(db_tbl, block_by, target_batch_size, id_col = "id") {
  
  # keep full block-first output including block_size
  first <- .chunk_block_first(db_tbl, block_by, target_batch_size, id_col)
  
  block_cols <- setdiff(
    names(first),
    c("batch_id", "row_count", "row_start", "row_end", "block_size")
  )
  
  # create a block list-column for all block-first batches
  # each row gets a list containing its block values
  first[, blocks := lapply(seq_len(.N), function(i) {
    as.list(first[i, block_cols, with = FALSE])
  })]
  

  # separate large and small block batches
  large_batches <- first[block_size >  target_batch_size]
  small_batches <- first[block_size <= target_batch_size]
  
  # consolidation accumulators
  consolidated <- list()
  cur_rows  <- 0L
  cur_start <- NA_integer_
  cur_end   <- NA_integer_
  cur_blocks <- list()
  
  flush <- function() {
    consolidated[[length(consolidated) + 1L]] <<- data.table::data.table(
      row_count = cur_rows,
      row_start = cur_start,
      row_end   = cur_end,
      blocks = list(cur_blocks)
    )
    cur_rows  <<- 0L
    cur_start <<- NA_integer_
    cur_end   <<- NA_integer_
    cur_blocks <<- list()
  }
  
  # greedy consolidation
  for (i in seq_len(nrow(small_batches))) {
    row <- small_batches[i]
    
    if (cur_rows + row$row_count > target_batch_size && cur_rows > 0L) {
      flush()
    }
    
    if (cur_rows == 0L) cur_start <- row$row_start
    cur_end  <- row$row_end
    cur_rows <- cur_rows + row$row_count
    
    # append this block's metadata to list column
    cur_blocks <- c(cur_blocks, list(row$blocks[[1]]))
  }
  
  if (cur_rows > 0L) flush()
  
  consolidated_dt <- if (length(consolidated)) {
    data.table::rbindlist(consolidated, fill = TRUE)
  } else {
    NULL
  }
  
  # large block batches already have: row_count, row_start, row_end, blocks
  large_batches <- large_batches[, .(
    row_count, row_start, row_end, blocks
  )]
  
  # combine large-block and consolidated batches
  result <- data.table::rbindlist(
    list(large_batches, consolidated_dt),
    fill = TRUE
  )
  
  result[, batch_id := seq_len(.N)]
  
  # final ordering without block columns (they do not exist in result)
  data.table::setcolorder(
    result,
    c("batch_id", "row_count", "row_start", "row_end", "blocks")
  )
  
  result[]
}

#' Suggest batch parameters for efficient DuckDB processing
#'
#' Examines hardware limits, row sizes, and block structure to compute
#' sensible defaults for duckdb_batch_plan(). Useful for large datasets.
#'
#' @param con DBI connection to DuckDB
#' @param table Character. Name of the table to inspect
#' @param block_by Optional character vector of block columns
#' @param safety_factor Fraction of RAM to use (0.3 is safe)
#' @param as_tibble Logical. Return a tibble instead of data.table
#'
#' @return A data.table or tibble with recommended parameters:
#'   batch_size, target_batch_size, min_batch_size, avg_row_bytes, avail_ram_bytes, notes
#'
#' @noRd
suggest_batch_params <- function(con,
                                 table,
                                 block_by = NULL,
                                 safety_factor = 0.30,
                                 as_tibble = FALSE) {
  
  # RAM detection ---------------------------------------------------------
  get_total_ram <- function() {
    os <- Sys.info()[["sysname"]]
    
    if (os == "Linux") {
      x <- readLines("/proc/meminfo")
      line <- x[grepl("^MemTotal", x)][1]
      kb <- as.numeric(gsub("[^0-9]", "", line))
      return(kb * 1024)
    }
    
    if (os == "Darwin") {
      bytes <- suppressWarnings(as.numeric(system("sysctl -n hw.memsize", intern = TRUE)))
      if (is.finite(bytes)) return(bytes)
    }
    
    8 * 1024^3
  }
  
  total_ram <- get_total_ram()
  avail_ram <- total_ram * safety_factor
  
  # Row size estimation ----------------------------------------------------
  sample_df <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " LIMIT 10000"))
  avg_row_bytes <- as.numeric(utils::object.size(sample_df)) / nrow(sample_df)
  if (!is.finite(avg_row_bytes) || avg_row_bytes <= 0) avg_row_bytes <- 300
  
  # Compute batch sizes ----------------------------------------------------
  target_rows <- floor(avail_ram / avg_row_bytes)
  target_rows <- max(50e3, min(target_rows, 2e6))
  min_batch_size <- floor(target_rows / 4)
  
  # Adjust for block sizes -------------------------------------------------
  if (!is.null(block_by)) {
    block_cols <- paste(block_by, collapse = ", ")
    largest_block <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ", table,
          " GROUP BY ", block_cols,
          " ORDER BY n DESC LIMIT 1"
        )
      )$n,
      error = function(e) NA
    )
    
    if (is.finite(largest_block) && largest_block > target_rows) {
      target_rows <- max(floor(largest_block / 4), 50e3)
    }
  }
  
  out <- data.table::data.table(
    batch_size = target_rows,
    target_batch_size = target_rows,
    min_batch_size = min_batch_size,
    avg_row_bytes = avg_row_bytes,
    avail_ram_bytes = avail_ram,
    notes = paste0("Using safety factor ", safety_factor)
  )
  
  if (as_tibble) out <- tibble::as_tibble(out)
  out
}





#' Apply a function to DuckDB table batches
#'
#' Streams a DuckDB table through a batch plan and applies a user-defined
#' function to each batch. The function must accept a data.frame and return
#' a data.frame. Results can be collected in memory or written back to
#' DuckDB incrementally.
#'
#' Database work is performed batch-by-batch, allowing preprocessing of
#' tables that exceed available RAM. For each batch, a SQL slice or block
#' filter is executed, the function is applied, and (optionally) results
#' are appended to a DuckDB table.
#'
#' @param plan A batch plan produced by `duckdb_batch_plan()`. Must include
#'   columns `batch_id` and `row_count`, plus either row-number windows
#'   (`row_start`, `row_end`), block identifier columns, or a `blocks`
#'   list-column for consolidated batches.
#' @param con A DuckDB connection.
#' @param input_table Character. Name of the source table in DuckDB.
#' @param fn A function applied to each batch. Receives a data.frame and
#'   must return a data.frame.
#' @param persist Logical. If `TRUE`, results of each batch are appended
#'   to `output_table` inside DuckDB and a lazy table reference is returned.
#'   If `FALSE`, returns a list of batch results as data.frames.
#' @param output_table Optional DuckDB table name where results are stored
#'   when `persist = TRUE`. If omitted, a random temporary table name is
#'   generated. Ignored when `persist = FALSE`.
#'
#' @return
#' - If `persist = TRUE`: A `tbl_duckdb_connection` pointing to the output table.  
#' - If `persist = FALSE`: A list of data.frames, one per batch.
#'
#' @export
batch_map <- function(plan,
                      con,
                      input_table,
                      fn,
                      persist = TRUE,
                      output_table = NULL) {
  
  stopifnot(is.data.frame(plan))
  stopifnot("batch_id"  %in% names(plan))
  stopifnot("row_count" %in% names(plan))
  
  # Choose output table if persisting
  if (persist) {
    if (is.null(output_table)) {
      output_table <- paste0("__duckdb_batch_", sample.int(1e9, 1))
    }
    first <- TRUE
  }
  
  n_batches <- nrow(plan)
  results   <- vector("list", n_batches)
  
  # ----------------------------------------------------------------------
  # Iterate over batches
  # ----------------------------------------------------------------------
  for (i in seq_len(n_batches)) {
    row <- plan[i]
    
    cli::cli_inform(
      "Processing batch {i} of {n_batches} ({row$row_count} rows)",
      .auto_close = TRUE
    )
    cat("\n")
    # ------------------------------------------------------------------
    # Determine slicing mode
    # ------------------------------------------------------------------
    has_windows <- !is.na(row$row_start) && !is.na(row$row_end)
    has_blocks  <- "blocks" %in% names(row)
    block_cols  <- setdiff(
      names(row),
      c("batch_id", "row_count", "row_start", "row_end", "blocks")
    )
    
    # ------------------------------------------------------------------
    # Build SQL for this batch
    # ------------------------------------------------------------------
    if (has_windows) {
      
      sql <- paste0(
        "SELECT * FROM (",
        "  SELECT ROW_NUMBER() OVER() AS rn, * FROM ", input_table,
        ") WHERE rn BETWEEN ", row$row_start, " AND ", row$row_end
      )
      
    } else if (length(block_cols) > 0) {
      
      conds <- vapply(block_cols, function(col) {
        val <- row[[col]]
        if (is.na(val)) {
          paste0(col, " IS NULL")
        } else {
          paste0(col, " = ", DBI::dbQuoteLiteral(con, val))
        }
      }, character(1))
      
      sql <- paste0(
        "SELECT * FROM ", input_table,
        " WHERE ", paste(conds, collapse = " AND ")
      )
      
    } else if (has_blocks) {
      
      sql <- paste0(
        "SELECT * FROM (",
        "  SELECT ROW_NUMBER() OVER() AS rn, * FROM ", input_table,
        ") WHERE rn BETWEEN ", row$row_start, " AND ", row$row_end
      )
      
    } else {
      cli::cli_abort("Batch plan row contains neither windows nor blocks")
    }
    
    # ------------------------------------------------------------------
    # Fetch and process batch
    # ------------------------------------------------------------------
    df <- DBI::dbGetQuery(con, sql)
    if ("rn" %in% names(df)) df$rn <- NULL
    
    out <- fn(df)
    
    # ------------------------------------------------------------------
    # Persist results or store in memory
    # ------------------------------------------------------------------
    if (persist) {
      DBI::dbWriteTable(
        con,
        output_table,
        as.data.frame(out),
        overwrite = first,
        append    = !first
      )
      first <- FALSE
    } else {
      results[[i]] <- out
    }
  }
  
  # ----------------------------------------------------------------------
  # Return result
  # ----------------------------------------------------------------------
  if (persist) {
    dplyr::tbl(con, output_table)
  } else {
    results
  }
}




