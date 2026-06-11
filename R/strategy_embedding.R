# ============================================================
# Embedding Strategy Class for joinery
# ============================================================
#
# Defines the Embedding_Strategy S7 class for semantic matching via embeddings.
# This is a separate strategy type from Search_Strategy (token-based matching).
#
# Embedding strategies:
# - Represent entire records as embeddings
# - Use cosine similarity for scoring
# - Do not support token preparers, weights, blocking, or rarity
# - Require tidyllm for embedding computation
#
# ============================================================


# ---------------------------------------------------------------------------
# Embedding_Strategy class
# ---------------------------------------------------------------------------

#' Embedding Strategy Class
#'
#' @description
#' An S7 class representing a semantic matching strategy using embeddings.
#' This is a distinct strategy type from `Search_Strategy` (token-based).
#'
#' Embedding strategies compute one embedding vector per record and use
#' cosine similarity for matching. They are designed for use in multi-stage
#' workflows via `multi_stage_search()`.
#'
#' @slot columns Character vector. If empty (character(0)),
#'   all non-id character columns are used.
#' @slot embedding_model A tidyllm provider object that specifies both the
#'   provider and model (e.g., `ollama(.model = "mxbai-embed-large")`).
#'   This is passed directly to tidyllm's `embed()` function.
#' @slot threshold Numeric scalar in [0, 1] for cosine similarity filtering.
#' @slot collapse_sep Character scalar used to join multiple columns into
#'   a single text string per record. Default is " ".
#' @slot normalize Logical. If TRUE (default), apply L2 normalization to
#'   embeddings before computing cosine similarity.
#' @slot batch_size Numeric scalar for embedding computation batch size.
#' @slot block_by NULL or a character vector of blocking variables. When
#'   specified, comparisons are only made within matching blocks.
#'
#' @seealso [embedding_strategy()]
#'
#' @noRd
Embedding_Strategy <- new_class(
  "Embedding_Strategy",
  properties = list(
    columns          = class_character,
    embedding_model  = class_any,
    threshold        = class_numeric,
    collapse_sep     = class_character,
    normalize        = class_logical,
    batch_size       = class_numeric,
    block_by         = class_any
  ),
  validator = function(self) {
    # Threshold validation
    if (length(self@threshold) != 1) {
      return("threshold must be a scalar")
    }
    if (!is.finite(self@threshold)) {
      return("threshold must be finite")
    }
    if (self@threshold < 0 || self@threshold > 1) {
      return("threshold must be in [0, 1]")
    }
    # collapse_sep validation
    if (length(self@collapse_sep) != 1) {
      return("collapse_sep must be a scalar")
    }
    
    # normalize validation
    if (length(self@normalize) != 1) {
      return("normalize must be a scalar logical")
    }
    
    # batch_size validation
    if (length(self@batch_size) != 1) {
      return("batch_size must be a scalar")
    }
    if (!is.finite(self@batch_size) || self@batch_size <= 0) {
      return("batch_size must be a positive finite number")
    }
  }
)


# ---------------------------------------------------------------------------
# Print method for Embedding_Strategy
# ---------------------------------------------------------------------------

#' @noRd
method(print.Search_Preparer, Embedding_Strategy) <- function(x, ...) {
  cli::cli_text("{.strong <joinery::Embedding_Strategy>}")

  cli::cli_text()
  cli::cli_text("{.strong columns}")
  if (length(x@columns) == 0) {
    cli::cli_text("all")
  } else {
    cli::cli_text("{paste(x@columns, collapse = ', ')}")
  }

  cli::cli_text()
  cli::cli_text("model: {deparse(x@embedding_model, nlines = 1)}")
  cli::cli_text("threshold: {format(x@threshold)}")
  cli::cli_text("collapse_sep: {.val {x@collapse_sep}}")
  cli::cli_text("normalize: {x@normalize}")
  cli::cli_text("batch_size: {format(x@batch_size)}")

  if (is.null(x@block_by)) {
    cli::cli_text("blocking: none")
  } else {
    cli::cli_text("blocking: {paste(x@block_by, collapse = ', ')}")
  }

  invisible(x)
}


# ---------------------------------------------------------------------------
# Constructor: embedding_strategy()
# ---------------------------------------------------------------------------

#' Create an Embedding Strategy
#'
#' @description
#' Construct an `Embedding_Strategy` object for semantic matching using
#' embeddings. This is a distinct strategy type from token-based strategies
#' created with `search_strategy()`.
#'
#' Embedding strategies:
#' - Represent entire records as embedding vectors
#' - Use cosine similarity for scoring
#' - Support blocking variables to restrict comparisons
#' - Require the tidyllm package for embedding computation
#'
#' @param columns Character vector of column names to embed, or NULL (default)
#'   to use all non-id character-like columns.
#' @param embedding_model A tidyllm provider object (e.g., 
#'   `ollama(.model = "mxbai-embed-large")`). This is passed directly to
#'   tidyllm's `embed()` function.
#' @param threshold Numeric scalar in [0, 1]. Cosine similarity threshold for
#'   filtering matches.
#' @param collapse_sep Character scalar. Separator used when joining multiple
#'   columns into a single text string. Default is " ".
#' @param normalize Logical scalar. If TRUE (default), apply L2 normalization
#'   to embeddings before computing cosine similarity.
#' @param batch_size Numeric scalar. Number of records to process per batch
#'   when computing embeddings. Default is 1000.
#' @param block_by Character vector of blocking variable names, or NULL (default).
#'   When specified, comparisons are only made within matching blocks.
#'
#' @return An `Embedding_Strategy` S7 object.
#'
#' @examples
#' \dontrun{
#' library(tidyllm)
#' 
#' # Create an embedding strategy using Ollama
#' emb_strat <- embedding_strategy(
#'   columns = c("name", "address"),
#'   embedding_model = ollama(.model = "mxbai-embed-large"),
#'   threshold = 0.85
#' )
#' 
#' # Use in multi-stage workflow
#' results <- multi_stage_search(
#'   base_table = customers_a,
#'   target_table = customers_b,
#'   base_id = "id_a",
#'   target_id = "id_b",
#'   strategies = list(
#'     token_stage = search_strategy(name ~ normalize_text() + word_tokens()),
#'     semantic_stage = emb_strat
#'   )
#' )
#' }
#'
#' @export
embedding_strategy <- function(columns = NULL,
                               embedding_model,
                               threshold,
                               collapse_sep = " ",
                               normalize = TRUE,
                               batch_size = 1000,
                               block_by = NULL) {

  # Check tidyllm availability
  if (!requireNamespace("tidyllm", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn embedding_strategy} requires the {.pkg tidyllm} package",
      "i" = "Install it via {.run install.packages(\"tidyllm\")}"
    ))
  }
  
  # Validate inputs
  if (missing(embedding_model)) {
    cli::cli_abort("{.arg embedding_model} is required")
  }

  if (missing(threshold)) {
    cli::cli_abort("{.arg threshold} is required")
  }
  
  if (is.null(columns)) {
    columns <- character(0)
  }
  
  # Construct the strategy
  Embedding_Strategy(
    columns = columns,
    embedding_model = embedding_model,
    threshold = threshold,
    collapse_sep = collapse_sep,
    normalize = normalize,
    batch_size = batch_size,
    block_by = block_by
  )
}


# ---------------------------------------------------------------------------
# Helper: assemble_record_text()
# ---------------------------------------------------------------------------

#' Assemble Record Text for Embedding
#'
#' @description
#' Internal helper that concatenates multiple columns into a single text
#' string per record for embedding computation.
#'
#' @param data A data frame, tibble, or data.table.
#' @param id Character scalar naming the ID column.
#' @param columns Character vector of columns to concatenate, or character(0) to
#'   use all non-id character-like columns.
#' @param sep Character scalar used to join column values.
#'
#' @return A data frame with columns: `id` and `text`.
#'
#' @noRd
assemble_record_text <- function(data, id, columns = character(0), sep = " ") {
  
  # Determine columns to use
  if (length(columns) == 0) {
    # Use all non-id character-like columns
    is_char_like <- vapply(data, function(x) {
      is.character(x) || is.factor(x)
    }, logical(1))
    
    columns <- names(data)[is_char_like & names(data) != id]
    
    if (length(columns) == 0) {
      cli::cli_abort(c(
        "No character-like columns found for embedding",
        "i" = "Specify {.arg columns} explicitly or ensure data contains text columns"
      ))
    }
  } else {
    # Validate specified columns exist
    missing_cols <- setdiff(columns, names(data))
    if (length(missing_cols) > 0) {
      cli::cli_abort("Columns not found in data: {.field {missing_cols}}")
    }
  }
  
  # Extract ID and text columns
  id_vals <- data[[id]]
  
  # Coerce each column to character and collect
  text_parts <- lapply(columns, function(col) {
    as.character(data[[col]])
  })
  
  # For each record, drop NAs and collapse
  text_vals <- vapply(seq_along(id_vals), function(i) {
    parts <- vapply(text_parts, `[`, character(1), i)
    parts <- parts[!is.na(parts)]
    paste(parts, collapse = sep)
  }, character(1))
  
  # Return as data frame
  data.frame(
    id = id_vals,
    text = text_vals,
    stringsAsFactors = FALSE
  )
}
