# ============================================================
# summarise_matches() — all backends
# ============================================================
#
# Implements the post-match overview verb end-to-end. Auto-detects
# whether the input is a duplicate table (presence of `duplicate_group`)
# or a candidate table (presence of `match_id` + `source`), and computes:
#
#   * n_pairs_or_groups, n_records_involved
#   * coverage (when base / target supplied)
#   * score distribution: summary, fixed quantiles, histogram bins
#   * cluster_dist + cluster_summary  (duplicates only)
#   * ambiguity_dist + top_gap_dist   (candidates only)
#   * recommendations (catalog dispatch)
#
# Backends: data.table (reference), DuckDB, tibble/data.frame thin wrappers.
# ============================================================


# ---------------------------------------------------------------------------
# Internal: validate and detect match_type
# ---------------------------------------------------------------------------

#' @noRd
.detect_match_type_cols <- function(cols) {
  if ("duplicate_group" %in% cols && "id" %in% cols && "score" %in% cols) {
    return("duplicates")
  }
  if (all(c("match_id", "source", "id", "score") %in% cols)) {
    return("candidates")
  }
  stop(
    "`matches` does not look like a joinery match table.\n",
    "Expected either:\n",
    "  - duplicate columns: duplicate_group, id, score (from `detect_duplicates`)\n",
    "  - candidate columns: match_id, source, id, score (from `search_candidates`)",
    call. = FALSE
  )
}

#' @noRd
.detect_match_type <- function(matches) {
  .detect_match_type_cols(names(matches))
}


# ---------------------------------------------------------------------------
# Internal: score distribution
# ---------------------------------------------------------------------------

#' @noRd
.score_distribution <- function(scores, bins = 50) {
  if (!is.numeric(scores)) {
    stop("`score` column must be numeric.", call. = FALSE)
  }
  scores <- scores[!is.na(scores)]

  if (length(scores) == 0L) {
    return(list(
      summary   = c(min = NA_real_, q1 = NA_real_, median = NA_real_,
                    mean = NA_real_, q3 = NA_real_, max = NA_real_),
      quantiles = stats::setNames(
        rep(NA_real_, 7),
        c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
      ),
      histogram = data.table::data.table(
        bin_lower = numeric(),
        bin_upper = numeric(),
        count     = integer()
      ),
      threshold = NA_real_
    ))
  }

  q <- stats::quantile(
    scores,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    names = FALSE,
    na.rm = TRUE
  )
  summary_vec <- c(
    min    = q[[1]],
    q1     = q[[2]],
    median = q[[3]],
    mean   = mean(scores),
    q3     = q[[4]],
    max    = q[[5]]
  )

  qq <- stats::quantile(
    scores,
    probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    names = FALSE,
    na.rm = TRUE
  )
  quantiles_vec <- stats::setNames(
    qq,
    c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
  )

  if (length(unique(scores)) == 1L) {
    only <- unique(scores)
    hist_dt <- data.table::data.table(
      bin_lower = only,
      bin_upper = only,
      count     = length(scores)
    )
  } else {
    breaks <- seq(min(scores), max(scores), length.out = bins + 1L)
    h <- graphics::hist(scores, breaks = breaks, plot = FALSE)
    hist_dt <- data.table::data.table(
      bin_lower = h$breaks[-length(h$breaks)],
      bin_upper = h$breaks[-1L],
      count     = as.integer(h$counts)
    )
  }

  list(
    summary   = summary_vec,
    quantiles = quantiles_vec,
    histogram = hist_dt,
    threshold = NA_real_   # not propagated into match tables
  )
}


# ---------------------------------------------------------------------------
# Method: summarise_matches on data.table
# ---------------------------------------------------------------------------

method(
  summarise_matches,
  DT_tbl
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {

  match_type <- .detect_match_type(matches)
  dt <- data.table::as.data.table(matches)

  # --- score distribution ------------------------------------------------
  score_dist <- .score_distribution(dt$score, bins = bins)

  # --- per-branch ---------------------------------------------------------
  cluster_dist    <- NULL
  cluster_summary <- NULL
  ambiguity_dist  <- NULL
  top_gap_dist    <- NULL
  signals         <- list()

  if (match_type == "duplicates") {

    n_groups   <- data.table::uniqueN(dt$duplicate_group)
    n_records  <- data.table::uniqueN(dt$id)

    # cluster size distribution
    sizes <- dt[, .(cluster_size = .N), by = "duplicate_group"]
    cluster_dist <- sizes[, .(n_clusters = .N), by = "cluster_size"]
    data.table::setorder(cluster_dist, cluster_size)

    max_cluster_size <- if (nrow(sizes) > 0L) max(sizes$cluster_size) else 0L

    pct_in_cluster <- NA_real_
    if (!is.null(base)) {
      if (nrow(base) > 0L) pct_in_cluster <- n_records / nrow(base)
    }

    cluster_summary <- list(
      max_cluster_size       = as.integer(max_cluster_size),
      pct_records_in_cluster = pct_in_cluster
    )

    base_coverage   <- pct_in_cluster
    target_coverage <- NA_real_

    signals[["max_cluster_size"]] <- as.numeric(max_cluster_size)

  } else { # candidates

    n_pairs    <- data.table::uniqueN(dt$match_id)
    n_records  <- data.table::uniqueN(dt$id)

    base_rows   <- dt[source == "base"]
    target_rows <- dt[source == "target"]

    base_ids   <- unique(base_rows$id)
    target_ids <- unique(target_rows$id)

    base_coverage <- if (!is.null(base) && nrow(base) > 0L) {
      length(base_ids) / nrow(base)
    } else NA_real_
    target_coverage <- if (!is.null(target) && nrow(target) > 0L) {
      length(target_ids) / nrow(target)
    } else NA_real_

    # ambiguity: candidates per base record
    if (nrow(base_rows) > 0L) {
      n_cands_per_base <- base_rows[, .(
        n_candidates = data.table::uniqueN(match_id)
      ), by = "id"]
      ambiguity_dist <- n_cands_per_base[, .(n_records = .N),
                                         by = "n_candidates"]
      data.table::setnames(ambiguity_dist,
                           old = "n_candidates",
                           new = "candidates_per_record")
      data.table::setorder(ambiguity_dist, candidates_per_record)

      pct_ge3 <- mean(n_cands_per_base$n_candidates >= 3L)
      signals[["pct_records_with_ge3_matches"]] <- pct_ge3
    }

    # top_gap: per base record with >= 2 candidates, top1 - top2
    if (nrow(base_rows) > 0L) {
      ordered <- data.table::copy(base_rows)
      data.table::setorder(ordered, id, -score)
      gaps <- ordered[, {
        if (.N >= 2L) list(gap = score[1L] - score[2L])
        else list(gap = NA_real_)
      }, by = "id"]
      gaps_clean <- gaps$gap[!is.na(gaps$gap)]
      if (length(gaps_clean) > 0L) {
        if (length(unique(gaps_clean)) == 1L) {
          top_gap_dist <- data.table::data.table(
            bin_lower = unique(gaps_clean),
            bin_upper = unique(gaps_clean),
            count     = length(gaps_clean)
          )
        } else {
          breaks <- seq(min(gaps_clean), max(gaps_clean), length.out = 21L)
          h <- graphics::hist(gaps_clean, breaks = breaks, plot = FALSE)
          top_gap_dist <- data.table::data.table(
            bin_lower = h$breaks[-length(h$breaks)],
            bin_upper = h$breaks[-1L],
            count     = as.integer(h$counts)
          )
        }
        signals[["score_top_gap_median"]] <- stats::median(gaps_clean)
      }
    }

    if (!is.na(base_coverage)) {
      signals[["base_coverage_candidates"]] <- base_coverage
    }
  }

  # --- assemble n_records & coverage -------------------------------------
  n_records_list <- if (match_type == "duplicates") {
    list(
      n_pairs_or_groups  = as.integer(n_groups),
      n_records_involved = as.integer(n_records)
    )
  } else {
    list(
      n_pairs_or_groups  = as.integer(n_pairs),
      n_records_involved = as.integer(n_records)
    )
  }

  coverage_list <- list(
    base_coverage   = base_coverage,
    target_coverage = target_coverage
  )

  # --- recommendations ----------------------------------------------------
  recs <- .dispatch_recommendations(signals)

  out <- Match_Overview(
    match_type      = match_type,
    n_records       = n_records_list,
    coverage        = coverage_list,
    score_dist      = score_dist,
    cluster_dist    = cluster_dist,
    cluster_summary = cluster_summary,
    ambiguity_dist  = ambiguity_dist,
    top_gap_dist    = top_gap_dist,
    recommendations = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  out
}


# ---------------------------------------------------------------------------
# Method: summarise_matches on DuckDB
# ---------------------------------------------------------------------------

method(
  summarise_matches,
  Duck_tbl
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {

  con      <- matches$src$con
  tbl_name <- matches$lazy_query$x

  # --- detect match type ---------------------------------------------------
  col_rows   <- DBI::dbGetQuery(
    con,
    paste0("SELECT column_name FROM information_schema.columns ",
           "WHERE table_name = '", tbl_name, "'")
  )
  match_type <- .detect_match_type_cols(col_rows$column_name)

  # --- score distribution --------------------------------------------------
  score_dist <- .duckdb_score_distribution(con, tbl_name, bins = bins)

  # --- per-branch ----------------------------------------------------------
  cluster_dist    <- NULL
  cluster_summary <- NULL
  ambiguity_dist  <- NULL
  top_gap_dist    <- NULL
  signals         <- list()

  if (match_type == "duplicates") {

    sizes <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) AS cluster_size FROM \"", tbl_name,
             "\" GROUP BY duplicate_group")
    )
    n_groups  <- nrow(sizes)
    n_records <- sum(sizes$cluster_size)

    if (nrow(sizes) == 0L) {
      cluster_dist <- data.table::data.table(
        cluster_size = integer(),
        n_clusters   = integer()
      )
    } else {
      cluster_size_freq <- as.data.table(
        as.data.frame(table(sizes$cluster_size))
      )
      data.table::setnames(cluster_size_freq, c("cluster_size", "n_clusters"))
      cluster_size_freq[, cluster_size := as.integer(as.character(cluster_size))]
      cluster_size_freq[, n_clusters   := as.integer(n_clusters)]
      data.table::setorder(cluster_size_freq, cluster_size)
      cluster_dist <- cluster_size_freq
    }

    max_cluster_size <- if (nrow(sizes) > 0L) max(sizes$cluster_size) else 0L

    pct_in_cluster <- NA_real_
    if (!is.null(base)) {
      base_n <- .backend_nrow(base)
      if (base_n > 0L) pct_in_cluster <- n_records / base_n
    }

    cluster_summary <- list(
      max_cluster_size       = as.integer(max_cluster_size),
      pct_records_in_cluster = pct_in_cluster
    )

    base_coverage   <- pct_in_cluster
    target_coverage <- NA_real_

    signals[["max_cluster_size"]] <- as.numeric(max_cluster_size)

  } else { # candidates

    counts <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(DISTINCT match_id) AS n_pairs, ",
             "COUNT(DISTINCT id) AS n_records FROM \"", tbl_name, "\"")
    )
    n_pairs   <- counts$n_pairs
    n_records <- counts$n_records

    base_ids_n <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(DISTINCT id) AS n FROM \"", tbl_name,
             "\" WHERE source = 'base'")
    )$n

    target_ids_n <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(DISTINCT id) AS n FROM \"", tbl_name,
             "\" WHERE source = 'target'")
    )$n

    base_coverage <- if (!is.null(base)) {
      base_n <- .backend_nrow(base)
      if (base_n > 0L) base_ids_n / base_n else NA_real_
    } else NA_real_

    target_coverage <- if (!is.null(target)) {
      target_n <- .backend_nrow(target)
      if (target_n > 0L) target_ids_n / target_n else NA_real_
    } else NA_real_

    # ambiguity distribution
    amb <- DBI::dbGetQuery(
      con,
      paste0("SELECT candidates_per_record, COUNT(*) AS n_records FROM (",
             "SELECT id, COUNT(DISTINCT match_id) AS candidates_per_record ",
             "FROM \"", tbl_name, "\" WHERE source = 'base' GROUP BY id",
             ") GROUP BY candidates_per_record ORDER BY candidates_per_record")
    )
    if (nrow(amb) > 0L) {
      ambiguity_dist <- data.table::as.data.table(amb)
      ambiguity_dist[, candidates_per_record := as.integer(candidates_per_record)]
      ambiguity_dist[, n_records             := as.integer(n_records)]
      pct_ge3 <- DBI::dbGetQuery(
        con,
        paste0("SELECT AVG(CAST(n_cands >= 3 AS DOUBLE)) AS pct FROM (",
               "SELECT id, COUNT(DISTINCT match_id) AS n_cands ",
               "FROM \"", tbl_name, "\" WHERE source = 'base' GROUP BY id)")
      )$pct
      signals[["pct_records_with_ge3_matches"]] <- pct_ge3
    }

    # top-gap distribution
    gaps <- DBI::dbGetQuery(
      con,
      paste0(
        "WITH ranked AS (",
        "  SELECT id, score, ROW_NUMBER() OVER (PARTITION BY id ORDER BY score DESC) AS rn",
        "  FROM \"", tbl_name, "\" WHERE source = 'base'",
        ") ",
        "SELECT id,",
        "  MAX(CASE WHEN rn = 1 THEN score END) -",
        "  MAX(CASE WHEN rn = 2 THEN score END) AS gap",
        " FROM ranked GROUP BY id HAVING COUNT(*) >= 2"
      )
    )
    if (nrow(gaps) > 0L) {
      gaps_clean <- gaps$gap[!is.na(gaps$gap)]
      if (length(gaps_clean) > 0L) {
        top_gap_dist <- .histogram_dt(gaps_clean, bins = 20L)
        signals[["score_top_gap_median"]] <- stats::median(gaps_clean)
      }
    }

    if (!is.na(base_coverage)) {
      signals[["base_coverage_candidates"]] <- base_coverage
    }
  }

  # --- assemble n_records & coverage ---------------------------------------
  n_records_list <- list(
    n_pairs_or_groups  = as.integer(if (match_type == "duplicates") n_groups else n_pairs),
    n_records_involved = as.integer(n_records)
  )

  coverage_list <- list(
    base_coverage   = base_coverage,
    target_coverage = target_coverage
  )

  # --- recommendations -----------------------------------------------------
  recs <- .dispatch_recommendations(signals)

  out <- Match_Overview(
    match_type      = match_type,
    n_records       = n_records_list,
    coverage        = coverage_list,
    score_dist      = score_dist,
    cluster_dist    = cluster_dist,
    cluster_summary = cluster_summary,
    ambiguity_dist  = ambiguity_dist,
    top_gap_dist    = top_gap_dist,
    recommendations = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  out
}


# ---------------------------------------------------------------------------
# Internal helpers shared by DuckDB method
# ---------------------------------------------------------------------------

#' @noRd
.backend_nrow <- function(x) {
  if (inherits(x, "tbl_duckdb_connection")) {
    as.integer(DBI::dbGetQuery(
      x$src$con,
      paste0("SELECT COUNT(*) AS n FROM \"", x$lazy_query$x, "\"")
    )$n)
  } else {
    nrow(x)
  }
}

#' @noRd
.histogram_dt <- function(values, bins = 50L) {
  if (length(unique(values)) == 1L) {
    return(data.table::data.table(
      bin_lower = unique(values),
      bin_upper = unique(values),
      count     = length(values)
    ))
  }
  breaks <- seq(min(values), max(values), length.out = bins + 1L)
  h <- graphics::hist(values, breaks = breaks, plot = FALSE)
  data.table::data.table(
    bin_lower = h$breaks[-length(h$breaks)],
    bin_upper = h$breaks[-1L],
    count     = as.integer(h$counts)
  )
}

#' @noRd
.duckdb_score_distribution <- function(con, tbl_name, bins = 50L) {
  row_count <- DBI::dbGetQuery(
    con, paste0("SELECT COUNT(*) AS n FROM \"", tbl_name, "\"")
  )$n

  if (row_count == 0L) {
    return(list(
      summary   = c(min = NA_real_, q1 = NA_real_, median = NA_real_,
                    mean = NA_real_, q3 = NA_real_, max = NA_real_),
      quantiles = stats::setNames(
        rep(NA_real_, 7),
        c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
      ),
      histogram = data.table::data.table(
        bin_lower = numeric(), bin_upper = numeric(), count = integer()
      ),
      threshold = NA_real_
    ))
  }

  # single-pass summary + approx quantiles (separate calls — portable across DuckDB versions)
  summ <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT MIN(score) AS min_s, MAX(score) AS max_s, AVG(score) AS mean_s,",
      "  APPROX_QUANTILE(score, 0.25) AS q1,",
      "  APPROX_QUANTILE(score, 0.5)  AS median_s,",
      "  APPROX_QUANTILE(score, 0.75) AS q3,",
      "  APPROX_QUANTILE(score, 0.05) AS p05,",
      "  APPROX_QUANTILE(score, 0.10) AS p10,",
      "  APPROX_QUANTILE(score, 0.90) AS p90,",
      "  APPROX_QUANTILE(score, 0.95) AS p95",
      " FROM \"", tbl_name, "\""
    )
  )

  summary_vec <- c(
    min    = summ$min_s,
    q1     = summ$q1,
    median = summ$median_s,
    mean   = summ$mean_s,
    q3     = summ$q3,
    max    = summ$max_s
  )

  quantiles_vec <- stats::setNames(
    c(summ$p05, summ$p10, summ$q1, summ$median_s,
      summ$q3, summ$p90, summ$p95),
    c("p05", "p10", "p25", "p50", "p75", "p90", "p95")
  )

  # histogram via FLOOR arithmetic — portable across DuckDB versions
  min_s <- summ$min_s
  max_s <- summ$max_s

  hist_dt <- if (min_s == max_s) {
    data.table::data.table(
      bin_lower = min_s,
      bin_upper = max_s,
      count     = as.integer(row_count)
    )
  } else {
    range_s <- max_s - min_s
    hist_r  <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT CAST(FLOOR((score - ", min_s, ") / ", range_s, " * ", bins, ") AS INTEGER) AS bin,",
        " COUNT(*) AS cnt",
        " FROM \"", tbl_name, "\"",
        " GROUP BY bin ORDER BY bin"
      )
    )
    # FLOOR can yield bin == bins for score == max_s; clamp to bins-1 then
    # re-aggregate so the overflow count is added to (not overwriting) bin bins-1.
    hist_r$bin[hist_r$bin >= bins] <- bins - 1L
    hist_r <- stats::aggregate(cnt ~ bin, data = hist_r, FUN = sum)
    breaks <- seq(min_s, max_s, length.out = bins + 1L)
    counts <- integer(bins)
    counts[hist_r$bin + 1L] <- as.integer(hist_r$cnt)
    data.table::data.table(
      bin_lower = breaks[-length(breaks)],
      bin_upper = breaks[-1L],
      count     = counts
    )
  }

  list(
    summary   = summary_vec,
    quantiles = quantiles_vec,
    histogram = hist_dt,
    threshold = NA_real_
  )
}


# ---------------------------------------------------------------------------
# Methods: summarise_matches on tibble / data.frame
# ---------------------------------------------------------------------------

method(
  summarise_matches,
  .jyDF
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  summarise_matches(as_DT(matches), base = base_dt, target = target_dt,
                    bins = bins, ...)
}

method(
  summarise_matches,
  .jyTBL_DF
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  summarise_matches(as_DT(matches), base = base_dt, target = target_dt,
                    bins = bins, ...)
}

method(
  summarise_matches,
  .jyTBL
) <- function(matches, base = NULL, target = NULL, bins = 50L, ...) {
  base_dt   <- if (!is.null(base))   as_DT(base)   else NULL
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  summarise_matches(as_DT(matches), base = base_dt, target = target_dt,
                    bins = bins, ...)
}
