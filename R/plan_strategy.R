# ============================================================
# plan_strategy() — all backends
# ============================================================
#
# Pre-match, pre-STRATEGY diagnostic (v0.8 Stage 08, plan item A7). Where
# audit_strategy() grades a *given* strategy and rarity_distribution() reads one
# column distribution, plan_strategy() helps you FIND the strategy: it surveys a
# set of candidate blockings and surfaces the cost/recall knee *before* the run.
#
# It is deliberately SCORING-FREE — no token-overlap join, no .score_token_pairs
# / .score_pairs_sql. Every probe is O(rows) or O(blocks): GROUP BY + arithmetic,
# never a materialized pair set. The four reads:
#
#   1. Blocking-resolution frontier   — per candidate block key: #blocks, size
#        distribution, Sum(na*nb) brute-pair COUNT (the cost axis, arithmetic),
#        and the share of exact-token-set twins that stay co-blocked (recall
#        axis). The headline: shows the knee before paying for it.
#   2. Exact-set persister rate       — how much a Stage-0 exact_strategy() front
#        would absorb (A2 yield), so the user knows whether to stage at all.
#   3. Residual structure             — matchable (both sides in block) vs
#        one-sided, plus the per-column partial-recoverable shares (which
#        attribute carries identity).
#   4. Per-column discriminativeness  — df/rarity distribution + offender list
#        (reuses Stage 04's core) + a min_rarity -> intermediate-size cost curve,
#        the empty-column score-ceiling warning (§25), and an OPT-IN containment
#        share (§22; the one read that does a bounded structural join).
#
# The twin-survival read uses A2's FAITHFUL fingerprint (.exact_fp_wide_dt via
# the exact proxy strategy) — never a re-rolled one — so it cannot diverge from
# the engine's score==1.0.
#
# Backends: data.table (reference), DuckDB (collect/sample then delegate, like
#   audit_strategy / rarity_distribution — so no pairs ever touch the DB), and
#   tibble/data.frame (as_DT wrappers).
# ============================================================


#' Strategy Plan Result
#'
#' @description
#' Result of [plan_strategy()]. Carries the four pre-match, pre-strategy reads:
#' the blocking-resolution frontier, the exact-set persister rate, the residual
#' structure, and the per-column discriminativeness / `min_rarity` cost curve -
#' all computed scoring-free.
#'
#' @slot frontier `data.table`. One row per candidate block key:
#'   `block_key`, `n_blocks`, `max_block`, `mean_block`, `brute_pairs`
#'   (Sum(na*nb), the cost axis), `exact_twin_survival` (recall axis).
#' @slot persister_rate Named list: `overall` (scalar) + `per_column` (named
#'   numeric) - the share of records an exact front stage would link.
#' @slot residual_structure Named list: `matchable`, `one_sided`,
#'   `partial_recoverable` (named per-column numeric), and `reference_block`.
#' @slot column_reads Named list: `distribution`, `offenders` (Stage-04 core),
#'   `min_rarity_curve` (`data.table` `min_rarity` / `intermediate_pairs`),
#'   `empty_column` (`data.table` `column` / `empty_rate` / `score_ceiling`),
#'   and `containment_share` (scalar or `NA` when not requested).
#' @slot mode Character. `"dedup"` or `"search"`.
#' @slot recommendations Character. Strings from the recommendations catalog.
#'
#' @noRd
Strategy_Plan <- new_class(
  "Strategy_Plan",
  properties = list(
    frontier           = class_any,
    persister_rate     = class_list,
    residual_structure = class_list,
    column_reads       = class_list,
    mode               = class_character,
    recommendations    = class_character
  )
)


#' @noRd
print.Strategy_Plan  <- new_external_generic("base", "print", "x")
#' @noRd
format.Strategy_Plan <- new_external_generic("base", "format", "x")
#' @noRd
as.data.table.Strategy_Plan <- new_external_generic(
  "data.table", "as.data.table", "x"
)
#' @noRd
as.data.frame.Strategy_Plan <- new_external_generic(
  "base", "as.data.frame", "x"
)


# ---------------------------------------------------------------------------
# Internal helpers (all scoring-free)
# ---------------------------------------------------------------------------

# A stable block-key string from one or more columns of a data.table.
#' @noRd
.plan_block_key <- function(dt, cols) {
  if (length(cols) == 1L) return(as.character(dt[[cols]]))
  parts <- lapply(cols, function(b) as.character(dt[[b]]))
  do.call(paste, c(parts, list(sep = ", ")))
}

# One-row-per-record table: id | full_fp | pcfp_<col>... built from A2's
# FAITHFUL fingerprint (.exact_fp_wide_dt), block-agnostic. The full fingerprint
# is the per-column token sets pasted; per-column fingerprints ride alongside so
# the partial-recoverable read can ask "same name, different street".
#' @noRd
.plan_fp_table <- function(dt, id_col, proxy, cols, delim) {
  wide <- .exact_fp_wide_dt(
    prepare_search_data(dt, id_col, proxy),
    id_col, character(), cols, delim
  )
  fp_cols  <- paste0("fp_", cols)
  pc_cols  <- paste0("pcfp_", cols)
  cols_lst <- c(
    list(id = as.character(wide[[id_col]])),
    stats::setNames(lapply(fp_cols, function(f) wide[[f]]), pc_cols)
  )
  cols_lst$full_fp <- do.call(paste, c(cols_lst[pc_cols], list(sep = delim)))
  data.table::as.data.table(cols_lst)
}

# Frontier row for ONE candidate block key. Pure arithmetic over GROUP BYs —
# brute_pairs and twin-survival are Sum() over (block) / (fp, block) groups,
# never a materialized pair.
#' @noRd
.plan_frontier_row <- function(rec_b, rec_t, bk_b, bk_t, mode) {
  if (mode == "dedup") {
    cnt <- data.table::data.table(bk = bk_b)[, .(n = .N), by = "bk"]
    n   <- cnt$n
    list(
      n_blocks    = nrow(cnt),
      max_block   = max(n),
      mean_block  = mean(n),
      brute_pairs = sum(as.numeric(n) * (n - 1) / 2)
    )
  } else {
    cb <- data.table::data.table(bk = bk_b)[, .(nb = .N), by = "bk"]
    ct <- data.table::data.table(bk = bk_t)[, .(nt = .N), by = "bk"]
    m  <- merge(cb, ct, by = "bk", all = TRUE)
    m[is.na(nb), nb := 0L]; m[is.na(nt), nt := 0L]
    list(
      n_blocks    = nrow(m),
      max_block   = max(m$nb + m$nt),
      mean_block  = mean(m$nb + m$nt),
      brute_pairs = sum(as.numeric(m$nb) * m$nt)
    )
  }
}

# Share of exact-token-set twins that survive (stay co-blocked) under a
# candidate block key. dedup: twin pairs = Sum_fp C(n_fp, 2); survivors =
# Sum_(fp,bk) C(n,2). search: twin pairs = Sum_fp nb*nt; survivors =
# Sum_(fp,bk) nb*nt. All Sum() arithmetic — no pair is enumerated.
#' @noRd
.plan_twin_survival <- function(rec_b, rec_t, bk_b, bk_t, mode) {
  if (mode == "dedup") {
    d <- data.table::data.table(fp = rec_b$full_fp, bk = bk_b)
    tot <- d[, .(n = .N), by = "fp"][, sum(as.numeric(n) * (n - 1) / 2)]
    if (tot == 0) return(NA_real_)
    sur <- d[, .(n = .N), by = c("fp", "bk")][, sum(as.numeric(n) * (n - 1) / 2)]
    return(sur / tot)
  }
  db <- data.table::data.table(fp = rec_b$full_fp, bk = bk_b)[, .(nb = .N), by = c("fp", "bk")]
  dt <- data.table::data.table(fp = rec_t$full_fp, bk = bk_t)[, .(nt = .N), by = c("fp", "bk")]
  # total: per-fp totals (block-agnostic)
  tb <- db[, .(nb = sum(nb)), by = "fp"]
  tt <- dt[, .(nt = sum(nt)), by = "fp"]
  tot <- merge(tb, tt, by = "fp")[, sum(as.numeric(nb) * nt)]
  if (tot == 0) return(NA_real_)
  sur <- merge(db, dt, by = c("fp", "bk"))[, sum(as.numeric(nb) * nt)]
  sur / tot
}

# The min_rarity -> intermediate-overlap-row cost curve. Pure df-histogram math:
# at each threshold t, keep tokens with rarity >= t on each side, inner-join the
# surviving (col, token) vocabularies, and Sum(base_df * target_df). dedup:
# self-join, Sum(df*(df-1)/2). This is the pre-aggregation overlap-row count the
# backend would produce — never materialized here, only counted from df.
#' @noRd
.plan_min_rarity_curve <- function(btok, ttok, mode, grid) {
  bu <- unique(btok[, .(src_column, token, df, rarity)])
  if (mode == "dedup") {
    rows <- lapply(grid, function(t) {
      keep <- bu[rarity >= t]
      data.table::data.table(
        min_rarity = t,
        intermediate_pairs = keep[, sum(as.numeric(df) * (df - 1) / 2)]
      )
    })
    return(data.table::rbindlist(rows))
  }
  tu <- unique(ttok[, .(src_column, token, df, rarity)])
  data.table::setnames(tu, c("df", "rarity"), c("df_t", "rarity_t"))
  rows <- lapply(grid, function(t) {
    kb <- bu[rarity >= t, .(src_column, token, df)]
    kt <- tu[rarity_t >= t, .(src_column, token, df_t)]
    j  <- merge(kb, kt, by = c("src_column", "token"))
    data.table::data.table(
      min_rarity = t,
      intermediate_pairs = j[, sum(as.numeric(df) * df_t)]
    )
  })
  data.table::rbindlist(rows)
}

# Per-column empty-token-set rate + the score ceiling a record with that column
# empty cannot exceed: 1 - normalized_weight(col). A record missing column c can
# only ever score up to 1 - w_share(c) under weighted rIP (§25 footgun).
#' @noRd
.plan_empty_column <- function(rec_b, cols, weights, delim) {
  w <- weights / sum(weights)
  rows <- lapply(cols, function(c) {
    empty_rate <- mean(rec_b[[paste0("pcfp_", c)]] == "")
    data.table::data.table(
      column        = c,
      empty_rate    = empty_rate,
      score_ceiling = 1 - unname(w[c])
    )
  })
  data.table::rbindlist(rows)
}


# ---------------------------------------------------------------------------
# data.table method (reference implementation)
# ---------------------------------------------------------------------------

method(
  plan_strategy,
  list(DT_tbl, Search_Strategy)
) <- function(base, strategy,
              target           = NULL,
              block_candidates = list(),
              base_id          = NULL,
              target_id        = NULL,
              n_offenders      = 20L,
              min_rarity_grid  = NULL,
              containment      = FALSE,
              ...) {

  if (is.null(base_id)) cli::cli_abort("{.arg base_id} is required.")
  if (length(block_candidates) == 0L) {
    cli::cli_abort("{.arg block_candidates} must be a non-empty named list of block specs.")
  }
  if (is.null(names(block_candidates)) || any(names(block_candidates) == "")) {
    names(block_candidates) <- paste0("cand_", seq_along(block_candidates))
  }

  mode <- if (is.null(target)) "dedup" else "search"
  cols <- names(strategy@preparers)

  base_dt <- data.table::copy(data.table::as.data.table(base))
  base_dt[[base_id]] <- as.character(base_dt[[base_id]])
  if (mode == "search") {
    if (is.null(target_id)) target_id <- base_id
    target_dt <- data.table::copy(data.table::as.data.table(target))
    target_dt[[target_id]] <- as.character(target_dt[[target_id]])
  } else {
    target_dt <- NULL
    target_id <- base_id
  }

  # Validate candidate block columns exist.
  all_bcols <- unique(unlist(block_candidates))
  miss_b <- setdiff(all_bcols, names(base_dt))
  if (length(miss_b) > 0L) {
    cli::cli_abort("Candidate block column{?s} not in {.arg base}: {.field {miss_b}}.")
  }
  if (mode == "search") {
    miss_t <- setdiff(all_bcols, names(target_dt))
    if (length(miss_t) > 0L) {
      cli::cli_abort("Candidate block column{?s} not in {.arg target}: {.field {miss_t}}.")
    }
  }

  # The strategy supplies the tokenization; its own block_by is ignored here.
  # A block-AGNOSTIC proxy gives global fingerprints / df (block_by chosen per
  # candidate, not baked into the tokens).
  proxy <- .exact_proxy_strategy(strategy)
  proxy <- Search_Strategy(
    preparers = proxy@preparers, weights = numeric(), block_by = NULL,
    rarity = proxy@rarity, threshold = 1, min_rarity = 0, max_token_df = Inf,
    smoothing = proxy@smoothing, max_candidates = Inf, feedback_strength = 0
  )
  delim <- .JOINERY_FP_DELIM

  # --- per-record fingerprint tables ----------------------------------------
  rec_b <- .plan_fp_table(base_dt, base_id, proxy, cols, delim)
  rec_t <- if (mode == "search") {
    .plan_fp_table(target_dt, target_id, proxy, cols, delim)
  } else rec_b

  # --- rarity'd token tables (for column reads + curve) ---------------------
  btok <- compute_rarity(prepare_search_data(base_dt, base_id, proxy), proxy)
  ttok <- if (mode == "search") {
    compute_rarity(prepare_search_data(target_dt, target_id, proxy), proxy)
  } else btok

  # --- READ 1: blocking-resolution frontier ---------------------------------
  bk_b_list <- lapply(block_candidates, function(bc) .plan_block_key(base_dt, bc))
  bk_t_list <- if (mode == "search") {
    lapply(block_candidates, function(bc) .plan_block_key(target_dt, bc))
  } else bk_b_list
  # rec_b/rec_t rows are in fingerprint order; map block keys (raw-row order) by id.
  idx_b <- match(rec_b$id, base_dt[[base_id]])
  idx_t <- if (mode == "search") match(rec_t$id, target_dt[[target_id]]) else idx_b

  frontier_rows <- lapply(names(block_candidates), function(nm) {
    bk_b <- bk_b_list[[nm]][idx_b]
    bk_t <- if (mode == "search") bk_t_list[[nm]][idx_t] else bk_b
    fr   <- .plan_frontier_row(rec_b, rec_t, bk_b, bk_t, mode)
    surv <- .plan_twin_survival(rec_b, rec_t, bk_b, bk_t, mode)
    data.table::data.table(
      block_key           = nm,
      n_blocks            = as.integer(fr$n_blocks),
      max_block           = as.integer(fr$max_block),
      mean_block          = as.numeric(fr$mean_block),
      brute_pairs         = as.numeric(fr$brute_pairs),
      exact_twin_survival = as.numeric(surv)
    )
  })
  frontier <- data.table::rbindlist(frontier_rows)
  data.table::setorder(frontier, brute_pairs)

  # --- READ 2: exact-set persister rate -------------------------------------
  if (mode == "dedup") {
    overall <- rec_b[, .N, by = full_fp][N > 1L, sum(N)] / nrow(rec_b)
    per_col <- stats::setNames(map_dbl(cols, function(c) {
      v <- rec_b[[paste0("pcfp_", c)]]
      d <- data.table::data.table(f = v)[, .N, by = f]
      sum(d[N > 1L]$N) / length(v)
    }), cols)
  } else {
    tset <- unique(rec_t$full_fp)
    overall <- mean(rec_b$full_fp %in% tset)
    per_col <- stats::setNames(map_dbl(cols, function(c) {
      mean(rec_b[[paste0("pcfp_", c)]] %in% unique(rec_t[[paste0("pcfp_", c)]]))
    }), cols)
  }
  persister_rate <- list(overall = as.numeric(overall), per_column = per_col)

  # --- READ 3: residual structure -------------------------------------------
  ref_block <- if (!is.null(strategy@block_by)) strategy@block_by else block_candidates[[1L]]
  ref_bk_b  <- .plan_block_key(base_dt, ref_block)[idx_b]
  if (mode == "search") {
    ref_bk_t <- .plan_block_key(target_dt, ref_block)[idx_t]
    tblocks  <- unique(ref_bk_t)
    matchable <- mean(ref_bk_b %in% tblocks)
    # partial-recoverable: among base records with no full-fp twin on target,
    # the share each single column alone could still bridge.
    no_twin <- !(rec_b$full_fp %in% unique(rec_t$full_fp))
    partial <- stats::setNames(map_dbl(cols, function(c) {
      tset <- unique(rec_t[[paste0("pcfp_", c)]])
      if (sum(no_twin) == 0L) return(0)
      mean((rec_b[[paste0("pcfp_", c)]][no_twin] %in% tset) &
           (rec_b[[paste0("pcfp_", c)]][no_twin] != ""))
    }), cols)
  } else {
    bc <- data.table::data.table(bk = ref_bk_b)[, .(n = .N), by = "bk"]
    big <- bc[n > 1L]$bk
    matchable <- mean(ref_bk_b %in% big)
    no_twin <- rec_b[, .N, by = full_fp][N == 1L]$full_fp
    no_twin_set <- rec_b$full_fp %in% no_twin
    partial <- stats::setNames(map_dbl(cols, function(c) {
      v <- rec_b[[paste0("pcfp_", c)]]
      d <- data.table::data.table(f = v)[, .N, by = f]
      shared <- d[N > 1L]$f
      if (sum(no_twin_set) == 0L) return(0)
      mean((v[no_twin_set] %in% shared) & (v[no_twin_set] != ""))
    }), cols)
  }
  residual_structure <- list(
    matchable           = as.numeric(matchable),
    one_sided           = as.numeric(1 - matchable),
    partial_recoverable = partial,
    reference_block     = ref_block
  )

  # --- READ 4: per-column discriminativeness + min_rarity curve -------------
  core <- .rarity_distribution_core(btok, proxy, as.integer(n_offenders))

  if (is.null(min_rarity_grid)) {
    rr <- unique(btok$rarity)
    min_rarity_grid <- sort(unique(c(0, stats::quantile(
      rr, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), names = FALSE
    ))))
  }
  curve <- .plan_min_rarity_curve(btok, ttok, mode, min_rarity_grid)

  weights <- if (length(strategy@weights) > 0L) strategy@weights else {
    stats::setNames(rep(1 / length(cols), length(cols)), cols)
  }
  empty_col <- .plan_empty_column(rec_b, cols, weights, delim)

  # OPT-IN containment share (§22) — the one read that does a bounded structural
  # join (the exact containment kernel). NA by default so the verb stays
  # scoring-free unless the user asks.
  containment_share <- NA_real_
  if (isTRUE(containment)) {
    cprox <- Search_Strategy(
      preparers = proxy@preparers, weights = numeric(), block_by = ref_block,
      rarity = proxy@rarity, threshold = 1, min_rarity = 0, max_token_df = Inf,
      smoothing = proxy@smoothing, max_candidates = Inf, feedback_strength = 0
    )
    if (mode == "search") {
      cl <- .exact_links_containment_dt(base_dt, cprox, base_id, target_dt,
                                        target_id, ref_block, self = FALSE,
                                        "forward", 0)
      containment_share <- if (nrow(rec_b)) nrow(unique(cl[, "._bid"])) / nrow(rec_b) else NA_real_
    } else {
      cl <- .exact_links_containment_dt(base_dt, cprox, base_id, NULL, base_id,
                                        ref_block, self = TRUE, "forward", 0)
      containment_share <- if (nrow(rec_b)) nrow(unique(cl[, "._bid"])) / nrow(rec_b) else NA_real_
    }
  }

  column_reads <- list(
    distribution      = core$distribution,
    offenders         = core$offenders,
    min_rarity_curve  = curve,
    empty_column      = empty_col,
    containment_share = as.numeric(containment_share)
  )

  # --- signals -> recommendations -------------------------------------------
  signals <- list()
  # blocking knee: a coarser candidate that is (near-)lossless for twins but
  # materially cheaper than the finest one.
  knee <- .plan_blocking_knee(frontier)
  if (!is.null(knee)) {
    signals[["blocking_knee_survival"]] <- knee$survival
    signals[["blocking_knee_block"]]    <- knee$block_key
    signals[["blocking_knee_savings"]]  <- knee$savings
  }
  signals[["max_empty_column_rate"]] <- max(empty_col$empty_rate)
  signals[["max_empty_column_name"]] <- empty_col$column[which.max(empty_col$empty_rate)]
  signals[["max_empty_column_ceiling"]] <- empty_col$score_ceiling[which.max(empty_col$empty_rate)]
  if (!is.na(containment_share)) {
    signals[["containment_share"]] <- containment_share
  }
  signals[["est_comparisons"]] <- min(frontier$brute_pairs)

  recs <- .dispatch_recommendations(signals)

  out <- Strategy_Plan(
    frontier           = frontier,
    persister_rate     = persister_rate,
    residual_structure = residual_structure,
    column_reads       = column_reads,
    mode               = mode,
    recommendations    = recs$messages
  )
  attr(out, "recommendation_ids") <- recs$ids
  out
}


# Identify a blocking knee: the coarsest candidate whose twin-survival is within
# `tol` of the best survival while costing materially fewer brute pairs than the
# finest (smallest brute_pairs) candidate is the baseline. Returns NULL if none.
#' @noRd
.plan_blocking_knee <- function(frontier, tol = 0.01, min_savings = 0.25) {
  if (nrow(frontier) < 2L) return(NULL)
  fr <- data.table::copy(frontier)
  fr <- fr[!is.na(exact_twin_survival)]
  if (nrow(fr) < 2L) return(NULL)
  best_surv <- max(fr$exact_twin_survival)
  finest    <- fr[which.max(brute_pairs)]   # the most expensive (finest-recall) candidate
  # candidates that are near-lossless vs the best survival
  cand <- fr[exact_twin_survival >= best_surv - tol]
  cand <- cand[brute_pairs < finest$brute_pairs]
  if (nrow(cand) == 0L) return(NULL)
  pick <- cand[which.min(brute_pairs)]
  savings <- 1 - pick$brute_pairs / finest$brute_pairs
  if (savings < min_savings) return(NULL)
  list(block_key = pick$block_key, survival = pick$exact_twin_survival,
       savings = savings, coarser_than = finest$block_key)
}


# ---------------------------------------------------------------------------
# DuckDB method: sample to R, delegate to data.table (no pairs touch the DB)
# ---------------------------------------------------------------------------

method(
  plan_strategy,
  list(Duck_tbl, Search_Strategy)
) <- function(base, strategy,
              target           = NULL,
              block_candidates = list(),
              base_id          = NULL,
              target_id        = NULL,
              n_offenders      = 20L,
              min_rarity_grid  = NULL,
              containment      = FALSE,
              sample_n         = NULL,
              ...) {

  con      <- base$src$con
  base     <- .materialise_duck_input(base, con)
  tbl_name <- base$lazy_query$x

  pull <- function(con, tbl_name, sample_n) {
    if (is.null(sample_n)) {
      data.table::as.data.table(
        DBI::dbGetQuery(con, paste0("SELECT * FROM \"", tbl_name, "\""))
      )
    } else {
      data.table::as.data.table(
        DBI::dbGetQuery(con, paste0(
          "SELECT * FROM \"", tbl_name, "\" USING SAMPLE ",
          as.integer(sample_n), " ROWS"
        ))
      )
    }
  }

  base_dt <- pull(con, tbl_name, sample_n)

  target_dt <- NULL
  if (!is.null(target)) {
    if (inherits(target, "tbl_duckdb_connection")) {
      t_con  <- target$src$con
      target <- .materialise_duck_input(target, t_con)
      target_dt <- pull(t_con, target$lazy_query$x, sample_n)
    } else {
      target_dt <- data.table::as.data.table(target)
    }
  }

  plan_strategy(base_dt, strategy,
                target = target_dt, block_candidates = block_candidates,
                base_id = base_id, target_id = target_id,
                n_offenders = n_offenders, min_rarity_grid = min_rarity_grid,
                containment = containment, ...)
}


# ---------------------------------------------------------------------------
# Tibble / data.frame thin wrappers
# ---------------------------------------------------------------------------

.plan_df_wrapper <- function(base, strategy, target, block_candidates,
                             base_id, target_id, n_offenders,
                             min_rarity_grid, containment, ...) {
  target_dt <- if (!is.null(target)) as_DT(target) else NULL
  plan_strategy(as_DT(base), strategy,
                target = target_dt, block_candidates = block_candidates,
                base_id = base_id, target_id = target_id,
                n_offenders = n_offenders, min_rarity_grid = min_rarity_grid,
                containment = containment, ...)
}

method(plan_strategy, list(.jyDF, Search_Strategy)) <- function(
    base, strategy, target = NULL, block_candidates = list(),
    base_id = NULL, target_id = NULL, n_offenders = 20L,
    min_rarity_grid = NULL, containment = FALSE, ...) {
  .plan_df_wrapper(base, strategy, target, block_candidates, base_id,
                   target_id, n_offenders, min_rarity_grid, containment, ...)
}

method(plan_strategy, list(.jyTBL_DF, Search_Strategy)) <- function(
    base, strategy, target = NULL, block_candidates = list(),
    base_id = NULL, target_id = NULL, n_offenders = 20L,
    min_rarity_grid = NULL, containment = FALSE, ...) {
  .plan_df_wrapper(base, strategy, target, block_candidates, base_id,
                   target_id, n_offenders, min_rarity_grid, containment, ...)
}

method(plan_strategy, list(.jyTBL, Search_Strategy)) <- function(
    base, strategy, target = NULL, block_candidates = list(),
    base_id = NULL, target_id = NULL, n_offenders = 20L,
    min_rarity_grid = NULL, containment = FALSE, ...) {
  .plan_df_wrapper(base, strategy, target, block_candidates, base_id,
                   target_id, n_offenders, min_rarity_grid, containment, ...)
}


# ---------------------------------------------------------------------------
# format() / print() / coercion
# ---------------------------------------------------------------------------

method(format.Strategy_Plan, Strategy_Plan) <- function(x, ...) {
  lines <- character()
  push  <- function(...) lines <<- c(lines, paste0(...))

  push("<joinery::Strategy_Plan> (", x@mode, ")")
  push("")
  push("blocking frontier (sorted by brute_pairs):")
  fr <- x@frontier
  for (i in seq_len(nrow(fr))) {
    push(sprintf(
      "  %-16s n_blocks=%d  max=%d  brute_pairs=%.0f  twin_survival=%s",
      fr$block_key[i], fr$n_blocks[i], fr$max_block[i], fr$brute_pairs[i],
      if (is.na(fr$exact_twin_survival[i])) "NA"
      else sprintf("%.1f%%", 100 * fr$exact_twin_survival[i])
    ))
  }
  push("")
  push(sprintf("exact-set persister rate (overall): %.1f%%",
               100 * x@persister_rate$overall))

  rs <- x@residual_structure
  push("")
  push(sprintf("residual: matchable=%.1f%%  one_sided=%.1f%%  (ref block: %s)",
               100 * rs$matchable, 100 * rs$one_sided,
               paste(rs$reference_block, collapse = ", ")))

  ec <- x@column_reads$empty_column
  if (!is.null(ec) && nrow(ec) > 0L) {
    push("")
    push("empty-column score ceilings:")
    for (i in seq_len(nrow(ec))) {
      push(sprintf("  %-12s empty=%.1f%%  ceiling=%.3f",
                   ec$column[i], 100 * ec$empty_rate[i], ec$score_ceiling[i]))
    }
  }

  cs <- x@column_reads$containment_share
  if (!is.null(cs) && !is.na(cs)) {
    push("")
    push(sprintf("containment share: %.1f%%", 100 * cs))
  }

  if (length(x@recommendations) > 0L) {
    push("")
    push("recommendations:")
    for (r in x@recommendations) push("  ! ", r)
  }
  lines
}

method(print.Strategy_Plan, Strategy_Plan) <- function(x, ...) {
  cli::cli_h1(sprintf("Strategy_Plan ({.field %s})", x@mode))
  fr <- x@frontier
  cli::cli_text("{.strong blocking frontier} (by brute_pairs)")
  for (i in seq_len(nrow(fr))) {
    cli::cli_bullets(sprintf(
      "{.field %s}: %d blocks, brute_pairs=%.0f, twin_survival=%s",
      fr$block_key[i], fr$n_blocks[i], fr$brute_pairs[i],
      if (is.na(fr$exact_twin_survival[i])) "NA"
      else sprintf("%.1f%%", 100 * fr$exact_twin_survival[i])
    ))
  }
  cli::cli_text("persister rate (overall): {.val {sprintf('%.1f%%', 100*x@persister_rate$overall)}}")
  rs <- x@residual_structure
  cli::cli_text("residual matchable: {.val {sprintf('%.1f%%', 100*rs$matchable)}}")
  for (r in x@recommendations) cli::cli_alert_warning(r)
  invisible(x)
}

#' @noRd
.strategy_plan_to_dt <- function(x) {
  fr <- x@frontier
  data.table::copy(fr)[, `:=`(
    mode               = x@mode,
    persister_overall  = x@persister_rate$overall,
    matchable          = x@residual_structure$matchable,
    n_recommendations  = length(x@recommendations)
  )][]
}

method(as.data.table.Strategy_Plan, Strategy_Plan) <- function(x, ...) {
  .strategy_plan_to_dt(x)
}
method(as.data.frame.Strategy_Plan, Strategy_Plan) <- function(x, ...) {
  as.data.frame(.strategy_plan_to_dt(x))
}

method(recommendations, Strategy_Plan) <- function(x) x@recommendations
