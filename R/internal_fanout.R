# ==========================================================================
# Token-overlap fan-out guard  (v0.9 — notes/v09_performance/08_plan_fanout_cap.md)
# ==========================================================================
# The token-overlap join — the heart of every score path — joins records on a
# shared (src_column, token[, block]) and groups / scores / thresholds AFTER.
# One hot or boilerplate token shared by f records in a block materialises
# f*(f-1) (dedup) / n_base*n_target (search) intermediate rows BEFORE any filter.
# Blocking is the only other guard, and a dense block defeats it (the v0.9 audit
# CRITICAL; the YP directory-publisher clique).
#
# This guard estimates that fan-out cheaply from the token document-frequency
# histogram — `sum df*(df-1)` (dedup) / `sum df_base*df_target` (search) over
# (src_column, block, token) groups, an O(#distinct tokens) aggregate that never
# materialises a pair — and, when it busts the strategy's `max_fanout` budget,
# either auto-drops the smallest set of hyper-common (near-zero-rarity) tokens
# needed to get under it (`on_fanout = "cap"`, the default) or aborts
# (`"abort"`). The applied cut is a document-frequency ceiling `df <= cut`, the
# same axis as `max_token_df`; both backends apply the identical cut so results
# stay in parity.


# Pick the df ceiling that brings kept fan-out under budget.
#
# `hist` is a data.frame with columns `df` (distinct document-frequency values)
# and `mass` (total intermediate rows contributed by all groups at that df).
# Returns list(cut, total, kept, abort):
#   cut   — df ceiling to apply (`df <= cut`), or Inf for no-op
#   abort — TRUE when no ceiling >= 2 fits the budget (capping would have to
#           drop genuine signal tokens; the caller turns this into an error)
.fanout_choose_cut <- function(hist, budget) {
  if (nrow(hist) == 0L) return(list(cut = Inf, total = 0, kept = 0, abort = FALSE))
  o     <- order(hist$df)
  dfv   <- hist$df[o]
  mass  <- as.numeric(hist$mass[o])
  total <- sum(mass)
  if (!is.finite(budget) || total <= budget)
    return(list(cut = Inf, total = total, kept = total, abort = FALSE))
  cum <- cumsum(mass)
  ok  <- which(cum <= budget)
  if (length(ok) == 0L)
    return(list(cut = NA_real_, total = total, kept = 0, abort = TRUE))
  cut <- dfv[max(ok)]
  # A ceiling below 2 means dropping every multi-record token — that nukes real
  # signal, so refuse to cap and let the caller abort instead.
  if (cut < 2) return(list(cut = NA_real_, total = total, kept = cum[max(ok)], abort = TRUE))
  list(cut = cut, total = total, kept = cum[max(ok)], abort = FALSE)
}

# Loud warning emitted by the "cap" policy when tokens are dropped.
.fanout_warn <- function(n_groups, cut, total, kept, worst) {
  ex <- ""
  if (!is.null(worst) && nrow(worst)) {
    blk <- if (!is.null(worst$block) && nzchar(worst$block[1]))
      paste0(" in block ", worst$block[1]) else ""
    ex <- sprintf(" (e.g. %s='%s' df=%s%s)", worst$src_column[1], worst$token[1],
                  format(worst$df[1], big.mark = ","), blk)
  }
  cli::cli_warn(c(
    "!" = "Fan-out guard: dropped {.val {n_groups}} hyper-common token group{?s} \\
           (df > {.val {cut}}){ex} to bound the token-overlap join.",
    "i" = "Estimated intermediate rows {format(total, big.mark = ',', scientific = FALSE)} \\
           -> {format(kept, big.mark = ',', scientific = FALSE)}; these tokens carry \\
           near-zero rarity, so scores are essentially unchanged.",
    "i" = "Tune with {.arg max_token_df} / {.arg min_rarity}, raise {.arg max_fanout}, \\
           or set {.code on_fanout = \"off\"} to disable this guard."
  ))
}

# Abort path: "abort" policy, or "cap" fallback when the budget can't be met
# without dropping signal tokens.
.enforce_fanout_budget <- function(total, budget, worst, face,
                                   call = rlang::caller_env()) {
  what <- if (identical(face, "self")) "duplicate" else "search"
  msg <- c(
    "x" = "Estimated token-overlap fan-out \\
           ({format(total, big.mark = ',', scientific = FALSE)} intermediate rows) \\
           exceeds {.arg max_fanout} \\
           ({format(budget, big.mark = ',', scientific = FALSE)}) for this {what} pass.",
    "i" = "Raise {.arg min_rarity} or lower {.arg max_token_df} to thin hot tokens, \\
           tighten {.arg block_by}, set {.code on_fanout = \"cap\"} to auto-drop them, \\
           or raise {.arg max_fanout}."
  )
  if (!is.null(worst) && nrow(worst)) {
    blk <- if (!is.null(worst$block)) ifelse(nzchar(worst$block), paste0(" (", worst$block, ")"), "") else ""
    off <- sprintf("%s='%s' df=%s%s", worst$src_column, worst$token,
                   format(worst$df, big.mark = ","), blk)
    msg <- c(msg, stats::setNames(off, rep("*", length(off))))
  }
  cli::cli_abort(msg, call = call)
}

# Worst (highest-df) groups, as a small data.frame(src_column, token, df, block).
.fanout_worst_dt <- function(grp, block_by, n) {
  if (nrow(grp) == 0L) return(NULL)
  o <- utils::head(grp[order(-grp$df)], n)
  block <- if (length(block_by))
    do.call(paste, c(o[, block_by, with = FALSE], sep = "/")) else rep("", nrow(o))
  data.frame(src_column = o$src_column, token = o$token, df = o$df,
             block = block, stringsAsFactors = FALSE)
}


# ---- data.table backend ----------------------------------------------------
# Thin `tokens` (a compute_rarity() output) so the downstream overlap join can't
# exceed the strategy's `max_fanout`. `face = "self"` for dedup (sum df*(df-1)),
# `"cross"` for search (sum df_base*df_target, needs `id_col` + `side_col`).
.fanout_guard_dt <- function(tokens, strategy, face = c("self", "cross"),
                             id_col, side_col = NULL) {
  face   <- match.arg(face)
  budget <- strategy@max_fanout
  policy <- strategy@on_fanout
  if (identical(policy, "off") || !is.finite(budget)) return(tokens)

  # Effective block columns of the token table (plain + derived `._btok`): the
  # fan-out cost axis is per-block, and `._btok` is just another block column.
  block_by <- .block_cols(strategy)
  key <- c("src_column", block_by, "token")

  if (face == "self") {
    # df*(df-1): the off-diagonal overlap rows for one token (the join keeps
    # id1 <> id2), so a df=1 token costs nothing and never trips the guard.
    grp <- unique(tokens[, c(key, "df"), with = FALSE])
    grp[, mass := as.numeric(df) * (as.numeric(df) - 1)]
  } else {
    grp <- tokens[, .(
      df_b = data.table::uniqueN(.SD[[id_col]][.SD[[side_col]] == "base"]),
      df_t = data.table::uniqueN(.SD[[id_col]][.SD[[side_col]] == "target"])
    ), by = key, .SDcols = c(id_col, side_col)]
    grp[, df := df_b + df_t]
    grp[, mass := as.numeric(df_b) * as.numeric(df_t)]
  }

  total <- sum(grp$mass)
  if (total <= budget) return(tokens)

  worst_overall <- .fanout_worst_dt(grp, block_by, 3L)
  if (identical(policy, "abort"))
    .enforce_fanout_budget(total, budget, worst_overall, face)

  hist <- grp[, .(mass = sum(mass)), by = "df"]
  dec  <- .fanout_choose_cut(as.data.frame(hist), budget)
  if (isTRUE(dec$abort))
    .enforce_fanout_budget(dec$total, budget, worst_overall, face)
  if (!is.finite(dec$cut)) return(tokens)

  over <- grp[df > dec$cut]
  .fanout_warn(nrow(over), dec$cut, dec$total, dec$kept,
               .fanout_worst_dt(over, block_by, 1L))
  tokens[!over[, key, with = FALSE], on = key]
}


# ---- DuckDB backend --------------------------------------------------------
# Returns the df ceiling to apply (`WHERE df <= cut`), or Inf for no-op. The
# histogram is one SQL aggregate (O(#distinct tokens), no pairs); only the tiny
# (df, mass) table crosses to R. The caller folds the returned cut into a
# `df <= cut` filter, identical in effect to the data.table anti-join.
.fanout_guard_sql <- function(con, tokens_tbl, strategy, face = c("self", "cross")) {
  face   <- match.arg(face)
  budget <- strategy@max_fanout
  policy <- strategy@on_fanout
  if (identical(policy, "off") || !is.finite(budget)) return(Inf)

  # Effective block columns of the token table (plain + derived `._btok`).
  block_by <- .block_cols(strategy)
  blk_sel  <- if (length(block_by))
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", ")) else ""

  if (face == "self") {
    hist <- DBI::dbGetQuery(con, paste0(
      "SELECT df, COUNT(*) AS ng FROM (",
      "SELECT DISTINCT src_column", blk_sel, ", token, df FROM ", tokens_tbl,
      ") GROUP BY df"))
    if (nrow(hist) == 0L) return(Inf)
    hist$mass <- as.numeric(hist$df) * (as.numeric(hist$df) - 1) * as.numeric(hist$ng)
  } else {
    hist <- DBI::dbGetQuery(con, paste0(
      "SELECT (df_b + df_t) AS df, SUM(df_b * df_t) AS mass FROM (",
      "SELECT COUNT(DISTINCT CASE WHEN source = 'base'   THEN doc_id END) AS df_b, ",
      "       COUNT(DISTINCT CASE WHEN source = 'target' THEN doc_id END) AS df_t ",
      "FROM ", tokens_tbl, " GROUP BY src_column", blk_sel, ", token",
      ") WHERE df_b > 0 AND df_t > 0 GROUP BY (df_b + df_t)"))
    if (nrow(hist) == 0L) return(Inf)
    hist$mass <- as.numeric(hist$mass)
  }

  total <- sum(hist$mass)
  if (total <= budget) return(Inf)

  worst <- .fanout_worst_sql(con, tokens_tbl, block_by)
  if (identical(policy, "abort"))
    .enforce_fanout_budget(total, budget, worst, face)

  dec <- .fanout_choose_cut(hist[, c("df", "mass")], budget)
  if (isTRUE(dec$abort))
    .enforce_fanout_budget(dec$total, budget, worst, face)
  if (!is.finite(dec$cut)) return(Inf)

  n_over <- nrow(DBI::dbGetQuery(con, paste0(
    "SELECT 1 FROM (SELECT DISTINCT src_column", blk_sel, ", token, df FROM ",
    tokens_tbl, ") WHERE df > ", dec$cut)))
  .fanout_warn(n_over, dec$cut, dec$total, dec$kept, worst)
  dec$cut
}

# Highest-df token group on a DuckDB token table, for the warning / abort.
.fanout_worst_sql <- function(con, tokens_tbl, block_by) {
  blk_sel <- if (length(block_by))
    paste0(", ", paste(sprintf('"%s"', block_by), collapse = ", ")) else ""
  q <- DBI::dbGetQuery(con, paste0(
    "SELECT src_column, token, df", blk_sel,
    " FROM ", tokens_tbl, " ORDER BY df DESC LIMIT 1"))
  if (nrow(q) == 0L) return(NULL)
  block <- if (length(block_by))
    do.call(paste, c(q[, block_by, drop = FALSE], sep = "/")) else ""
  data.frame(src_column = q$src_column, token = q$token, df = q$df,
             block = block, stringsAsFactors = FALSE)
}
