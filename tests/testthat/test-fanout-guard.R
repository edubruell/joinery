# Token-overlap fan-out guard (v0.9) — R/internal_fanout.R
#
# The always-on guard that caps (or aborts on) hyper-common tokens before the
# overlap join. Covers: the cut chooser, cap+warn+faithfulness on dedup/search,
# the "off"/"abort" policies, the abort fallback, and data.table/DuckDB parity.

# ---- helpers ----------------------------------------------------------------

# group signature: sorted ids per duplicate_group, as a set of "id,id,.." strings
dup_groups <- function(res) {
  res <- data.table::as.data.table(res)
  res <- res[!is.na(duplicate_group)]
  sort(unname(vapply(
    split(res$id, res$duplicate_group),
    function(ids) paste(sort(ids), collapse = ","),
    character(1)
  )))
}

# candidate signature: sorted "base|target" pairs
cand_pairs <- function(res) {
  res <- data.table::as.data.table(res)
  if (nrow(res) == 0L) return(character())
  b <- res[source == "base",   .(match_id, bid = id)]
  t <- res[source == "target", .(match_id, tid = id)]
  m <- merge(b, t, by = "match_id")
  sort(unique(paste(m$bid, m$tid, sep = "|")))
}

# A dedup block: token "common" in all 30 records (df=30), a genuine pair
# (r01,r02) sharing rare anna+meyer, the rest singletons.
self_dt <- function() {
  data.table::data.table(
    id   = sprintf("r%02d", 1:30),
    name = c("anna meyer common", "anna meyer common",
             paste("common", paste0("uniq", sprintf("%02d", 1:28))))
  )
}

# A search block: "common" in both base records and all 20 targets (a cross
# fan-out of 2*20), a genuine b1<->t1 match on rare "anna".
cross_base <- function() data.table::data.table(
  bid = c("b1", "b2"), name = c("anna common", "zoe common")
)
cross_target <- function() data.table::data.table(
  tid  = paste0("t", 1:20),
  name = c("anna common", paste("common", paste0("u", 1:19)))
)


# ---- the cut chooser --------------------------------------------------------

test_that(".fanout_choose_cut: no-op under budget, picks the ceiling over it", {
  # df=1 (mass 0), df=2 (mass 4), df=30 (mass 870)  -> total 874
  h <- data.frame(df = c(1, 2, 30), mass = c(0, 4, 870))
  expect_equal(.fanout_choose_cut(h, 1e6)$cut, Inf)         # under budget
  dec <- .fanout_choose_cut(h, 100)
  expect_equal(dec$cut, 2)                                   # drop df>2
  expect_equal(dec$kept, 4)
  expect_false(dec$abort)
})

test_that(".fanout_choose_cut: aborts when no ceiling >= 2 fits", {
  # only df=2 groups, total mass 12 > budget 10 -> cannot cap without nuking df=2
  h <- data.frame(df = 2, mass = 12)
  dec <- .fanout_choose_cut(h, 10)
  expect_true(dec$abort)
  expect_true(is.na(dec$cut))
})


# ---- data.table dedup -------------------------------------------------------

test_that("cap fires, warns, and still finds the genuine duplicate (dt)", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 100)
  expect_warning(
    res <- suppressMessages(detect_duplicates(self_dt(), "id", s)),
    "Fan-out guard"
  )
  expect_equal(dup_groups(res), "r01,r02")
})

test_that("cap is faithful: identical to off + the equivalent max_token_df (dt)", {
  capped <- suppressMessages(suppressWarnings(detect_duplicates(
    self_dt(), "id",
    search_strategy(name ~ word_tokens(), weights = c(name = 1),
                    threshold = 0.9, max_fanout = 100)
  )))
  # the chooser picks cut = 2, so the faithful control drops df > 2 explicitly
  control <- suppressMessages(detect_duplicates(
    self_dt(), "id",
    search_strategy(name ~ word_tokens(), weights = c(name = 1),
                    threshold = 0.9, on_fanout = "off", max_token_df = 2)
  ))
  cap <- data.table::as.data.table(capped)[order(id)]
  con <- data.table::as.data.table(control)[order(id)]
  expect_equal(dup_groups(cap), dup_groups(con))
  expect_equal(cap$score, con$score, tolerance = 1e-9)
})

test_that("on_fanout = 'off' is a silent no-op", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, on_fanout = "off")
  expect_warning(suppressMessages(detect_duplicates(self_dt(), "id", s)), NA)
})

test_that("under-budget default does not warn", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 1e7)
  expect_warning(suppressMessages(detect_duplicates(self_dt(), "id", s)), NA)
})

test_that("on_fanout = 'abort' raises an actionable error (dt)", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 100, on_fanout = "abort")
  expect_error(suppressMessages(detect_duplicates(self_dt(), "id", s)),
               "exceeds")
})

test_that("cap aborts as fallback when the budget can't be met (dt)", {
  # 6 genuine pairs (df=2 tokens), no hyper-common token to drop -> must abort
  dt <- data.table::data.table(
    id   = sprintf("r%02d", 1:12),
    name = rep(paste0("pair", 1:6), each = 2)
  )
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 10, on_fanout = "cap")
  expect_error(suppressMessages(detect_duplicates(dt, "id", s)), "exceeds")
})


# ---- data.table search ------------------------------------------------------

test_that("cap fires on search and keeps the genuine candidate (dt)", {
  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 10)
  expect_warning(
    res <- suppressMessages(
      search_candidates(cross_base(), cross_target(), "bid", "tid", s)
    ),
    "Fan-out guard"
  )
  expect_equal(cand_pairs(res), "b1|t1")
})

test_that("search cap is faithful vs off + equivalent max_token_df (dt)", {
  capped <- suppressMessages(suppressWarnings(search_candidates(
    cross_base(), cross_target(), "bid", "tid",
    search_strategy(name ~ word_tokens(), weights = c(name = 1),
                    threshold = 0.9, max_fanout = 10)
  )))
  control <- suppressMessages(search_candidates(
    cross_base(), cross_target(), "bid", "tid",
    search_strategy(name ~ word_tokens(), weights = c(name = 1),
                    threshold = 0.9, on_fanout = "off", max_token_df = 2)
  ))
  expect_equal(cand_pairs(capped), cand_pairs(control))
})


# ---- DuckDB parity ----------------------------------------------------------

test_that("DuckDB dedup cap matches the data.table backend", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 100)
  dt_res <- suppressMessages(suppressWarnings(detect_duplicates(self_dt(), "id", s)))

  duck <- local_duckdb_table(as.data.frame(self_dt()), "self_tbl")
  dk_res <- suppressMessages(suppressWarnings(
    dplyr::collect(detect_duplicates(duck, "id", s))
  ))
  expect_equal(dup_groups(dk_res), dup_groups(dt_res))
})

test_that("DuckDB dedup also aborts under on_fanout = 'abort'", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 100, on_fanout = "abort")
  duck <- local_duckdb_table(as.data.frame(self_dt()), "self_tbl")
  expect_error(suppressMessages(detect_duplicates(duck, "id", s)), "exceeds")
})

test_that("DuckDB search cap matches the data.table backend", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  s <- search_strategy(name ~ word_tokens(), weights = c(name = 1),
                       threshold = 0.9, max_fanout = 10)
  dt_res <- suppressMessages(suppressWarnings(
    search_candidates(cross_base(), cross_target(), "bid", "tid", s)
  ))

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "b", as.data.frame(cross_base()))
  DBI::dbWriteTable(con, "t", as.data.frame(cross_target()))
  dk_res <- suppressMessages(suppressWarnings(dplyr::collect(
    search_candidates(dplyr::tbl(con, "b"), dplyr::tbl(con, "t"), "bid", "tid", s)
  )))
  expect_equal(cand_pairs(dk_res), cand_pairs(dt_res))
})
