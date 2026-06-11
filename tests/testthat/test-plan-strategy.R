# Pre-match, pre-strategy planning (v0.8 Stage 08).
#
# plan_strategy() surveys candidate blockings SCORING-FREE and surfaces the
# cost/recall knee before the run. These tests cover: the blocking frontier
# ranking + faithful-fingerprint twin survival; the min_rarity cost curve
# (monotone + exact vs an independent overlap-row reference); the empty-column
# score ceiling (§25); the opt-in containment share (§22); the scoring-free
# guarantee (no pairs touch the DuckDB connection); and data.table/DuckDB parity.

library(data.table)

# ---------------------------------------------------------------------------
# 1. Frontier ranks block keys; coarser block is cheaper, twin survival faithful
# ---------------------------------------------------------------------------

test_that("the frontier orders candidates by cost and reads faithful twins", {
  set.seed(1)
  dt <- data.table(
    id   = sprintf("r%03d", 1:60),
    plz2 = rep(c("10", "20", "30"), each = 20),
    plz5 = rep(sprintf("%05d", 1:12), each = 5),
    name = c(rep("anna meier gmbh", 5), rep("bert klee handel", 5),
             sample(c("cara low solo", "dora fish ag"), 50, TRUE))
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.8)
  p <- plan_strategy(dt, s, base_id = "id",
                     block_candidates = list(plz5 = "plz5", plz2 = "plz2"))

  expect_s3_class(p, "joinery::Strategy_Plan")
  fr <- p@frontier
  expect_setequal(fr$block_key, c("plz2", "plz5"))
  # sorted by brute_pairs ascending: the finer plz5 is cheaper.
  expect_equal(fr$block_key[1L], "plz5")
  expect_lt(fr[block_key == "plz5"]$brute_pairs, fr[block_key == "plz2"]$brute_pairs)

  # Twin survival is a share in [0, 1]; the coarser block keeps >= the finer one.
  expect_true(all(fr$exact_twin_survival >= 0 & fr$exact_twin_survival <= 1))
  expect_gte(fr[block_key == "plz2"]$exact_twin_survival,
             fr[block_key == "plz5"]$exact_twin_survival)

  # brute_pairs is the arithmetic Sum_block C(n,2), not a materialized join.
  n_plz2 <- dt[, .N, by = plz2]$N
  expect_equal(fr[block_key == "plz2"]$brute_pairs,
               sum(n_plz2 * (n_plz2 - 1) / 2))

  # Exact twin-survival value on a hand-computable fixture: 4 identical-name
  # records (all one fp) => C(4,2)=6 twin pairs; block splits them 2+2, so
  # co-blocked twin pairs = C(2,2)+C(2,2) = 1+1 = 2 => survival = 2/6.
  ex <- data.table(id = c("a", "b", "c", "d"),
                   blk = c("L", "L", "R", "R"),
                   name = rep("anna meier", 4))
  pe <- plan_strategy(ex, s, base_id = "id", block_candidates = list(blk = "blk"))
  expect_equal(pe@frontier[block_key == "blk"]$exact_twin_survival, 2 / 6)
})

# ---------------------------------------------------------------------------
# 2. min_rarity curve is monotone and matches an independent overlap-row count
# ---------------------------------------------------------------------------

test_that("the min_rarity cost curve is monotone and exact vs a reference join", {
  set.seed(7)
  dt <- data.table(
    id   = sprintf("r%03d", 1:40),
    plz  = rep(c("10", "20"), 20),
    name = sample(c("anna meier gmbh", "bert klee handel", "cara low solo",
                    "anna meier ag", "bert klee ag"), 40, TRUE)
  )
  s    <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)
  grid <- c(0, 0.05, 0.1, 0.2)
  p <- plan_strategy(dt, s, base_id = "id",
                     block_candidates = list(plz = "plz"),
                     min_rarity_grid = grid)
  curve <- p@column_reads$min_rarity_curve

  # Monotone non-increasing in min_rarity.
  expect_true(all(diff(curve$intermediate_pairs) <= 0))

  # The curve predicts the pre-aggregation overlap-row count Sum_token C(df,2)
  # (the quantity the backend filters on). Verify EXACTLY against an independent
  # tokenize+df computation (the expensive reference is fine in a test).
  pr  <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 1)
  tok <- compute_rarity(prepare_search_data(dt, "id", pr), pr)
  u   <- unique(tok[, .(src_column, token, df, rarity)])
  ref <- vapply(grid, function(t) u[rarity >= t, sum(as.numeric(df) * (df - 1) / 2)],
                numeric(1))
  expect_equal(curve$intermediate_pairs, ref)

  # And it moves WITH reality: the engine's actual distinct-pair count is
  # non-increasing across the same thresholds (the curve predicts the right
  # direction of the lever, bounded above by the overlap rows).
  pairs_at <- function(t) {
    st <- search_strategy(name ~ normalize_text() + word_tokens(),
                          block_by = "plz", threshold = 0, min_rarity = t)
    d  <- detect_duplicates(dt, "id", st)
    if (nrow(d) == 0L) return(0L)
    uniqueN(d$duplicate_group)
  }
  n0 <- pairs_at(0); n2 <- pairs_at(0.2)
  expect_gte(n0, n2)
  expect_lte(n2, curve[min_rarity == 0.2]$intermediate_pairs)
})

# ---------------------------------------------------------------------------
# 3. Empty-column ceiling fires at the right percentage with 1 - weight(col)
# ---------------------------------------------------------------------------

test_that("empty-column score ceiling = 1 - normalized weight, fired correctly", {
  base <- data.table(
    id     = sprintf("b%02d", 1:8),
    plz    = rep(c("10", "20"), 4),
    name   = c("anna meier", "anna meier", "bert klee", "bert klee",
               "cara low", "dora fish", "emil stein", "fred north"),
    street = c("hauptstr 1", "", "ringweg 2", "", "", "", "seeweg 9", "")
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(),
                       street ~ normalize_text() + word_tokens(),
                       threshold = 0.5)
  p <- plan_strategy(base, s, base_id = "id",
                     block_candidates = list(plz = "plz"))

  ec <- p@column_reads$empty_column
  # street is empty on 5/8 records; two equal-weight columns -> ceiling 0.5.
  st <- ec[column == "street"]
  expect_equal(st$empty_rate, 5 / 8)
  expect_equal(st$score_ceiling, 0.5)
  expect_equal(ec[column == "name"]$empty_rate, 0)

  # The empty_column_ceiling recommendation fires (street empty_rate > 10%).
  expect_true("empty_column_ceiling" %in% attr(p, "recommendation_ids"))
})

# ---------------------------------------------------------------------------
# 4. Containment share is opt-in and matches a known containment fixture
# ---------------------------------------------------------------------------

test_that("containment read is opt-in and fires consider_containment", {
  # base records whose token set is a strict subset of a target record's.
  base <- data.table(
    id   = c("b1", "b2", "b3"),
    plz  = c("10", "10", "10"),
    name = c("anna meier", "bert klee", "zeta omega")
  )
  tgt <- data.table(
    id   = c("t1", "t2"),
    plz  = c("10", "10"),
    name = c("anna meier gmbh berlin", "bert klee handel hamburg")
  )
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)

  # default: scoring-free, containment NOT computed.
  p0 <- plan_strategy(base, s, target = tgt, base_id = "id", target_id = "id",
                      block_candidates = list(plz = "plz"))
  expect_true(is.na(p0@column_reads$containment_share))

  # opt-in: b1 ⊆ t1, b2 ⊆ t2 -> 2/3 of base contained.
  p1 <- plan_strategy(base, s, target = tgt, base_id = "id", target_id = "id",
                      block_candidates = list(plz = "plz"), containment = TRUE)
  expect_equal(p1@column_reads$containment_share, 2 / 3)
  expect_true("consider_containment" %in% attr(p1, "recommendation_ids"))
})

# ---------------------------------------------------------------------------
# 5. Scoring-free guarantee: the DuckDB path puts no pairs on the connection
# ---------------------------------------------------------------------------

test_that("plan_strategy is scoring-free on DuckDB (no pairs tables created)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()

  dt <- data.table(
    id   = sprintf("r%03d", 1:50),
    plz  = rep(c("10", "20"), 25),
    name = sample(c("anna meier gmbh", "bert klee handel", "cara low"), 50, TRUE)
  )
  DBI::dbWriteTable(con, "d", as.data.frame(dt))
  s <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)

  before <- DBI::dbListTables(con)
  p <- plan_strategy(dplyr::tbl(con, "d"), s, base_id = "id",
                     block_candidates = list(plz = "plz"))
  after <- DBI::dbListTables(con)

  expect_s3_class(p, "joinery::Strategy_Plan")
  # The verb samples with SELECT * and delegates to R — it must leave NO new
  # tables (no overlap-join / scoring intermediates) on the connection.
  expect_equal(sort(after), sort(before))
})

# ---------------------------------------------------------------------------
# 6. Backend parity: data.table vs DuckDB frontier + curve agree
# ---------------------------------------------------------------------------

test_that("data.table and DuckDB plans agree on the frontier and curve", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  con <- local_duckdb_con()

  set.seed(3)
  dt <- data.table(
    id   = sprintf("r%03d", 1:60),
    plz2 = rep(c("10", "20", "30"), each = 20),
    plz5 = rep(sprintf("%05d", 1:12), each = 5),
    name = sample(c("anna meier gmbh", "bert klee handel", "cara low solo"), 60, TRUE)
  )
  s    <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.7)
  grid <- c(0, 0.1, 0.2)
  cands <- list(plz2 = "plz2", plz5 = "plz5")

  dt_p <- plan_strategy(dt, s, base_id = "id",
                        block_candidates = cands, min_rarity_grid = grid)

  DBI::dbWriteTable(con, "d", as.data.frame(dt))
  duck_p <- plan_strategy(dplyr::tbl(con, "d"), s, base_id = "id",
                          block_candidates = cands, min_rarity_grid = grid)

  expect_equal(as.data.frame(duck_p@frontier), as.data.frame(dt_p@frontier))
  expect_equal(duck_p@column_reads$min_rarity_curve$intermediate_pairs,
               dt_p@column_reads$min_rarity_curve$intermediate_pairs)
  expect_equal(duck_p@persister_rate$overall, dt_p@persister_rate$overall)
})

# ---------------------------------------------------------------------------
# 7. Validation + distinct from audit_strategy (multi-block, pre-strategy)
# ---------------------------------------------------------------------------

test_that("plan_strategy validates inputs and is multi-block by construction", {
  dt <- data.table(id = c("a", "b"), plz = c("10", "20"), name = c("x", "y"))
  s  <- search_strategy(name ~ normalize_text() + word_tokens(), threshold = 0.5)

  expect_error(plan_strategy(dt, s, block_candidates = list(plz = "plz")),
               "base_id")
  expect_error(plan_strategy(dt, s, base_id = "id"), "block_candidates")
  expect_error(
    plan_strategy(dt, s, base_id = "id", block_candidates = list(nope = "nope")),
    "not in"
  )

  # Multi-block: one call, several candidate keys compared in one frontier.
  dt2 <- data.table(id = sprintf("r%02d", 1:6), plz = rep(c("10", "20"), 3),
                    reg = rep(c("a", "b", "c"), 2), name = rep("anna meier", 6))
  p <- plan_strategy(dt2, s, base_id = "id",
                     block_candidates = list(plz = "plz", reg = "reg"))
  expect_equal(nrow(p@frontier), 2L)
  expect_setequal(p@frontier$block_key, c("plz", "reg"))
})
