skip_if_not_installed("tibble")

# ── Fixtures ──────────────────────────────────────────────────────────────────

base_tbl <- function() {
  tibble::tibble(
    id   = c("A", "B", "C"),
    name = c("alpha beta", "gamma delta", "alpha beta"),
    city = c("Berlin", "Hamburg", "Berlin")
  )
}

target_tbl <- function() {
  tibble::tibble(
    id   = c("X", "Y"),
    name = c("alpha beta", "epsilon zeta"),
    city = c("Berlin", "Munich")
  )
}

base_df    <- function() as.data.frame(base_tbl())
target_df  <- function() as.data.frame(target_tbl())

token_strat <- function(...) {
  search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.5,
    ...
  )
}


# ── prepare_search_data ───────────────────────────────────────────────────────

test_that("prepare_search_data() works with tibble input and returns a tibble", {
  out <- prepare_search_data(base_tbl(), "id", token_strat())
  expect_true(tibble::is_tibble(out))
  expect_true(all(c("id", "src_column", "token") %in% names(out)))
})

test_that("prepare_search_data() works with data.frame input and returns a data.frame", {
  out <- prepare_search_data(base_df(), "id", token_strat())
  expect_true(is.data.frame(out))
  expect_false(tibble::is_tibble(out))
  expect_false(data.table::is.data.table(out))
})


# ── compute_rarity ────────────────────────────────────────────────────────────

test_that("compute_rarity() works on tibble token output", {
  tokens <- prepare_search_data(base_tbl(), "id", token_strat())
  out <- compute_rarity(tokens, token_strat())
  expect_true(tibble::is_tibble(out))
  expect_true("rarity" %in% names(out))
})

test_that("compute_rarity() works on data.frame token output", {
  tokens <- prepare_search_data(base_df(), "id", token_strat())
  out <- compute_rarity(tokens, token_strat())
  expect_true(is.data.frame(out))
  expect_false(tibble::is_tibble(out))
})


# ── detect_duplicates (existing) ──────────────────────────────────────────────

test_that("detect_duplicates() works with tibble input and returns a tibble", {
  tbl <- tibble::tibble(
    id   = c("A", "B"),
    name = c("alpha", "alpha")
  )
  result <- detect_duplicates(tbl, "id", token_strat())
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("duplicate_group", "id", "score", "rank") %in% names(result)))
})

test_that("detect_duplicates() works with data.frame input and returns a data.frame", {
  df <- data.frame(
    id   = c("A", "B"),
    name = c("alpha", "alpha"),
    stringsAsFactors = FALSE
  )
  result <- detect_duplicates(df, "id", token_strat())
  expect_true(is.data.frame(result))
  expect_false(data.table::is.data.table(result))
})


# ── search_candidates ─────────────────────────────────────────────────────────

test_that("search_candidates() works with tibble inputs and returns a tibble", {
  out <- search_candidates(base_tbl(), target_tbl(), "id", "id", token_strat())
  expect_true(tibble::is_tibble(out))
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out)))
})

test_that("search_candidates() works with data.frame inputs and returns a data.frame", {
  out <- search_candidates(base_df(), target_df(), "id", "id", token_strat())
  expect_true(is.data.frame(out))
  expect_false(tibble::is_tibble(out))
})


# ── extract_unmatched ─────────────────────────────────────────────────────────

test_that("extract_unmatched() works with tibble + tibble matches", {
  matches <- search_candidates(base_tbl(), target_tbl(), "id", "id", token_strat())
  base_matches <- matches[matches$source == "base", ]
  out <- extract_unmatched(base_tbl(), "id", base_matches)
  expect_true(tibble::is_tibble(out))
  expect_true("id" %in% names(out))
})

test_that("extract_unmatched() works with data.frame + data.frame matches", {
  matches <- search_candidates(base_df(), target_df(), "id", "id", token_strat())
  base_matches <- matches[matches$source == "base", ]
  out <- extract_unmatched(base_df(), "id", base_matches)
  expect_true(is.data.frame(out))
  expect_false(tibble::is_tibble(out))
})


# ── deduplicate_table ─────────────────────────────────────────────────────────

test_that("deduplicate_table() works with tibble input", {
  dupes <- detect_duplicates(base_tbl(), "id", token_strat())
  if (nrow(dupes) > 0) {
    out <- deduplicate_table(base_tbl(), dupes, "id")
    expect_true(tibble::is_tibble(out))
  } else {
    skip("No duplicates detected; skipping deduplicate_table assertion")
  }
})

test_that("deduplicate_table() works with data.frame input", {
  dupes <- detect_duplicates(base_df(), "id", token_strat())
  if (nrow(dupes) > 0) {
    out <- deduplicate_table(base_df(), dupes, "id")
    expect_true(is.data.frame(out))
    expect_false(tibble::is_tibble(out))
  } else {
    skip("No duplicates detected; skipping deduplicate_table assertion")
  }
})


# ── multi_stage_search ────────────────────────────────────────────────────────

test_that("multi_stage_search() works with tibble inputs", {
  out <- multi_stage_search(
    base_tbl(), target_tbl(), "id", "id",
    strategies = list(s1 = token_strat())
  )
  expect_true(tibble::is_tibble(out))
  expect_true(all(c("entity", "id", "rep", "rank", "score", "stage") %in% names(out)))
  expect_false(is.null(attr(out, "ledger")))   # directed ledger preserved
})

test_that("multi_stage_search() works with data.frame inputs", {
  out <- multi_stage_search(
    base_df(), target_df(), "id", "id",
    strategies = list(s1 = token_strat())
  )
  expect_true(is.data.frame(out))
  expect_false(tibble::is_tibble(out))
})
