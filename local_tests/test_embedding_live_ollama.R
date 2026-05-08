# Live Ollama embedding test
# ---------------------------------------------------------------------------
# Excluded from the package via .Rbuildignore. Requires:
#   - tidyllm
#   - a running ollama server
#   - the embedding model below pulled locally
#
# Usage:
#   Rscript local_tests/test_embedding_live_ollama.R
#
# Covers (per CLAUDE.md):
#   1) Smoke test: embed + match a tiny dataset end-to-end
#   2) Backend parity: data.table vs duckdb produce equivalent matches
#   3) Known-pair correctness: hand-crafted near-duplicates rank above noise
# ---------------------------------------------------------------------------

devtools::load_all()

library(tidyllm)
library(data.table)
library(testthat)

MODEL_NAME <- "all-minilm:latest"

# Confirm the model is available locally; bail out cleanly if not.
installed <- tryCatch(tidyllm::ollama_list_models(), error = function(e) NULL)
if (is.null(installed) || !MODEL_NAME %in% installed$name) {
  stop(sprintf(
    "Model '%s' not available via ollama_list_models(). Pull with:\n  ollama pull %s",
    MODEL_NAME, sub(":.*$", "", MODEL_NAME)
  ))
}

emb_model <- ollama(.model = sub(":latest$", "", MODEL_NAME))

# ---- Fixtures --------------------------------------------------------------
# Hand-crafted near-duplicates. Each pair (A1/A2, B1/B2, C1/C2) describes the
# same entity with surface variation that token matching alone struggles with.
base_dt <- data.table(
  id   = c("A1", "B1", "C1", "D1"),
  name = c(
    "International Business Machines Corporation",
    "Apple Inc.",
    "Alphabet Inc. (Google)",
    "Tesla Motors"
  ),
  city = c("Armonk, NY", "Cupertino, CA", "Mountain View, CA", "Austin, TX")
)

target_dt <- data.table(
  id   = c("A2", "B2", "C2", "E2"),
  name = c(
    "IBM Corp",
    "Apple Computer Incorporated",
    "Google LLC, a subsidiary of Alphabet",
    "Microsoft Corporation"
  ),
  city = c("Armonk", "Cupertino", "Mountain View", "Redmond, WA")
)

strategy <- embedding_strategy(
  columns         = c("name", "city"),
  embedding_model = emb_model,
  threshold       = 0.6,
  batch_size      = 8L
)

# ---- 1. Smoke test (data.table) -------------------------------------------
test_that("live: search_candidates runs end-to-end on data.table", {
  out <- search_candidates(base_dt, target_dt, "id", "id", strategy)

  expect_true(is.data.table(out))
  expect_true(all(c("match_id", "score", "source", "id", "rank") %in% names(out)))
  expect_setequal(unique(out$source), c("base", "target"))
  expect_true(nrow(out) > 0L)
})

# ---- 2. Known-pair correctness --------------------------------------------
test_that("live: hand-crafted near-duplicates are top matches", {
  out <- search_candidates(base_dt, target_dt, "id", "id", strategy)

  # Pull (base_id, target_id) from each match group; rank-1 base vs rank-1 target.
  pairs <- out[, .(
    base_id   = id[source == "base"][1L],
    target_id = id[source == "target"][1L],
    score     = score[1L]
  ), by = match_id]

  expected <- list(c("A1", "A2"), c("B1", "B2"), c("C1", "C2"))
  for (p in expected) {
    hit <- pairs[base_id == p[[1]] & target_id == p[[2]]]
    expect_true(
      nrow(hit) == 1L,
      info = sprintf("expected pair %s/%s missing", p[[1]], p[[2]])
    )
    expect_gt(hit$score, 0.6)
  }

  # Microsoft (E2) and Tesla (D1) should NOT pair with each other.
  expect_equal(nrow(pairs[base_id == "D1" & target_id == "E2"]), 0L)
})

# ---- 3. Backend parity: data.table vs DuckDB ------------------------------
test_that("live: data.table and duckdb produce the same matched pairs", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("dplyr")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:", array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # vss extension is required; install if missing.
  tryCatch(DBI::dbExecute(con, "INSTALL vss;"), error = function(e) NULL)

  DBI::dbWriteTable(con, "base_tbl",   as.data.frame(base_dt))
  DBI::dbWriteTable(con, "target_tbl", as.data.frame(target_dt))
  base_db   <- dplyr::tbl(con, "base_tbl")
  target_db <- dplyr::tbl(con, "target_tbl")

  out_dt    <- search_candidates(base_dt, target_dt, "id", "id", strategy)
  out_duck  <- search_candidates(base_db, target_db, "id", "id", strategy)

  pairs_dt <- out_dt[, .(
    base_id   = id[source == "base"][1L],
    target_id = id[source == "target"][1L]
  ), by = match_id][, .(base_id, target_id)]

  pairs_duck <- as.data.table(out_duck)[, .(
    base_id   = id[source == "base"][1L],
    target_id = id[source == "target"][1L]
  ), by = match_id][, .(base_id, target_id)]

  setkey(pairs_dt,   base_id, target_id)
  setkey(pairs_duck, base_id, target_id)

  expect_equal(pairs_dt, pairs_duck)
})

# ---- 4. Smoke test: detect_duplicates -------------------------------------
test_that("live: detect_duplicates groups identical-meaning records", {
  combined <- rbind(
    base_dt,
    data.table(
      id   = c("A1_dup", "B1_dup"),
      name = c(
        "International Business Machines Corp.",
        "Apple Incorporated"
      ),
      city = c("Armonk, NY", "Cupertino, CA")
    )
  )
  out <- detect_duplicates(combined, id = "id", strategy = strategy)

  expect_true(is.data.table(out))
  expect_true(all(c("duplicate_group", "score", "id", "rank") %in% names(out)))
  # A1 and A1_dup should land in the same duplicate_group.
  a_groups <- out[id %in% c("A1", "A1_dup"), unique(duplicate_group)]
  expect_equal(length(a_groups), 1L)
})

cat("\nAll live ollama embedding checks passed.\n")
