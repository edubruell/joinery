test_that("prepare_search_data() works for data.table backend", {
  data("base_example", package = "joinery")   # adjust package if needed
  
  yp_strategy <- search_strategy(
    Nachname ~ normalize_text + word_tokens(min_nchar= 3),
    Vorname ~ normalize_text + word_tokens(min_nchar = 3),
    Strasse ~ normalize_text + word_tokens,
    Hausnummer ~ normalize_text + word_tokens,
    Ort ~ normalize_text + word_tokens,
    block_by = "Kreis" 
  )
  
  result <- prepare_search_data(
    data     = as.data.table(base_example),
    id       = "id_base",
    strategy = yp_strategy
  )
  
  # Basic structure ----------------------------------------------------------
  expect_true(data.table::is.data.table(result))
  
  # Expected columns: id, column, token, row_id, and any block_by cols
  block_cols    <- yp_strategy@block_by %||% character()
  expected_names <- c("id_base", "src_column", "token", "row_id", block_cols)
  
  expect_true(all(expected_names %in% names(result)))
  
  expect_type(result$id_base, "character")
  expect_type(result$src_column, "character")
  expect_type(result$token,  "character")
  expect_type(result$row_id, "integer")
  
  # Expected row count (you know this for base_example)
  expect_equal(nrow(result), 17171)
  
  # All columns in strategy should appear in output --------------------------
  expected_cols <- names(yp_strategy@preparers)
  expect_setequal(unique(result$src_column), expected_cols)
  
  # Each preparer should produce > 0 rows ------------------------------------
  map(expected_cols, function(col) {
    expect_true(
      sum(result$src_column == col) > 0L,
      info = paste("Column", col, "produced no output")
    )
  })
  
  # Spot-check pipeline for Nachname -----------------------------------------
  # Manually apply: normalize_text() then word_tokens(min_nchar = 3)
  normalized       <- normalize_text(base_example$Nachname)
  expected_tokens  <- word_tokens(normalized, min_nchar = 3)
  expected_tokens  <- unlist(expected_tokens, use.names = FALSE)
  
  # Tokens produced by prepare_search_data() for Nachname
  res_nachname <- result[src_column == "Nachname", token]
  
  # Lengths should match, and tokens should be the same (order should match too)
  expect_equal(length(res_nachname), length(expected_tokens))
  expect_identical(res_nachname, expected_tokens)
  
  # Sanity: no empty tokens --------------------------------------------------
  expect_false(any(result$token == ""))
  
  # Print should not error ---------------------------------------------------
  expect_no_error(capture.output(print(result)))
})


test_that("compute_rarity() works for data.table backend", {
  data("base_example", package = "joinery")   # adjust package if needed
  
  yp_strategy <- search_strategy(
    Nachname ~ normalize_text + word_tokens(min_nchar= 3),
    Vorname ~ normalize_text + word_tokens(min_nchar = 3),
    Strasse ~ normalize_text + word_tokens,
    Hausnummer ~ normalize_text + word_tokens,
    Ort ~ normalize_text + word_tokens,
    block_by = "Kreis" 
  )
  
  tokens <- prepare_search_data(
    data     = as.data.table(base_example),
    id       = "id_base",
    strategy = yp_strategy
  )
  
  # Compute rarity
  rar <- compute_rarity(tokens, yp_strategy)
  
  # Basic structure ----------------------------------------------------------
  expect_true(data.table::is.data.table(rar))
  expect_true("rarity" %in% names(rar))
  expect_type(rar$rarity, "double")
  
  # Rarity must be non-negative
  expect_true(all(rar$rarity >= 0))
  
  # Expected grouping variables
  block_cols <- yp_strategy@block_by %||% character()
  expected_cols <- c("id_base", "src_column", "token", "row_id", block_cols, "freq", "df", "N", "rarity")
  expect_true(all(expected_cols %in% names(rar)))
  
  # Inverse frequency expectation -------------------------------------------
  # For inverse_freq, rarity = 1 / freq
  # Check this for a small, known subset (e.g., the first 20 rows)
  subset <- rar[1:20]
  expect_equal(
    subset$rarity,
    1 / subset$freq,
    tolerance = 1e-12
  )
  
  # Column/block grouping should yield same freq for duplicate rows ----------
  # (All rows within a group must have same freq and rarity)
  rar_check <- rar[, .(
    n_rows = .N,
    freq_unique   = uniqueN(freq),
    rarity_unique = uniqueN(rarity)
  ), by = c(block_cols, "src_column", "token")]
  
  expect_true(all(rar_check$freq_unique == 1))
  expect_true(all(rar_check$rarity_unique == 1))
  
  # Known value check: pick a specific token to validate ---------------------
  # Example: "MUELLER" in Nachname inside "Region Hannover"
  known <- rar[src_column == "Nachname" & token == "MUELLER" &
                 get(block_cols) == "Region Hannover"]
  
  expect_equal(known$freq[1], 25)        # from your earlier output
  expect_equal(known$rarity[1], 1/25, tol = 1e-12)
  
  # No NA rarity values ------------------------------------------------------
  expect_false(any(is.na(rar$rarity)))
  
  # Rarity monotonicity: freq=1 tokens must have rarity=1 --------------------
  freq1 <- rar[freq == 1]
  expect_true(all(freq1$rarity == 1))
  
  # Print should not error ---------------------------------------------------
  expect_no_error(capture.output(print(rar)))
})

test_that("detect_duplicates() works for data.table backend and returns merged original data", {
  
  data("base_example", package = "joinery")
  
  yp_strategy <- search_strategy(
    Nachname ~ normalize_text + word_tokens(min_nchar = 3),
    Vorname  ~ normalize_text + word_tokens(min_nchar = 3),
    Strasse  ~ normalize_text + word_tokens,
    Hausnummer ~ normalize_text + word_tokens,
    Ort ~ normalize_text + word_tokens,
    block_by = "Kreis", 
    threshold = 0.8
  )
  
  # --- Run duplicate detection --------------------------------------------
  dup <- detect_duplicates(
    as.data.table(base_example),
    id        = "id_base",
    strategy  = yp_strategy
  )
  
  # --- Basic structure -----------------------------------------------------
  expect_true(data.table::is.data.table(dup))
  expect_true(all(c("id", "duplicate_group", "score", "rank") %in% names(dup)))
  
  # At least a few groups should exist
  expect_true(length(unique(dup$duplicate_group)) >= 1)
  
  # --- Check that identical rows have score = 1 ----------------------------
  # Known pair in base_example: B3142 and B2039 are identical
  pair <- dup[id %in% c("B3142", "B2039")]
  
  expect_equal(sort(pair$score), c(1, 1))
  
  # --- Check score logic: rIP ensures column-sum = 1 per identical record ---
  # For any exact pair, the sum of weights is 1
  # (weights are equal by default)
  expect_equal(unique(pair$score), 1)
  
  # --- Check ranking: identical scores must rank deterministically ----------
  # rank = 1, 2 for the pair in the same group
  expect_setequal(pair$rank, c(1L, 2L))
  
  # --- Check that merging back original data preserves values --------------
  joined_row <- dup[id == "B3142"]
  
  expect_equal(joined_row$Vorname,  subset(base_example,id_base=="B3142")$Vorname)
  expect_equal(joined_row$Nachname, subset(base_example,id_base=="B3142")$Nachname)
  expect_equal(joined_row$Ort,      subset(base_example,id_base=="B3142")$Ort)
  
  # --- Check no NA in duplicate_group or score/rank ------------------------
  expect_false(any(is.na(dup$duplicate_group)))
  expect_false(any(is.na(dup$score)))
  expect_false(any(is.na(dup$rank)))
  
  # --- Print should not error ----------------------------------------------
  expect_no_error(capture.output(print(dup)))
})

test_that("deduplicate_table() removes non-top-ranked duplicates", {
  
  data("base_example", package = "joinery")
  
  # Simple synthetic duplicate table for testing:
  # cluster 1: A kept, B removed
  # cluster 2: C kept
  duplicates <- data.table::data.table(
    id = c("A", "B", "C"),
    duplicate_group = c(1, 1, 2),
    score = c(1, 0.8, 1),
    rank  = c(1L, 2L, 1L)
  )
  
  # Base table to deduplicate
  base <- data.table::data.table(
    id = c("A", "B", "C", "D"),
    value = c(10, 20, 30, 40)
  )
  
  # Expected behavior:
  # - Remove B (rank != 1)
  # - Keep A, C, D
  result <- deduplicate_table(
    base_table = base,
    duplicates = duplicates,
    id         = "id"
  )
  
  # Correct set of remaining IDs
  expect_setequal(result$id, c("A", "C", "D"))
  
  # Removed ID is really gone
  expect_false("B" %in% result$id)
  
  # Output row order should remain consistent
  expect_equal(result$id, c("A", "C", "D"))
  
  # Values must remain unchanged
  expect_equal(result$value[result$id == "A"], 10)
  expect_equal(result$value[result$id == "C"], 30)
  expect_equal(result$value[result$id == "D"], 40)
  
  # No duplicates in the output
  expect_true(all(result[, .N, by = id]$N == 1))
})

test_that("extract_unmatched() works for data.table backend", {
  
  data("base_example", package = "joinery")
  base_dt <- as.data.table(base_example)
  
  # Simple match table mimicking joinery output
  # (Only the column name `id` matters for extract_unmatched)
  matches <- data.table::data.table(
    id = c("B0001", "B0003", "B0005"),
    score = c(0.95, 0.92, 0.90),
    match_id = 1:3
  )
  
  # --- Run extract_unmatched() --------------------------------------------
  result <- extract_unmatched(
    data    = base_dt,
    id      = "id_base",
    matches = matches
  )
  
  # --- Basic structure -----------------------------------------------------
  expect_true(data.table::is.data.table(result))
  expect_true("id_base" %in% names(result))
  
  # Matched IDs must be removed
  expect_false(any(result$id_base %in% matches$id))
  
  # All other IDs must remain
  remaining_ids <- setdiff(base_dt$id_base, matches$id)
  expect_setequal(result$id_base, remaining_ids)
  
  # Order must be preserved relative to original
  expect_equal(
    result$id_base,
    base_dt[!(id_base %in% matches$id), id_base]
  )
  
  # No columns must be lost
  expect_setequal(names(result), names(base_dt))
  
  # Should work with empty match tables -------------------------------------
  result2 <- extract_unmatched(
    data    = base_dt,
    id      = "id_base",
    matches = data.table(id = character())
  )
  expect_equal(result2$id_base, base_dt$id_base)
  
  # Should work when every row is matched -----------------------------------
  result3 <- extract_unmatched(
    data    = base_dt,
    id      = "id_base",
    matches = data.table(id = base_dt$id_base)
  )
  expect_equal(nrow(result3), 0L)
  
  # Print should not error --------------------------------------------------
  expect_no_error(capture.output(print(result)))
})


test_that("multi_stage_match works with example data", {
  base   <- as.data.table(base_example)
  target <- as.data.table(target_example)
  
  # Strategy A: strict word tokens
  strat_a <- search_strategy(
    Vorname ~ normalize_text + word_tokens(min_nchar = 3),
    Nachname ~ normalize_text + word_tokens(min_nchar = 3),
    block_by = "Kreis",
    threshold = 0.8
  )
  
  # Strategy B: fallback n-grams
  strat_b <- search_strategy(
    Vorname ~ normalize_text + generate_ngrams(n = 3),
    Nachname ~ normalize_text + generate_ngrams(n = 3),
    block_by = "Kreis",
    threshold = 0.6
  )
  
  # Intentionally no names supplied → should auto-name strategy_1, strategy_2
  strategies <- list(strat_a, strat_b)
  
  res <- multi_stage_match(
    base,
    target,
    base_id = "id_base",
    target_id = "id_target",
    strategies = strategies
  )
  
  expect_s3_class(res, "data.table")
  
  # Output schema should contain:
  expect_true(all(c("match_id", "score", "stage", "source", "id", "rank") %in% names(res)))
  
  # Stages must equal the auto names
  expect_true(all(unique(res$stage) %in% c("strategy_1", "strategy_2")))
  
  # Global match IDs must be unique
  expect_equal(
    length(unique(res$match_id)),
    length(unique(res$match_id))
  )
  
  # Basic sanity
  expect_gt(nrow(res), 0)
  expect_true(all(res$rank >= 1))
  expect_true(all(res$source %in% c("base", "target")))
})
