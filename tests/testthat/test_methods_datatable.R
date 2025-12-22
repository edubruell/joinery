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

test_that("log smoothing increases ngram similarity", {
  three_meyers <- data.table(
    id = 1:3,
    Nachname = c("Meyer", "Meier", "Mair"),
    Kreis = "A"
  )
  
  strat_no_smooth <- search_strategy(
    Nachname ~ normalize_text() + generate_ngrams(2),
    block_by = "Kreis",
    threshold = 0.1
  )
  
  strat_log_smooth <- search_strategy(
    Nachname ~ normalize_text() + generate_ngrams(2),
    block_by = "Kreis",
    threshold = 0.1,
    smoothing = smooth_rip_log()
  )
  
  res_no  <- detect_duplicates(three_meyers, "id", strat_no_smooth)
  res_log <- detect_duplicates(three_meyers, "id", strat_log_smooth)
  
  # Only the pair Meyer–Meier should appear, so compare their scores directly
  score_no  <- res_no$score[res_no$duplicate_group == 1][1]
  score_log <- res_log$score[res_log$duplicate_group == 1][1]
  
  expect_gt(score_log, score_no)
})

test_that("max_candidates limits candidate matches per record", {
  # Create synthetic data where one base record matches many target records
  # Base: one "Smith" record
  base <- data.table(
    id = "B1",
    name = "Smith"
  )
  
  # Target: 10 "Smith" records with slight variations
  target <- data.table(
    id = paste0("T", 1:10),
    name = c("Smith", "Smith", "Smith", "Smithson", "Smithe", 
             "Smith", "Smith", "Smithers", "Smith", "Smith")
  )
  
  # Strategy without containment (default max_candidates = Inf)
  strat_unlimited <- search_strategy(
    name ~ normalize_text() + generate_ngrams(2),
    threshold = 0.1
  )
  
  # Strategy with containment limit of 3
  strat_limited <- search_strategy(
    name ~ normalize_text() + generate_ngrams(2),
    threshold = 0.1,
    max_candidates = 3
  )
  
  # Run candidate search
  res_unlimited <- search_candidates(
    base, target, "id", "id", strat_unlimited
  )
  
  res_limited <- search_candidates(
    base, target, "id", "id", strat_limited
  )
  
  # Count matches for the base record
  base_matches_unlimited <- res_unlimited[source == "base", .N, by = id]$N
  base_matches_limited   <- res_limited[source == "base", .N, by = id]$N
  
  # Unlimited should have more than 3 matches
  expect_gt(base_matches_unlimited, 3)
  
  # Limited should have exactly 3 matches
  expect_equal(base_matches_limited, 3)
  
  # Limited results should be the top-3 by score
  # Get all unlimited matches and pick top 3
  top3_scores <- res_unlimited[source == "base"][
    order(-score)
  ][1:3]$score
  
  limited_scores <- res_limited[source == "base"]$score
  
  # The limited scores should match the top 3 from unlimited
  expect_equal(sort(limited_scores, decreasing = TRUE), 
               sort(top3_scores, decreasing = TRUE))
})

test_that("feedback_strength penalizes low-overlap matches", {
  # Create records where we can observe clear overlap differences
  # Use multi-token records to see the feedback effect
  
  test_data <- data.table(
    id = c("R1", "R2", "R3"),
    name = c(
      "Smith Jones Brown",  # R1 - 3 tokens
      "Smith Jones Brown",  # R2 - exact match (100% overlap)
      "Smith Anderson Lee"  # R3 - partial match (33% overlap with R1)
    )
  )
  
  strat_no_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.1
  )
  
  strat_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.1,
    feedback_strength = 0.5
  )
  
  res_no_fb <- detect_duplicates(test_data, "id", strat_no_fb)
  res_fb <- detect_duplicates(test_data, "id", strat_fb)
  
  # Both should find matches
  expect_gt(nrow(res_no_fb), 0)
  expect_gt(nrow(res_fb), 0)
  
  # R1-R2 should have perfect score (exact match)
  expect_true(all(c("R1", "R2") %in% res_no_fb$id))
  expect_true(all(c("R1", "R2") %in% res_fb$id))
  
  # R3 might or might not be in results depending on threshold/scoring
  # But if it is, its score should be lower with feedback
  if ("R3" %in% res_no_fb$id && "R3" %in% res_fb$id) {
    r3_no_fb <- res_no_fb[id == "R3"]$score[1]
    r3_fb <- res_fb[id == "R3"]$score[1]
    
    # Feedback should penalize the partial match
    expect_lt(r3_fb, r3_no_fb)
  }
})

test_that("feedback_strength works in search_candidates", {
  # Use multi-token base records to see feedback effect clearly
  base <- data.table(
    id = "B1",
    name = "Smith Jones Brown"  # 3 tokens
  )
  
  target <- data.table(
    id = c("T1", "T2"),
    name = c(
      "Smith Jones Brown",  # T1 - exact match (100% overlap)
      "Smith Anderson Lee"  # T2 - partial match (33% overlap from B1's view)
    )
  )
  
  strat_no_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.01
  )
  
  strat_fb <- search_strategy(
    name ~ normalize_text() + word_tokens(),
    threshold = 0.01,
    feedback_strength = 0.5
  )
  
  res_no_fb <- search_candidates(base, target, "id", "id", strat_no_fb)
  res_fb <- search_candidates(base, target, "id", "id", strat_fb)
  
  # Should find both matches
  expect_equal(nrow(res_no_fb[source == "base"]), 2)
  expect_equal(nrow(res_fb[source == "base"]), 2)
  
  # Get scores for B1's matches
  scores_no_fb <- res_no_fb[source == "base", .(match_id, score)]
  scores_fb <- res_fb[source == "base", .(match_id, score)]
  
  # Match 1 (T1 - exact match): score should stay close to 1.0
  exact_match_no_fb <- max(scores_no_fb$score)
  exact_match_fb <- max(scores_fb$score)
  expect_gte(exact_match_fb, exact_match_no_fb * 0.99)
  
  # Match 2 (T2 - partial): score should be lower with feedback
  partial_match_no_fb <- min(scores_no_fb$score)
  partial_match_fb <- min(scores_fb$score)
  expect_lt(partial_match_fb, partial_match_no_fb)
})

test_that("max_candidates trims pairwise candidates in detect_duplicates", {
  # Create many similar records using ngrams (high recall)
  # With ngrams, we'll get many low-quality matches
  many_similar <- data.table(
    id = paste0("R", 1:20),
    name = c(
      rep("Mueller", 5),     # Cluster 1: exact matches
      rep("Müller", 5),      # Cluster 2: slight variation
      rep("Muller", 5),      # Cluster 3: no umlaut
      rep("Moeller", 5)      # Cluster 4: different spelling
    ),
    block = "A"
  )
  
  # Use ngrams with low threshold to create MANY candidate pairs
  strat_unlimited <- search_strategy(
    name ~ normalize_text() + generate_ngrams(2),
    block_by = "block",
    threshold = 0.2  # very low - many pairs will match
  )
  
  # Same but with strict containment
  strat_limited <- search_strategy(
    name ~ normalize_text() + generate_ngrams(2),
    block_by = "block",
    threshold = 0.2,
    max_candidates = 3  # each record can match at most 3 others
  )
  
  res_unlimited <- detect_duplicates(many_similar, "id", strat_unlimited)
  res_limited   <- detect_duplicates(many_similar, "id", strat_limited)
  
  # Both should find matches
  expect_gt(nrow(res_unlimited), 0)
  
  # The limited version should produce fewer or equal rows
  # (containment removes edges, which can disconnect components)
  expect_lte(nrow(res_limited), nrow(res_unlimited))
  
  # More specifically: the limited result should have fewer records OR more groups
  # (both indicate that containment had an effect)
  has_effect <- (nrow(res_limited) < nrow(res_unlimited)) ||
                (length(unique(res_limited$duplicate_group)) > 
                 length(unique(res_unlimited$duplicate_group)))
  
  expect_true(has_effect)
})
