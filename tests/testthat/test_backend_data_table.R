test_that("normalize_text() works as in examples", {
  # Example 1: normalize_text("Café Coñac")
  expect_equal(
    normalize_text("Café Coñac"),
    "CAFE CONAC"
  )
  
  # Example 2: normalize_text(..., transliteration = "Latin-ASCII")
  expect_equal(
    normalize_text("Straße", transliteration = "Latin-ASCII"),
    "STRASSE"
  )
})

test_that("as_metaphone() returns correct Metaphone codes", {
  
  expect_equal(
    as_metaphone("Café"),
    "KF"
  )
  
  expect_equal(
    as_metaphone("Straße"),
    "STRS"
  )
  
})

test_that("as_soundex() works as in examples", {
  expect_equal(
    as_soundex("Café"),
    "C100"
  )
  
  expect_equal(
    as_soundex("Straße"),
    "S362"
  )
})

test_that("as_cologne() works as in examples", {
  expect_equal(
    as_cologne("Café"),
    "43"
  )
  
  expect_equal(
    as_cologne("Straße"),
    "8278"
  )
})

test_that("word_tokens() works as in examples", {
  expect_equal(
    word_tokens("This is an example."),
    list(c("This", "is", "an", "example."))
  )
  
  expect_equal(
    word_tokens("Another, test; string."),
    list(c("Another,", "test;", "string."))
  )
  
  # Check min_length filter
  expect_equal(
    word_tokens("This is an example", min_nchar = 3),
    list(c("This", "example"))
  )
})

test_that("generate_ngrams() works as in examples", {
  # Example: generate_ngrams("hello", 2)
  expect_equal(
    generate_ngrams("hello", 2),
    list(c("he", "el", "ll", "lo"))
  )
  
  # Example: generate_ngrams("an example", 3)
  # "an example" has length 11
  # 3-grams:
  # "an ", "n e", " ex", "exa", "xam", "amp", "mpl", "ple"
  expect_equal(
    generate_ngrams("an example", 3),
    list(c("an ", "n e", " ex", "exa", "xam", "amp", "mpl", "ple"))
  )
  
  # Edge case: too short => empty char vector
  expect_equal(
    generate_ngrams("hi", 3),
    list(character(0))
  )
})

test_that("use_dictionary() works as in examples", {
  dict <- data.table::data.table(
    tokens = c("example", "sample"),
    token_group = c("example/sample", "example/sample")
  )
  
  expect_equal(
    use_dictionary("example", dict),
    list("example/sample")
  )
  
  # nonexistent should return character(0)
  expect_equal(
    use_dictionary("nonexistent", dict),
    list(character(0))
  )
  
  # vectorized lookup
  expect_equal(
    use_dictionary(c("example", "nonexistent"), dict),
    list("example/sample", character(0))
  )
})

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
  expected_names <- c("id_base", "column", "token", "row_id", block_cols)
  
  expect_true(all(expected_names %in% names(result)))
  
  expect_type(result$id_base, "character")
  expect_type(result$column, "character")
  expect_type(result$token,  "character")
  expect_type(result$row_id, "integer")
  
  # Expected row count (you know this for base_example)
  expect_equal(nrow(result), 16416L)
  
  # All columns in strategy should appear in output --------------------------
  expected_cols <- names(yp_strategy@preparers)
  expect_setequal(unique(result$column), expected_cols)
  
  # Each preparer should produce > 0 rows ------------------------------------
  map(expected_cols, function(col) {
    expect_true(
      sum(result$column == col) > 0L,
      info = paste("Column", col, "produced no output")
    )
  })
  
  # Spot-check pipeline for Nachname -----------------------------------------
  # Manually apply: normalize_text() then word_tokens(min_nchar = 3)
  normalized       <- normalize_text(base_example$Nachname)
  expected_tokens  <- word_tokens(normalized, min_nchar = 3)
  expected_tokens  <- unlist(expected_tokens, use.names = FALSE)
  
  # Tokens produced by prepare_search_data() for Nachname
  res_nachname <- result[column == "Nachname", token]
  
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
  expected_cols <- c("id_base", "column", "token", "row_id", block_cols, "freq", "df", "N", "rarity")
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
  ), by = c(block_cols, "column", "token")]
  
  expect_true(all(rar_check$freq_unique == 1))
  expect_true(all(rar_check$rarity_unique == 1))
  
  # Known value check: pick a specific token to validate ---------------------
  # Example: "MUELLER" in Nachname inside "Region Hannover"
  known <- rar[column == "Nachname" & token == "MUELLER" &
                 get(block_cols) == "Region Hannover"]
  
  expect_equal(known$freq[1], 33L)        # from your earlier output
  expect_equal(known$rarity[1], 1/33, tol = 1e-12)
  
  # No NA rarity values ------------------------------------------------------
  expect_false(any(is.na(rar$rarity)))
  
  # Rarity monotonicity: freq=1 tokens must have rarity=1 --------------------
  freq1 <- rar[freq == 1]
  expect_true(all(freq1$rarity == 1))
  
  # Print should not error ---------------------------------------------------
  expect_no_error(capture.output(print(rar)))
})

