test_that("normalize_text() works as in examples", {
  # Example 1: normalize_text("Café Coñac")
  expect_equal(
    normalize_text("Café Coñac"),
    "CAFE CONAC"
  )
  
  # Example 2: normalize_text(..., transliteration = "Latin-ASCII")
  expect_equal(
    normalize_text("Straße", .transliteration = "Latin-ASCII"),
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
    word_tokens("This is an example", min_length = 3),
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
