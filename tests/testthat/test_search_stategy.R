test_that("search_strategy() creates valid Search_Strategy objects", {
  
  strat <- search_strategy(
    Nachname   ~ normalize_text + word_tokens(min_nchar = 3),
    Vorname    ~ normalize_text + word_tokens(min_nchar = 3),
    Strasse    ~ normalize_text + word_tokens(),
    Hausnummer ~ normalize_text + word_tokens(),
    Ort        ~ normalize_text + word_tokens(),
    block_by = "kreis",
    weights  = c(Nachname = 0.5, Vorname = 0.2),
    rarity   = "inverse_freq"
  )
  
  # Basic type checks ----------------------------------
  expect_true(S7::S7_inherits(strat, Search_Strategy))
  expect_type(strat@preparers, "list")
  expect_length(strat@preparers, 5)
  
  # Column names ---------------------------------------
  expect_setequal(
    names(strat@preparers),
    c("Nachname", "Vorname", "Strasse", "Hausnummer", "Ort")
  )
  
  # Check one preparer structure ------------------------
  p <- strat@preparers$Nachname
  expect_true(S7::S7_inherits(p, Search_Preparer))
  expect_identical(p@column, "Nachname")
  
  # Steps should be Step IR objects ---------------------
  expect_true(S7::S7_inherits(p@steps[[1]], Step))
  expect_true(S7::S7_inherits(p@steps[[2]], Step))
  
  # Step 1: normalize_text()
  expect_identical(p@steps[[1]]@name, "normalize_text")
  expect_identical(p@steps[[1]]@args, list())
  
  # Step 2: word_tokens(min_nchar = 3)
  expect_identical(p@steps[[2]]@name, "word_tokens")
  expect_identical(p@steps[[2]]@args$min_nchar, 3)
  
  # Blocking --------------------------------------------
  expect_identical(strat@block_by, "kreis")
  
  # Weights ---------------------------------------------
  expect_named(strat@weights)
  expect_equal(strat@weights[["Nachname"]], 0.5)
  
  # Rarity ----------------------------------------------
  expect_identical(strat@rarity, "inverse_freq")
  
  # Print should not error ------------------------------
  expect_no_error(capture.output(print(strat)))
  expect_no_error(capture.output(print(p)))
})

