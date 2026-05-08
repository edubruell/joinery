test_that("smooth_rip helpers return correct Smoothing objects", {
  sm <- smooth_rip_identity()
  expect_true(S7::S7_inherits(sm, Smoothing_None))
  expect_identical(sm@method, "none")

  sm <- smooth_rip_log()
  expect_true(S7::S7_inherits(sm, Smoothing_Log))
  expect_identical(sm@method, "log")

  sm <- smooth_rip_offset(alpha = 0.3)
  expect_true(S7::S7_inherits(sm, Smoothing_Offset))
  expect_identical(sm@method, "offset")
  expect_equal(sm@alpha, 0.3)

  sm <- smooth_rip_softmax(temperature = 2)
  expect_true(S7::S7_inherits(sm, Smoothing_Softmax))
  expect_identical(sm@method, "softmax")
  expect_equal(sm@temperature, 2)
})

test_that("smooth_rip_offset() validates alpha", {
  expect_error(smooth_rip_offset(alpha = "x"))
  expect_error(smooth_rip_offset(alpha = c(0.1, 0.2)))
  expect_error(smooth_rip_offset(alpha = -1))
  expect_no_error(smooth_rip_offset(alpha = 0))
  expect_no_error(smooth_rip_offset(alpha = 0.5))
})

test_that("smooth_rip_softmax() validates temperature", {
  expect_error(smooth_rip_softmax(temperature = "x"))
  expect_error(smooth_rip_softmax(temperature = c(1, 2)))
  expect_error(smooth_rip_softmax(temperature = 0))
  expect_error(smooth_rip_softmax(temperature = -1))
  expect_no_error(smooth_rip_softmax(temperature = 1))
  expect_no_error(smooth_rip_softmax(temperature = 2))
})

test_that("search_strategy() rejects invalid arguments", {
  valid_fml <- quote(name ~ word_tokens())

  # rarity must be a single character string
  expect_error(search_strategy(name ~ word_tokens(), rarity = 123))
  expect_error(search_strategy(name ~ word_tokens(), rarity = c("inverse_freq", "tfidf")))

  # min_rarity must be numeric
  expect_error(search_strategy(name ~ word_tokens(), min_rarity = "high"))

  # max_candidates must be a single positive numeric
  expect_error(search_strategy(name ~ word_tokens(), max_candidates = "top5"))
  expect_error(search_strategy(name ~ word_tokens(), max_candidates = -1))
  expect_error(search_strategy(name ~ word_tokens(), max_candidates = 0))
  expect_error(search_strategy(name ~ word_tokens(), max_candidates = c(1, 2)))

  # feedback_strength must be a single non-negative numeric
  expect_error(search_strategy(name ~ word_tokens(), feedback_strength = -0.5))
  expect_error(search_strategy(name ~ word_tokens(), feedback_strength = c(0.1, 0.2)))

  # block_by must be NULL or character
  expect_error(search_strategy(name ~ word_tokens(), block_by = 123))

  # weights must be named when non-empty
  expect_error(search_strategy(name ~ word_tokens(), weights = c(0.5, 0.5)))

  # smoothing must be a Smoothing object
  expect_error(search_strategy(name ~ word_tokens(), smoothing = "log"))
  expect_error(search_strategy(name ~ word_tokens(), smoothing = 1))

  # all ... args must be formulas
  expect_error(search_strategy("not a formula"))
  expect_error(search_strategy(name ~ word_tokens(), "also not a formula"))
})

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

test_that("search_strategy() stores smoothing, max_candidates, feedback_strength", {
  s <- search_strategy(
    name ~ word_tokens(),
    smoothing = smooth_rip_log()
  )
  expect_true(S7::S7_inherits(s@smoothing, Smoothing_Log))

  s <- search_strategy(
    name ~ word_tokens(),
    smoothing = smooth_rip_offset(0.5)
  )
  expect_true(S7::S7_inherits(s@smoothing, Smoothing_Offset))
  expect_equal(s@smoothing@alpha, 0.5)

  s <- search_strategy(
    name ~ word_tokens(),
    smoothing = smooth_rip_softmax(1.5)
  )
  expect_true(S7::S7_inherits(s@smoothing, Smoothing_Softmax))
  expect_equal(s@smoothing@temperature, 1.5)

  s <- search_strategy(name ~ word_tokens(), max_candidates = 3L, feedback_strength = 0.4)
  expect_equal(s@max_candidates, 3)
  expect_equal(s@feedback_strength, 0.4)
})

test_that("search_strategy() handles single-step and bare-symbol steps", {
  s <- search_strategy(name ~ word_tokens())
  expect_length(s@preparers$name@steps, 1L)
  expect_identical(s@preparers$name@steps[[1]]@name, "word_tokens")

  s <- search_strategy(name ~ normalize_text + word_tokens)
  expect_length(s@preparers$name@steps, 2L)
  expect_identical(s@preparers$name@steps[[1]]@name, "normalize_text")
  expect_identical(s@preparers$name@steps[[1]]@args, list())
  expect_identical(s@preparers$name@steps[[2]]@name, "word_tokens")
})

test_that("print(Search_Strategy) covers all display branches", {
  # No preparers
  expect_no_error(print(search_strategy(threshold = 0.9)))

  # Log smoothing (else branch in smoothing display)
  expect_no_error(print(search_strategy(name ~ word_tokens(), smoothing = smooth_rip_log())))

  # Offset smoothing
  expect_no_error(print(search_strategy(name ~ word_tokens(), smoothing = smooth_rip_offset(0.3))))

  # Softmax smoothing
  expect_no_error(print(search_strategy(name ~ word_tokens(), smoothing = smooth_rip_softmax(2))))

  # Finite max_candidates
  expect_no_error(print(search_strategy(name ~ word_tokens(), max_candidates = 5)))

  # feedback_strength > 0
  expect_no_error(print(search_strategy(name ~ word_tokens(), feedback_strength = 0.7)))

  # Blocking present
  expect_no_error(print(search_strategy(name ~ word_tokens(), block_by = "region")))

  # Weights present
  expect_no_error(print(search_strategy(
    name ~ word_tokens(),
    city ~ word_tokens(),
    weights = c(name = 0.7, city = 0.3)
  )))
})

test_that("print(Search_Preparer) handles all argument patterns", {
  # No-arg step
  expect_no_error(print(search_strategy(name ~ normalize_text())@preparers$name))

  # Named-arg step
  expect_no_error(print(search_strategy(name ~ word_tokens(min_nchar = 3))@preparers$name))

  # Positional (unnamed) arg step
  expect_no_error(print(search_strategy(name ~ generate_ngrams(2))@preparers$name))
})

