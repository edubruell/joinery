# Token-blocking (block_on_tokens) - Feature A, notes/region_free_linking.md §4
#
# Region-free blocking: a record blocks on each of its (rare) tokens of a
# designated column instead of a literal column value. Covers key selection
# under max_df, region-free reach, the `._btok` explosion's interaction with
# the non-unique-id guard and the fan-out guard, mixed character + token
# blocking, plain-character regression, and data.table/DuckDB parity.

# ---- helpers ----------------------------------------------------------------

# duplicate-group signature: sorted ids per group, as "id,id,.." strings
dup_groups_bt <- function(res) {
  res <- data.table::as.data.table(res)
  res <- res[!is.na(duplicate_group)]
  if (nrow(res) == 0L) return(character())
  sort(unname(vapply(
    split(res$id, res$duplicate_group),
    function(ids) paste(sort(ids), collapse = ","),
    character(1)
  )))
}

# candidate signature: sorted "base|target" pairs
cand_pairs_bt <- function(res) {
  res <- data.table::as.data.table(res)
  if (nrow(res) == 0L) return(character())
  b <- res[source == "base",   .(match_id, bid = id)]
  t <- res[source == "target", .(match_id, tid = id)]
  m <- merge(b, t, by = "match_id")
  sort(unique(paste(m$bid, m$tid, sep = "|")))
}


# ---- constructor + key selection -------------------------------------------

test_that("block_on_tokens validates its arguments", {
  expect_s7_class(block_on_tokens("name", max_df = 50), Block_On_Tokens)
  expect_error(block_on_tokens(123), "string")
  expect_error(block_on_tokens("name", max_df = 0), "max_df")
  expect_error(block_on_tokens("name", min_rarity = -1), "min_rarity")
})

test_that("a capless token block warns (no key selection)", {
  expect_warning(block_on_tokens("name"), "key selection")
  # a real cap is silent
  expect_silent(block_on_tokens("name", max_df = 10))
  expect_silent(block_on_tokens("name", min_rarity = 0.1))
})

test_that(".block_cols resolves token blocks to ._btok, plain pass through", {
  s_tok <- search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                           weights = c(name = 1),
                           block_by = block_on_tokens("name", max_df = 5))
  expect_identical(.block_cols(s_tok), "._btok")
  expect_identical(.plain_block_cols(s_tok), character())

  s_mix <- search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                           weights = c(name = 1),
                           block_by = list(block_on_tokens("name", max_df = 5), "plz2"))
  expect_identical(.block_cols(s_mix), c("plz2", "._btok"))
  expect_identical(.plain_block_cols(s_mix), "plz2")

  s_plain <- search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                             weights = c(name = 1), block_by = c("plz2", "kreis"))
  expect_identical(.block_cols(s_plain), c("plz2", "kreis"))
  expect_identical(.plain_block_cols(s_plain), c("plz2", "kreis"))
})

test_that("a record blocks only under its rare token, not its common one", {
  # "gmbh" is in every record (df=5, common); each distinctive token is df<=2.
  dt <- data.table::data.table(
    id   = as.character(1:5),
    name = c("nivelsteiner sandwerke gmbh", "nivelsteiner gmbh",
             "aldi gmbh", "aldi markt", "rewe gmbh")
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name", max_df = 2),
    threshold = 0.3
  )
  d <- detect_duplicates(dt, "id", s)
  # 1&2 link (nivelsteiner), 3&4 link (aldi); rewe is a singleton, and no pair
  # forms through the common token "gmbh" (df=5 > max_df=2, dropped as a key).
  expect_setequal(dup_groups_bt(d), c("1,2", "3,4"))
})


# ---- region-free reach ------------------------------------------------------

test_that("two records reach each other across different plz via a rare token", {
  dt <- data.table::data.table(
    id   = as.character(1:3),
    name = c("nivelsteiner gmbh", "nivelsteiner sandwerke", "rewe ag"),
    plz  = c("52134", "99999", "10000")     # all different - literal block fails
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name", max_df = 2),
    threshold = 0.3
  )
  d <- detect_duplicates(dt, "id", s)
  expect_setequal(dup_groups_bt(d), "1,2")

  # A plain plz block would NOT reach them (different plz).
  s_plz <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1), block_by = "plz", threshold = 0.3
  )
  expect_length(dup_groups_bt(detect_duplicates(dt, "id", s_plz)), 0L)
})


# ---- the ._btok explosion does NOT trip the non-unique-id guard -------------

test_that("the ._btok explosion does not raise a non-unique-id warning", {
  # All ids are unique; the explosion repeats each id across its ._btok values.
  dt <- data.table::data.table(
    id   = as.character(1:3),
    name = c("alpha beta", "alpha gamma", "delta")
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name", max_df = 2),
    threshold = 0.3
  )
  expect_no_warning(prepare_search_data(dt, "id", s))
  expect_no_warning(detect_duplicates(dt, "id", s))
})


# ---- mixed character + token blocking ---------------------------------------

test_that("mixed block_by blocks on BOTH a rare name token AND plz2", {
  dt <- data.table::data.table(
    id   = as.character(1:3),
    name = c("nivelsteiner gmbh", "nivelsteiner gmbh", "nivelsteiner gmbh"),
    plz2 = c("52", "52", "99")          # 1&2 share plz2; 3 is elsewhere
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = list(block_on_tokens("name", max_df = 3), "plz2"),
    threshold = 0.4
  )
  d <- detect_duplicates(dt, "id", s)
  # Same rare token in all three, but only 1&2 share plz2 -> only 1&2 link.
  expect_setequal(dup_groups_bt(d), "1,2")
})


# ---- plain-character regression --------------------------------------------

test_that("plain character block_by is unchanged by Feature A", {
  dt <- data.table::data.table(
    id   = as.character(1:4),
    name = c("alpha corp", "alpha corp", "beta corp", "beta corp"),
    reg  = c("x", "x", "y", "y")
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1), block_by = "reg", threshold = 0.5
  )
  d <- detect_duplicates(dt, "id", s)
  expect_setequal(dup_groups_bt(d), c("1,2", "3,4"))
})


# ---- fan-out guard fires on a common block token ----------------------------

test_that("the fan-out guard fires on a deliberately common block token", {
  # "common" is a surviving block key in all 12 records (capless spec), fanning
  # a dense block (df=12 -> 12*11=132 intermediate rows). A genuine rare pair
  # ("rarex") gives the cap policy a low-df token to keep. A tiny max_fanout
  # busts the budget on the hot token.
  n <- 12L
  dt <- data.table::data.table(
    id   = as.character(seq_len(n)),
    name = c(paste("common", seq_len(n - 2)),
             "common rarex aaa", "common rarex bbb")
  )
  # abort policy: the guard must stop.
  s_abort <- suppressWarnings(search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name"),     # capless -> "common" is a key
    threshold = 0.1,
    max_fanout = 5, on_fanout = "abort"
  ))
  expect_error(detect_duplicates(dt, "id", s_abort), "fan-out|max_fanout")

  # cap policy: the guard warns and drops the hot token (df=12), keeping the
  # rare one (df=2 -> 2 rows, under budget).
  s_cap <- suppressWarnings(search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name"),
    threshold = 0.1,
    max_fanout = 10, on_fanout = "cap"
  ))
  expect_warning(detect_duplicates(dt, "id", s_cap), "Fan-out guard")
})


# ---- print renders the spec readably ----------------------------------------

test_that("print.Search_Strategy renders a token-blocking spec", {
  s <- search_strategy(name ~ normalize_text + word_tokens(min_nchar = 3),
                       weights = c(name = 1),
                       block_by = block_on_tokens("name", max_df = 50))
  out <- cli::cli_fmt(print(s))
  expect_true(any(grepl("block_on_tokens(name, max_df=50)", out, fixed = TRUE)))
})


# ---- backend parity (data.table vs DuckDB) ----------------------------------

test_that("DuckDB dedup matches data.table under token-blocking (region-free)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  dt <- data.table::data.table(
    id   = as.character(1:5),
    name = c("nivelsteiner sandwerke gmbh", "nivelsteiner gmbh",
             "aldi gmbh", "aldi markt", "rewe gmbh"),
    plz  = c("52134", "99999", "10000", "20000", "30000")  # all different
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name", max_df = 2),
    threshold = 0.3
  )

  dt_res <- detect_duplicates(dt, "id", s)

  duck <- local_duckdb_table(as.data.frame(dt), "firms")
  duck_res <- suppressMessages(dplyr::collect(detect_duplicates(duck, "id", s)))

  expect_setequal(dup_groups_bt(duck_res), dup_groups_bt(dt_res))
  expect_setequal(dup_groups_bt(dt_res), c("1,2", "3,4"))
})

test_that("DuckDB search matches data.table under token-blocking", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  base <- data.table::data.table(
    id = as.character(1:3),
    name = c("nivelsteiner gmbh", "aldi markt", "rewe ag")
  )
  targ <- data.table::data.table(
    id = as.character(11:13),
    name = c("nivelsteiner sandwerke", "aldi sued", "edeka")
  )
  s <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 1),
    block_by = block_on_tokens("name", max_df = 2),
    threshold = 0.3
  )

  dt_res <- search_candidates(base, targ, "id", "id", s)

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "b", as.data.frame(base))
  DBI::dbWriteTable(con, "t", as.data.frame(targ))
  duck_res <- suppressMessages(dplyr::collect(
    search_candidates(dplyr::tbl(con, "b"), dplyr::tbl(con, "t"), "id", "id", s)
  ))

  expect_setequal(cand_pairs_bt(duck_res), cand_pairs_bt(dt_res))
  expect_setequal(cand_pairs_bt(dt_res), c("1|11", "2|12"))
})

test_that("explain_match explains the max-scoring block when a pair shares several block-tokens", {
  # id1,id2 share two rare blocking tokens (alpha, beta) but differ on the third
  # scored token; extra records skew alpha's vs beta's block-local rarity, so the
  # pair scores differently in each ._btok block. The scorer keeps the max block;
  # explain_match must report the same score, not the first block it happens upon.
  dt <- data.table::data.table(
    id = 1:7,
    name = c("alpha beta zeta", "alpha beta omega",
             "alpha pone", "alpha ptwo", "alpha pthree",
             "beta qone", "beta qtwo")
  )
  strat <- search_strategy(
    name ~ normalize_text + word_tokens(min_nchar = 3),
    weights  = c(name = 1.0),
    block_by = block_on_tokens("name", max_df = 20),
    threshold = 0.0
  )
  dups <- detect_duplicates(dt, "id", strat)

  g <- dups[id == 1, duplicate_group][1]
  pair_score <- unique(dups[duplicate_group == g & id %in% c(1, 2), score])
  expect_length(pair_score, 1L)

  ex <- explain_match(dups, strat, base = dt, id = "id", match_id = g)
  # Internal round-trip (mandatory contract).
  expect_equal(sum(ex@per_column_contrib$contribution), ex@score, tolerance = 1e-10)
  # And the explanation's score equals the reported match score (max block).
  expect_equal(ex@score, pair_score, tolerance = 1e-10)
})
