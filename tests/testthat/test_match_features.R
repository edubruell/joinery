# ============================================================
# Tests: match_features() + Match_Features class
# ============================================================
#
# Reference: notes/calibration_design.md (column schema; locked API).
# ============================================================

library(data.table)


# ---------- fixtures --------------------------------------------------

make_dedup_fixture <- function() {
  # 4 records. Tokens and their occurrence on the base side:
  #   john (a, d) occ=2, jon (b) occ=1, smith (a, b) occ=2,
  #   jane (c) occ=1, doe  (c, d) occ=2.
  # maxocc = 2 across the column, so aIP = 0 for occ=2, aIP = 1 for occ=1.
  data.table(
    id   = c("a", "b", "c", "d"),
    name = c("john smith", "jon smith", "jane doe", "john doe")
  )
}

make_candidate_base <- function() {
  data.table(
    id   = c("b1", "b2", "b3"),
    name = c("john smith",  "alice jones", "carlos rare"),
    city = c("berlin",      "berlin",      "paris")
  )
}

make_candidate_target <- function() {
  data.table(
    id   = c("t1", "t2", "t3"),
    name = c("john smith",  "alice jonas", "carlos rare"),
    city = c("berlin",      "berlin",      "paris")
  )
}

simple_dedup_strategy <- function() {
  search_strategy(name ~ word_tokens(), threshold = 0.3)
}

multi_col_strategy <- function() {
  search_strategy(
    name ~ word_tokens(),
    city ~ word_tokens(),
    threshold = 0.3
  )
}


# ---------- core schema ----------------------------------------------

test_that("dedup match_features returns documented core columns in order", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)

  mf <- match_features(dups, s, base = base, id = "id")

  expect_s3_class(mf@features, "data.table")
  expect_equal(mf@schema, "token")
  expect_equal(mf@strategy_class, "Search_Strategy")

  core_first <- c("searched", "found", "match_id", "stage", "score",
                  "cnt", "icnt", "ipos", "scnt", "rcnt")
  expect_equal(names(mf@features)[seq_along(core_first)], core_first)
})

test_that("dedup pairs are (rank-1, rank-k) for k >= 2 within each group", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")

  # 4 records all in one group → 3 pairs
  expect_equal(nrow(mf@features), 3L)
  expect_true(all(mf@features$searched == "d"))
  expect_setequal(mf@features$found, c("a", "b", "c"))
})


# ---------- aIP attribution: hand-worked m/f/s -----------------------

test_that("matched / found-only / search-only aIPs match hand-worked values", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")

  ft <- mf@features

  # All occ=2 tokens have aIP = 0; all occ=1 tokens have aIP = 1.
  pair_db <- ft[searched == "d" & found == "b"]   # {john,doe} vs {jon,smith}
  pair_da <- ft[searched == "d" & found == "a"]   # {john,doe} vs {john,smith}
  pair_dc <- ft[searched == "d" & found == "c"]   # {john,doe} vs {jane,doe}

  # (d, b): no matched tokens; found-only = {jon=1, smith=0}
  #         search-only = {john=0, doe=0}
  expect_true(is.na(pair_db$m_name_1))
  expect_equal(sort(c(pair_db$f_name_1, pair_db$f_name_2),
                    na.last = TRUE), c(0, 1))

  # (d, a): matched={john=0}; found-only={smith=0}; search-only={doe=0}
  expect_equal(pair_da$m_name_1, 0)
  expect_equal(pair_da$f_name_1, 0)
  expect_equal(pair_da$s_name_1, 0)

  # (d, c): matched={doe=0}; found-only={jane=1}; search-only={john=0}
  expect_equal(pair_dc$m_name_1, 0)
  expect_equal(pair_dc$f_name_1, 1)
  expect_equal(pair_dc$s_name_1, 0)
})


# ---------- block statistics -----------------------------------------

test_that("block stats cnt/icnt/ipos are computed within searched-id blocks", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")
  ft   <- mf@features

  expect_true(all(ft$cnt  == 3L))   # one block of 3 candidates
  expect_true(all(ft$icnt == 3L))   # 3 distinct found ids
  expect_true(all(ft$ipos > 0 & ft$ipos <= 1))
})


# ---------- ipos numerical -------------------------------------------

test_that("ipos is rank/N within each searched block (highest score = 1.0)", {
  # Hand-baked candidates table: one searched record with three candidates
  # at distinct scores 0.3, 0.6, 0.9.
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    score    = c(0.3, 0.3, 0.6, 0.6, 0.9, 0.9),
    source   = rep(c("base", "target"), 3),
    id       = c("b1", "t1", "b1", "t2", "b1", "t3"),
    rank     = rep(c(1L, 2L), 3)
  )
  bs <- data.table(id = "b1", name = "alpha beta")
  tg <- data.table(id = c("t1","t2","t3"),
                   name = c("alpha","beta","alpha beta"))
  s <- search_strategy(name ~ word_tokens(), threshold = 0.1)
  mf <- match_features(matches, s, base = bs, id = "id",
                       target = tg, target_id = "id")
  setorder(mf@features, score)
  expect_equal(mf@features$ipos, c(1/3, 2/3, 3/3))
})


# ---------- stage column always present ------------------------------

test_that("stage column is present and NA in single-stage runs", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")
  expect_true("stage" %in% names(mf@features))
  expect_true(all(is.na(mf@features$stage)))
})


# ---------- top_n configuration --------------------------------------

test_that("top_n list overrides per column; 0 suppresses the set", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)

  mf2 <- match_features(dups, s, base = base, id = "id",
                        top_n = list(default = 2L))
  m_cols <- grep("^m_name_", names(mf2@features), value = TRUE)
  expect_equal(length(m_cols), 2L)

  mf0 <- match_features(dups, s, base = base, id = "id",
                        top_n = list(name = 0L, default = 0L))
  expect_false(any(grepl("^m_name_", names(mf0@features))))
  expect_false(any(grepl("^f_name_", names(mf0@features))))
  expect_false(any(grepl("^s_name_", names(mf0@features))))
})

test_that("scalar top_n is interpreted as default", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)

  mf <- match_features(dups, s, base = base, id = "id", top_n = 3L)
  expect_equal(sum(grepl("^m_name_", names(mf@features))), 3L)
})


# ---------- candidates (cross-table) --------------------------------

test_that("candidate match_features uses base id as searched, target id as found", {
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)

  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id")

  expect_true(all(mf@features$searched %in% bs$id))
  expect_true(all(mf@features$found    %in% tg$id))
  expect_equal(mf@schema, "token")
  expect_true("m_name_1" %in% names(mf@features))
  expect_true("m_city_1" %in% names(mf@features))
})


# ---------- repeated-across-column tokens (rcnt, r1..rn) -------------

test_that("rcnt and r1..rn fire when a token appears in more than one column", {
  # Build a base where "berlin" appears in both name and city columns
  # for one record.
  bs <- data.table(
    id   = c("b1", "b2"),
    name = c("berlin smith", "alice jonas"),
    city = c("berlin",       "paris")
  )
  tg <- data.table(
    id   = c("t1", "t2"),
    name = c("berlin smith", "alice jonas"),
    city = c("berlin",       "paris")
  )
  s    <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf   <- match_features(cand, s, base = bs, id = "id",
                         target = tg, target_id = "id")

  # For b1, "berlin" appears in both name and city → rcnt >= 1
  row_b1 <- mf@features[searched == "b1"][1L, ]
  expect_gte(row_b1$rcnt, 1L)
  # r1 (name column max aIP among repeated tokens) is non-NA
  expect_false(is.na(row_b1$r1))
})


# ---------- empty matches edge case ----------------------------------

test_that("match_features handles empty match tables gracefully", {
  empty <- data.table(
    duplicate_group = integer(),
    id              = character(),
    score           = numeric(),
    rank            = integer()
  )
  s <- simple_dedup_strategy()
  mf <- match_features(empty, s, base = make_dedup_fixture(), id = "id")
  expect_equal(nrow(mf@features), 0L)
  expect_equal(mf@schema, "token")
  expect_equal(mf@strategy_class, "Search_Strategy")
})


# ---------- embedding-strategy reduced schema ------------------------

test_that("Embedding_Strategy dispatch returns the reduced schema", {
  emb_s <- embedding_strategy(
    columns         = "name",
    embedding_model = "openai_embedding_model",
    threshold       = 0.5
  )
  # Build a minimal candidates-shaped matches table by hand
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b2", "t2"),
    rank     = c(1L, 2L, 1L, 2L)
  )

  mf <- match_features(matches, emb_s)

  expect_equal(mf@schema, "embedding")
  expect_equal(mf@strategy_class, "Embedding_Strategy")
  # Reduced schema: no token columns
  expect_false(any(grepl("^m_|^f_|^s_|^r[0-9]+$|^scnt$|^rcnt$",
                         names(mf@features))))
  # But core columns are present
  expect_true(all(c("searched", "found", "match_id", "score",
                    "cnt", "icnt", "ipos") %in% names(mf@features)))
})


# ---------- M3: string similarity (token strategy) -------------------

test_that("string-sim columns are emitted for each preparer column (token)", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id")

  expect_true(all(c("sim_sf_name", "sim_fs_name",
                    "sim_sf_city", "sim_fs_city") %in% names(mf@features)))
})

test_that("identical strings score 1, different strings score < 1", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id")
  ft <- mf@features

  # b1↔t1: name = "john smith" both sides, city = "berlin"
  row_b1t1 <- ft[searched == "b1" & found == "t1"]
  expect_equal(row_b1t1$sim_sf_name, 1)
  expect_equal(row_b1t1$sim_fs_name, 1)
  expect_equal(row_b1t1$sim_sf_city, 1)

  # b2↔t2: name differs slightly ("alice jones" vs "alice jonas")
  row_b2t2 <- ft[searched == "b2" & found == "t2"]
  expect_true(row_b2t2$sim_sf_name < 1)
  expect_true(row_b2t2$sim_sf_name > 0)
})

test_that("method = 'lv' gives different numbers than method = 'jw'", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf_jw <- match_features(cand, s, base = bs, id = "id",
                          target = tg, target_id = "id", method = "jw")
  mf_lv <- match_features(cand, s, base = bs, id = "id",
                          target = tg, target_id = "id", method = "lv")
  expect_false(isTRUE(all.equal(mf_jw@features$sim_sf_name,
                                mf_lv@features$sim_sf_name)))
})

test_that("include_string_sim = FALSE suppresses sim_* columns", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id",
                       include_string_sim = FALSE)
  expect_false(any(grepl("^sim_(sf|fs)_", names(mf@features))))
})

test_that("string-sim columns follow s_* block in canonical order (token)", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id")
  cols <- names(mf@features)
  last_s   <- max(grep("^s_",      cols))
  first_sf <- min(grep("^sim_sf_", cols))
  last_sf  <- max(grep("^sim_sf_", cols))
  first_fs <- min(grep("^sim_fs_", cols))
  expect_true(last_s < first_sf)
  expect_true(last_sf < first_fs)
})

test_that("Jaro-Winkler is symmetric: sim_sf == sim_fs per row", {
  skip_if_not_installed("stringdist")
  bs <- make_candidate_base()
  tg <- make_candidate_target()
  s  <- multi_col_strategy()
  cand <- search_candidates(bs, tg, "id", "id", s)
  mf <- match_features(cand, s, base = bs, id = "id",
                       target = tg, target_id = "id", method = "jw")
  expect_equal(mf@features$sim_sf_name, mf@features$sim_fs_name)
  expect_equal(mf@features$sim_sf_city, mf@features$sim_fs_city)
})


# ---------- M3: embedding feature path -------------------------------

# Deterministic embedder: each unique text gets a stable, unique
# unnormalized vector. Used for testing the embedding feature path.
fake_embed_dt <- function(text, model) {
  vecs <- lapply(text, function(t) {
    set.seed(sum(utf8ToInt(t)) %% .Machine$integer.max)
    stats::runif(8, 0.1, 1.0)
  })
  tibble::tibble(input = text, embeddings = vecs)
}

test_that("Embedding_Strategy match_features emits cosine_sim, norms, sim_*", {
  skip_if_not_installed("tidyllm")
  skip_if_not_installed("stringdist")
  local_mocked_bindings(embed = fake_embed_dt, .package = "tidyllm")

  bs <- data.table(id = c("b1", "b2"), name = c("alpha beta", "gamma delta"))
  tg <- data.table(id = c("t1", "t2"), name = c("alpha beta", "epsilon zeta"))
  es <- embedding_strategy(
    columns         = "name",
    embedding_model = list(.model = "fake"),
    threshold       = 0.0,
    batch_size      = 100L
  )
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b2", "t2"),
    rank     = c(1L, 2L, 1L, 2L)
  )

  mf <- match_features(matches, es,
                       base = bs, id = "id",
                       target = tg, target_id = "id")
  ft <- mf@features

  # New columns present
  expect_true(all(c("cosine_sim", "embedding_norm_s", "embedding_norm_f",
                    "sim_sf_name", "sim_fs_name") %in% names(ft)))

  # cosine_sim == score
  expect_equal(ft$cosine_sim, ft$score)

  # Norms positive, finite, NOT all 1.0 — confirms un-normalized recompute
  expect_true(all(is.finite(ft$embedding_norm_s)))
  expect_true(all(is.finite(ft$embedding_norm_f)))
  expect_true(all(ft$embedding_norm_s > 0))
  expect_true(all(ft$embedding_norm_f > 0))
  expect_false(isTRUE(all.equal(ft$embedding_norm_s,
                                rep(1.0, nrow(ft)))))
})

test_that("Embedding_Strategy match_features canonical column order", {
  skip_if_not_installed("tidyllm")
  skip_if_not_installed("stringdist")
  local_mocked_bindings(embed = fake_embed_dt, .package = "tidyllm")

  bs <- data.table(id = c("b1", "b2"), name = c("alpha beta", "gamma delta"))
  tg <- data.table(id = c("t1", "t2"), name = c("alpha beta", "epsilon zeta"))
  es <- embedding_strategy(
    columns         = "name",
    embedding_model = list(.model = "fake"),
    threshold       = 0.0,
    batch_size      = 100L
  )
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b2", "t2"),
    rank     = c(1L, 2L, 1L, 2L)
  )
  mf <- match_features(matches, es,
                       base = bs, id = "id",
                       target = tg, target_id = "id")
  cols <- names(mf@features)
  expect_true(match("sim_sf_name", cols)  < match("sim_fs_name", cols))
  expect_true(match("sim_fs_name", cols)  < match("cosine_sim", cols))
  expect_true(match("cosine_sim", cols)   < match("embedding_norm_s", cols))
  expect_true(match("embedding_norm_s", cols) <
              match("embedding_norm_f", cols))
})

test_that("Embedding_Strategy match_features include_string_sim=FALSE keeps cosine+norms", {
  skip_if_not_installed("tidyllm")
  local_mocked_bindings(embed = fake_embed_dt, .package = "tidyllm")

  bs <- data.table(id = c("b1", "b2"), name = c("alpha beta", "gamma delta"))
  tg <- data.table(id = c("t1", "t2"), name = c("alpha beta", "epsilon zeta"))
  es <- embedding_strategy(
    columns         = "name",
    embedding_model = list(.model = "fake"),
    threshold       = 0.0,
    batch_size      = 100L
  )
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b2", "t2"),
    rank     = c(1L, 2L, 1L, 2L)
  )
  mf <- match_features(matches, es,
                       base = bs, id = "id",
                       target = tg, target_id = "id",
                       include_string_sim = FALSE)
  cn <- names(mf@features)
  expect_false(any(grepl("^sim_", cn)))
  expect_true("cosine_sim" %in% cn)
  expect_true("embedding_norm_s" %in% cn)
  expect_true("embedding_norm_f" %in% cn)
})

test_that("string-sim columns appear on dedup match results (token)", {
  skip_if_not_installed("stringdist")
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")
  expect_true("sim_sf_name" %in% names(mf@features))
  expect_true("sim_fs_name" %in% names(mf@features))
  # (d, a) pair: "john doe" vs "john smith" → finite, in (0,1)
  row_da <- mf@features[searched == "d" & found == "a"]
  expect_true(row_da$sim_sf_name > 0 & row_da$sim_sf_name < 1)
})

test_that("Embedding_Strategy dedup path emits cosine_sim and norms from base", {
  skip_if_not_installed("tidyllm")
  local_mocked_bindings(embed = fake_embed_dt, .package = "tidyllm")

  base <- data.table(
    id   = c("a", "b", "c"),
    name = c("alpha beta", "alpha beta", "gamma delta")
  )
  es <- embedding_strategy(
    columns         = "name",
    embedding_model = list(.model = "fake"),
    threshold       = 0.0,
    batch_size      = 100L
  )
  # Hand-baked dedup matches table (no source column; dedup shape)
  matches <- data.table(
    duplicate_group = c(1L, 1L, 1L),
    id              = c("a", "b", "c"),
    score           = c(1.0, 0.9, 0.6),
    rank            = c(1L, 2L, 3L)
  )
  mf <- match_features(matches, es, base = base, id = "id")
  ft <- mf@features

  expect_true(all(c("cosine_sim", "embedding_norm_s", "embedding_norm_f")
                  %in% names(ft)))
  expect_equal(ft$cosine_sim, ft$score)
  # Both norms drawn from base; finite and positive
  expect_true(all(is.finite(ft$embedding_norm_s)))
  expect_true(all(is.finite(ft$embedding_norm_f)))
  expect_true(all(ft$embedding_norm_s > 0))
  expect_true(all(ft$embedding_norm_f > 0))
})

test_that("Embedding_Strategy without base/target still returns core+cosine", {
  # Backward compat with M2 reduced-schema test: works without base/target,
  # but norms collapse to NA and string-sim is silently skipped.
  es <- embedding_strategy(
    columns         = "name",
    embedding_model = "openai_embedding_model",
    threshold       = 0.5
  )
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b2", "t2"),
    rank     = c(1L, 2L, 1L, 2L)
  )
  mf <- match_features(matches, es)
  cn <- names(mf@features)
  expect_true("cosine_sim" %in% cn)
  expect_equal(mf@features$cosine_sim, mf@features$score)
  expect_true(all(is.na(mf@features$embedding_norm_s)))
  expect_true(all(is.na(mf@features$embedding_norm_f)))
  # string-sim skipped because base/target are absent
  expect_false(any(grepl("^sim_", cn)))
})


# ---------- multi-stage stage one-hots -------------------------------

test_that("stage one-hot dummies are emitted with >1 stage level", {
  matches <- data.table(
    match_id = c(1L, 1L, 2L, 2L),
    score    = c(0.9, 0.9, 0.7, 0.7),
    source   = c("base", "target", "base", "target"),
    id       = c("b1", "t1", "b1", "t2"),
    rank     = c(1L, 2L, 1L, 2L),
    stage    = c("phase_a", "phase_a", "phase_b", "phase_b")
  )
  bs <- data.table(id = c("b1"), name = c("john smith"))
  tg <- data.table(id = c("t1", "t2"), name = c("john smith", "john s"))
  s  <- search_strategy(name ~ word_tokens(), threshold = 0.1)

  mf <- match_features(matches, s, base = bs, id = "id",
                       target = tg, target_id = "id")

  stage_cols <- grep("^stage_", names(mf@features), value = TRUE)
  expect_setequal(stage_cols, c("stage_phase_a", "stage_phase_b"))
  # mutual exclusivity: exactly one 1 per row across stage one-hots
  rowsums <- rowSums(as.matrix(mf@features[, ..stage_cols]))
  expect_true(all(rowsums == 1L))
})


# ---------- backend parity: tibble / data.frame ----------------------

test_that("tibble and data.frame inputs defer to the data.table backend", {
  base_dt <- make_dedup_fixture()
  s       <- simple_dedup_strategy()
  dups_dt <- detect_duplicates(base_dt, "id", s)

  mf_dt <- match_features(dups_dt, s, base = base_dt, id = "id")

  if (requireNamespace("tibble", quietly = TRUE)) {
    base_tbl <- tibble::as_tibble(base_dt)
    dups_tbl <- tibble::as_tibble(dups_dt)
    mf_tbl   <- match_features(dups_tbl, s, base = base_tbl, id = "id")
    expect_equal(
      mf_dt@features[order(searched, found)],
      data.table::as.data.table(mf_tbl@features)[order(searched, found)],
      ignore.attr = TRUE
    )
  }

  base_df <- as.data.frame(base_dt)
  dups_df <- as.data.frame(dups_dt)
  mf_df   <- match_features(dups_df, s, base = base_df, id = "id")
  expect_equal(
    mf_dt@features[order(searched, found)],
    data.table::as.data.table(mf_df@features)[order(searched, found)],
    ignore.attr = TRUE
  )
})


# ---------- coercion methods -----------------------------------------

test_that("as.data.table / as.data.frame coerce to wide feature table", {
  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups <- detect_duplicates(base, "id", s)
  mf   <- match_features(dups, s, base = base, id = "id")

  dt <- as.data.table(mf)
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), nrow(mf@features))

  df <- as.data.frame(mf)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), nrow(mf@features))
})


# ---------- DuckDB parity --------------------------------------------

test_that("DuckDB matches are collected and produce identical features", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  base <- make_dedup_fixture()
  s    <- simple_dedup_strategy()
  dups_dt <- detect_duplicates(base, "id", s)

  mf_dt <- match_features(dups_dt, s, base = base, id = "id")

  con <- local_duckdb_con()
  DBI::dbWriteTable(con, "dups", as.data.frame(dups_dt))
  DBI::dbWriteTable(con, "base_tbl", as.data.frame(base))
  dups_duck <- dplyr::tbl(con, "dups")
  base_duck <- dplyr::tbl(con, "base_tbl")

  mf_duck <- match_features(dups_duck, s, base = base_duck, id = "id")

  expect_equal(names(mf_duck@features), names(mf_dt@features))
  setorder(mf_dt@features,   searched, found)
  setorder(mf_duck@features, searched, found)
  expect_equal(mf_dt@features, mf_duck@features, ignore.attr = TRUE)
})
