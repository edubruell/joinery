# Acceptance fixture for the 1.1 subword theme: a German chamber register and
# a trade directory that spell the same workshops differently. The register
# writes each trade as one compound word, the directory splits the compound
# apart and reorders it, and a scanner has misread some directory lines. The
# question the fixture answers is how many of the known pairs each tokenizer
# recovers at the same threshold.

skip_if_not_installed("sentencepiece")

# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------

# Twenty workshops whose two spellings share at most the proprietor's name.
compound_pairs <- function() {
  register <- c(
    "Holzbaugesellschaft Wagner mbH",
    "Moebelwerkstatt Brandt",
    "Tischlereibetrieb Kaufmann",
    "Fensterbaugesellschaft Lehmann",
    "Treppenbauwerkstatt Ritter",
    "Parkettlegerbetrieb Sommer",
    "Innenausbaugesellschaft Naumann",
    "Kuechenmoebelfabrik Wendt",
    "Holzhandelsgesellschaft Gerlach",
    "Schreinereibetrieb Hoffmann",
    "Dachstuhlbaugesellschaft Kolbe",
    "Bautischlereibetrieb Reuter",
    "Ladenbaugesellschaft Piotrowski",
    "Gartenmoebelwerkstatt Ebeling",
    "Massivholzmoebelfabrik Stein",
    "Trockenbaugesellschaft Amrein",
    "Furnierhandelsgesellschaft Zorn",
    "Bootsbauwerkstatt Marquardt",
    "Saegewerksgesellschaft Uhlig",
    "Restaurierungswerkstatt Thiele"
  )
  directory <- c(
    "Wagner Holzbau KG",
    "Werkstatt fuer Moebel Brandt",
    "Kaufmann Tischlerei",
    "Lehmann Fensterbau GmbH",
    "Ritter Treppenbau Werkstatt",
    "Sommer Parkettleger",
    "Naumann Innenausbau GmbH",
    "Wendt Kuechenmoebel Fabrik",
    "Gerlach Holzhandel",
    "Hoffmann Schreinerei",
    "Kolbe Dachstuhlbau",
    "Reuter Bautischlerei",
    "Piotrowski Ladenbau GmbH",
    "Ebeling Gartenmoebel Werkstatt",
    "Stein Massivholzmoebel",
    "Amrein Trockenbau GmbH",
    "Zorn Furnierhandel",
    "Marquardt Bootsbau",
    "Uhlig Saegewerk",
    "Thiele Restaurierung"
  )
  # Scanner noise on every third directory line: rn read as m, O as zero,
  # a double l as two ones.
  noisy <- vapply(seq_along(directory), function(i) {
    x <- directory[i]
    if (i %% 3 != 0) return(x)
    x <- sub("rn", "m", x)
    x <- sub("O", "0", x)
    sub("ll", "11", x)
  }, character(1))

  data.frame(
    entity = sprintf("E%02d", seq_along(register)),
    register = register,
    directory = noisy,
    stringsAsFactors = FALSE
  )
}

# Ten workshops the directory copies almost verbatim, so that whole words
# already match. They are the control: a subword run must not lose them.
plain_pairs <- function() {
  register <- c(
    "Tischlerei Baumgartner", "Schreinerei Kowalczyk",
    "Zimmerei Oberlaender", "Drechslerei Feldmann",
    "Boettcherei Hanselmann", "Parkettstudio Vranken",
    "Holzwerkstatt Dallmann", "Moebelbau Steinhauser",
    "Treppenstudio Wendland", "Holzdesign Kretschmar"
  )
  data.frame(
    entity = sprintf("S%02d", seq_along(register)),
    register = register,
    directory = paste(register, "GmbH"),
    stringsAsFactors = FALSE
  )
}

other_trades <- function() {
  c(
    "Malerbetrieb Kessler", "Dachdeckerei Vogt", "Elektrotechnik Sander",
    "Sanitaertechnik Brunner", "Metallbau Hartwig", "Steinmetzbetrieb Gruber",
    "Glaserei Wiedemann", "Polsterei Nehring", "Zimmerei Achenbach",
    "Schlosserei Berndt"
  )
}

# One table to deduplicate: register rows prefixed R, directory rows D, and
# ten unrelated workshops that must not join anything.
acceptance_fixture <- function() {
  pairs <- rbind(compound_pairs(), plain_pairs())
  singles <- other_trades()
  rbind(
    data.frame(id = paste0("R", pairs$entity), firm = pairs$register),
    data.frame(id = paste0("D", pairs$entity), firm = pairs$directory),
    data.frame(id = sprintf("X%02d", seq_along(singles)), firm = singles),
    stringsAsFactors = FALSE
  )
}

# The vocabulary is fitted on the register side plus the trade words on their
# own, which is the workflow the article describes: fit on the file you keep,
# apply to both files.
acceptance_corpus <- function() {
  pairs <- rbind(compound_pairs(), plain_pairs())
  normalize_text(c(
    pairs$register, other_trades(),
    "Holzbau Wagner", "Moebel Brandt", "Tischlerei Kaufmann",
    "Fensterbau Lehmann", "Treppenbau Ritter", "Parkettleger Sommer",
    "Innenausbau Naumann", "Kuechenmoebel Wendt", "Holzhandel Gerlach",
    "Schreinerei Hoffmann", "Dachstuhlbau Kolbe", "Bautischlerei Reuter",
    "Ladenbau Piotrowski", "Gartenmoebel Ebeling", "Massivholzmoebel Stein",
    "Trockenbau Amrein", "Furnierhandel Zorn", "Bootsbau Marquardt",
    "Saegewerk Uhlig", "Restaurierung Thiele",
    "Werkstatt", "Gesellschaft", "Betrieb", "Fabrik", "Handel", "GmbH", "KG"
  ))
}

# Share of the 30 known pairs whose two rows land in one duplicate group.
pair_recall <- function(dups, entities) {
  if (nrow(dups) == 0L) return(0)
  groups <- split(dups$id, dups$duplicate_group)
  mean(vapply(entities, function(e) {
    wanted <- c(paste0("R", e), paste0("D", e))
    any(vapply(groups, function(ids) all(wanted %in% ids), logical(1)))
  }, logical(1)))
}

# Share of duplicate groups that hold one workshop only. Anything below 1
# means the tokenizer joined two different workshops.
group_purity <- function(dups) {
  if (nrow(dups) == 0L) return(NA_real_)
  groups <- split(sub("^[RDX]", "", dups$id), dups$duplicate_group)
  mean(vapply(groups, function(e) length(unique(e)) == 1L, logical(1)))
}

all_entities <- function() c(compound_pairs()$entity, plain_pairs()$entity)

word_strategy <- function() {
  search_strategy(firm ~ normalize_text() + word_tokens(), threshold = 0.5)
}

subword_strategy <- function(sw) {
  search_strategy(
    firm ~ normalize_text() + subword_tokens(sw) + drop_short_tokens(min_nchar = 3),
    threshold = 0.5
  )
}

# ---------------------------------------------------------------------------
# The recall claim
# ---------------------------------------------------------------------------

test_that("word tokens find only the pairs that already share whole words", {
  dups <- detect_duplicates(acceptance_fixture(), id = "id",
                            strategy = word_strategy())

  # 12 of 30 pairs: the ten near-verbatim ones plus two compounds whose
  # proprietor name alone clears the threshold.
  expect_equal(pair_recall(dups, all_entities()), 0.4)
  expect_equal(group_purity(dups), 1)
})

test_that("a subword vocabulary recovers every pair without joining workshops", {
  sw <- find_subwords(acceptance_corpus(), vocab_size = 340)
  dups <- detect_duplicates(acceptance_fixture(), id = "id",
                            strategy = subword_strategy(sw))

  word_recall <- pair_recall(
    detect_duplicates(acceptance_fixture(), id = "id", strategy = word_strategy()),
    all_entities()
  )
  subword_recall <- pair_recall(dups, all_entities())

  # Observed on sentencepiece 0.2.5: word 0.400, subword 1.000, delta 0.600.
  expect_gte(subword_recall, 0.9)
  expect_gte(subword_recall - word_recall, 0.5)
  expect_equal(group_purity(dups), 1)

  # Every pair, one group of two rows, and the ten unrelated workshops left out.
  expect_equal(nrow(dups), 60L)
  expect_false(any(grepl("^X", dups$id)))
})

test_that("the subword run keeps the pairs word tokens already had", {
  sw <- find_subwords(acceptance_corpus(), vocab_size = 340)
  plain <- plain_pairs()$entity

  word_dups <- detect_duplicates(acceptance_fixture(), id = "id",
                                 strategy = word_strategy())
  sub_dups <- detect_duplicates(acceptance_fixture(), id = "id",
                                strategy = subword_strategy(sw))

  expect_equal(pair_recall(word_dups, plain), 1)
  expect_equal(pair_recall(sub_dups, plain), 1)
})

test_that("too small a vocabulary joins workshops that do not belong together", {
  # The tuning lesson for the article: short pieces are shared by everything,
  # so a cramped vocabulary buys recall with false groups.
  sw <- find_subwords(acceptance_corpus(), vocab_size = 200)
  dups <- detect_duplicates(acceptance_fixture(), id = "id",
                            strategy = subword_strategy(sw))

  expect_lt(group_purity(dups), 1)
})
