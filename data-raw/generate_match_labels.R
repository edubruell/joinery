# =============================================================================
# data-raw/generate_match_labels.R
# -----------------------------------------------------------------------------
# P1 example data (notes/v09/06). A small fixed table of LABELLED candidate pairs
# for the calibration article and the calibration-verb @examples.
#
# It is a frozen search_candidates() run over the shipped workshop pair, with the
# `equal` column filled from ground truth (actual_link). It is sourced from the
# workshop data on purpose: the planted homonym tiers and the hub/category traps
# give the calibration article real, hard false positives to filter, which the
# person data has not got.
#
# The output matches what import_labels() returns (the candidate matches schema
# with a fully populated `equal` column), so it drops straight into
# fit_filter() / calibrate_matches() with no manual labelling step.
#
# base   = workshop_listings  (the messy directory side, carries actual_link)
# target = workshop_register  (the guild-roll corpus we search against)
#   searched = listing_id, found = reg_no.
#   equal = 1 when the found reg_no IS the listing's true actual_link, else 0.
#
# SEEDED + OFFLINE. A fixed threshold and a fixed downsample seed make it
# regenerable.
#
# RUN:  Rscript data-raw/generate_match_labels.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
})

set.seed(42)

load("data/workshop_register.rda")
load("data/workshop_listings.rda")

# A deliberately loose threshold so each block yields both true matches and
# false positives (homonyms, shared-block look-alikes) for the filter to learn.
strat <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.30
)

m <- search_candidates(
  workshop_listings, workshop_register,
  base_id   = "listing_id",
  target_id = "reg_no",
  strategy  = strat
)
m <- as.data.table(m)

# ---------------------------------------------------------------------------
# Label: equal = does the found reg_no equal the searched listing's true link?
# ---------------------------------------------------------------------------
base_link <- m[source == "base", .(match_id, true_link = actual_link)]
m <- merge(m, base_link, by = "match_id", all.x = TRUE, sort = FALSE)

m[, equal := NA_integer_]
# target (candidate) rows carry the real label
m[source == "target",
  equal := as.integer(!is.na(true_link) & id == true_link)]
# base (header) rows get the block default = whether a true match is reachable
blk_default <- m[source == "target", .(def = as.integer(any(equal == 1L))),
                 by = match_id]
m <- merge(m, blk_default, by = "match_id", all.x = TRUE, sort = FALSE)
m[source == "base", equal := fifelse(is.na(def), 0L, def)]
m[, def := NULL]

# ---------------------------------------------------------------------------
# Keep it small: sample blocks, not rows, so each kept match_id stays a whole
# labellable block (base header + its candidates). Keep every block that holds a
# true positive plus a sample of pure-false-positive blocks, for class balance.
# ---------------------------------------------------------------------------
tp_blocks <- m[source == "target" & equal == 1L, unique(match_id)]
fp_only   <- setdiff(m[source == "target", unique(match_id)], tp_blocks)
keep_fp   <- sample(fp_only, min(length(fp_only), 220L))
keep_ids  <- c(tp_blocks, keep_fp)

labels <- m[match_id %in% keep_ids]

# Trim to a compact, article-friendly column set (still a valid candidate-labels
# table: match_id / source / id / rank / equal are all present).
keep_cols <- intersect(
  c("match_id", "score", "source", "id", "workshop", "proprietor", "trade",
    "postcode_area", "gen_tier", "actual_link", "rank", "equal"),
  names(labels)
)
match_labels_example <- as.data.frame(labels[, ..keep_cols][order(match_id, source, -score)])

# =============================================================================
# Sanity checks
# =============================================================================
cat("== sanity ==\n")
tgt <- match_labels_example[match_labels_example$source == "target", ]
cat(sprintf("blocks (match_id)            : %d\n", length(unique(match_labels_example$match_id))))
cat(sprintf("candidate (target) rows      : %d\n", nrow(tgt)))
cat(sprintf("  positives (equal == 1)     : %d\n", sum(tgt$equal == 1L)))
cat(sprintf("  negatives (equal == 0)     : %d\n", sum(tgt$equal == 0L)))
cat(sprintf("false positives by gen_tier  :\n"))
print(table(tgt$gen_tier[tgt$equal == 0L]))
stopifnot(all(match_labels_example$equal %in% c(0L, 1L)))
stopifnot(!anyNA(match_labels_example$equal))
# every block has exactly one base header row
hdr <- tapply(match_labels_example$source == "base", match_labels_example$match_id, sum)
stopifnot(all(hdr == 1L))

# =============================================================================
# Save
# =============================================================================
cat(sprintf("\nrows: %d | cols: %s\n", nrow(match_labels_example),
            paste(names(match_labels_example), collapse = ", ")))
usethis::use_data(match_labels_example, overwrite = TRUE)
cat("\nDone. match_labels_example written to data/.\n")
