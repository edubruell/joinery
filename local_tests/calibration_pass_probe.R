# =============================================================================
# local_tests/calibration_pass_probe.R
# -----------------------------------------------------------------------------
# A real calibration pass over the shipped workshop example data, run BEFORE
# writing the calibration article. Goals:
#   1. Confirm the full search result has multi-candidate "blocks" (>1 register
#      candidate per listing) so the block-structure features (cnt/icnt/ipos)
#      are not degenerate.
#   2. Confirm match_features() actually produces the discriminating feature
#      families (the m_/f_/s_ aIP sets, sim_*, block stats) on this data.
#   3. Run fit_filter -> apply_filter -> calibrate and report per-class
#      confusion, Brier, log-loss at the Youden threshold AND at a
#      recall-favouring 0.30 override.
#   4. Cross-tab which planted FP archetypes (gen_tier) the filter catches.
#
# RUN: Rscript local_tests/calibration_pass_probe.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
})

set.seed(42)

data(workshop_register)
data(workshop_listings)
data(match_labels_example)

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

cat("================ 1. BLOCK STRUCTURE (full search result) ============\n")
# candidates per searched listing: source=="base" is the listing header,
# source=="target" rows are the register candidates. Group by the searched id.
# match_id is per-pair; the searched listing is the block key.
base_rows <- m[source == "base"]
# every match_id has one base row carrying the listing id
cand_per_listing <- m[source == "target", .N, by = .(listing = base_rows[match(match_id, base_rows$match_id), id])]
cat("candidates per listing (distribution of N):\n")
print(table(cand_per_listing$N))
cat(sprintf("listings with >1 candidate: %d of %d (%.1f%%)\n",
            sum(cand_per_listing$N > 1), nrow(cand_per_listing),
            100 * mean(cand_per_listing$N > 1)))

cat("\n================ 2. match_features() feature families ===============\n")
feats <- match_features(
  m, strat,
  base      = workshop_listings, id = "listing_id",
  target    = workshop_register, target_id = "reg_no"
)
ft <- as.data.table(feats@features)
cat(sprintf("feature rows: %d | cols: %d\n", nrow(ft), ncol(ft)))
cat("column names:\n"); print(names(ft))
cat("\nblock-stat columns (cnt/icnt/ipos) summary:\n")
for (cc in intersect(c("cnt","icnt","ipos","scnt","rcnt"), names(ft))) {
  cat(sprintf("  %-6s: ", cc)); print(summary(ft[[cc]]))
}

cat("\n================ 3. fit_filter -> apply_filter -> calibrate ==========\n")
fm <- fit_filter(feats, match_labels_example, model = "logistic")
cat(sprintf("training_n: %d\n", fm@training_n))

cm_youden <- apply_filter(feats, fm)                 # Youden's J threshold
cm_recall <- apply_filter(feats, fm, threshold = 0.30) # recall-favouring

cat(sprintf("Youden threshold:  %.3f\n", cm_youden@threshold))
cat(sprintf("Recall threshold:  %.3f\n", cm_recall@threshold))

# Per-class confusion needs labels. Join predictions back to the labelled set.
lab <- as.data.table(match_labels_example)[source == "target", .(match_id, found = id, equal)]
score_one <- function(cm, name) {
  pr <- as.data.table(cm@matches)
  # the enriched features table carries match_id, found, tp_prob, predicted_tp
  key <- intersect(c("match_id","found"), names(pr))
  j <- merge(pr[, c(key, "predicted_tp", "tp_prob"), with = FALSE], lab,
             by = c("match_id","found"))
  tab <- table(predicted = j$predicted_tp, actual = j$equal)
  cat(sprintf("\n-- %s (n labelled = %d) --\n", name, nrow(j)))
  print(tab)
  tp <- sum(j$predicted_tp == 1 & j$equal == 1)
  fp <- sum(j$predicted_tp == 1 & j$equal == 0)
  fn <- sum(j$predicted_tp == 0 & j$equal == 1)
  cat(sprintf("precision: %.3f | recall: %.3f\n",
              tp / (tp + fp), tp / (tp + fn)))
  j
}
j_y <- score_one(cm_youden, "YOUDEN")
j_r <- score_one(cm_recall, "RECALL-0.30")

cat("\n================ 4. calibrate() diagnostics =========================\n")
cal <- calibrate(cm_youden)
cat(sprintf("Brier: %.4f | log-loss: %.4f\n", cal@brier, cal@log_loss))
cat("reliability table:\n"); print(as.data.table(cal@reliability))

cat("\n================ 5. FP archetypes caught (gen_tier) =================\n")
# of the labelled false positives, which gen_tiers does the filter reject?
tgt <- as.data.table(match_labels_example)[source == "target"]
fp <- merge(j_y, tgt[, .(match_id, found = id, gen_tier)], by = c("match_id","found"))
fp_neg <- fp[equal == 0]
cat("false positives by gen_tier, and how many the Youden filter REJECTS:\n")
print(fp_neg[, .(n = .N, rejected = sum(predicted_tp == 0),
                 caught_pct = round(100 * mean(predicted_tp == 0), 1)),
             by = gen_tier][order(-n)])

cat("\nDONE.\n")
