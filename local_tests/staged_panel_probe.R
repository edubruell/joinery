# =============================================================================
# local_tests/staged_panel_probe.R
# -----------------------------------------------------------------------------
# A real cross-year staged-search pass over workshop_panel, run BEFORE writing
# the multi-year/multi-source article. Goals:
#   1. Confirm multi_stage_search(self=TRUE, source_by="year") returns the entity
#      grouping + ledger documented, on the shipped panel.
#   2. Measure cross-year entity recall vs true_entity for: a single fuzzy pass,
#      a staged exact->fuzzy run, and a staged run with a mover stage.
#   3. Show collapse="rep" bridging a name_drift trajectory that "none" splits.
#   4. Read entry/exit (first/last covered year, span) off the entity grouping
#      and compare to ground truth.
#   5. Run compare_stages.
#
# RUN: Rscript local_tests/staged_panel_probe.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
})
set.seed(42)
data(workshop_panel)
P <- as.data.table(workshop_panel)
cat(sprintf("panel: %d rows | %d entities | years %s\n",
            nrow(P), uniqueN(P$true_entity),
            paste(range(P$year), collapse = "-")))
cat("change_tier:\n"); print(table(P$change_tier))

# ---- scoring helper: pairwise recall/precision vs true_entity ---------------
# recall  = share of same-true-entity record pairs that share a predicted entity
# precision = share of same-predicted-entity pairs that share a true entity
pair_scores <- function(ent) {
  d <- merge(as.data.table(ent)[, .(record_id = id, pred = entity)],
             P[, .(record_id, truth = true_entity)], by = "record_id")
  same_true <- d[, .(combn_true = .N * (.N - 1) / 2), by = truth][, sum(combn_true)]
  same_pred <- d[, .(combn_pred = .N * (.N - 1) / 2), by = pred][, sum(combn_pred)]
  same_both <- d[, .N, by = .(truth, pred)][, sum(N * (N - 1) / 2)]
  list(recall = same_both / same_true, precision = same_both / same_pred,
       n_pred_entities = uniqueN(d$pred), n_true_entities = uniqueN(d$truth))
}
report <- function(tag, ent) {
  s <- pair_scores(ent)
  cat(sprintf("%-22s recall=%.3f precision=%.3f  pred_entities=%d (truth=%d)\n",
              tag, s$recall, s$precision, s$n_pred_entities, s$n_true_entities))
}

# ---- strategies -------------------------------------------------------------
exact <- exact_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by = c("postcode_area", "trade")
)
fuzzy <- search_strategy(
  workshop   ~ normalize_text() + word_tokens(min_nchar = 3),
  proprietor ~ normalize_text() + word_tokens(min_nchar = 2),
  block_by  = c("postcode_area", "trade"),
  threshold = 0.55
)
# mover stage: relocations change postcode_area, so block on a rare shared token
# of the name instead, with corpus-wide rarity so a distinctive stem reads strong
# anywhere.
mover <- search_strategy(
  workshop ~ normalize_text() + word_tokens(min_nchar = 3),
  block_by     = list(block_on_tokens("workshop", max_df = 50, min_nchar = 4),
                      "trade"),
  rarity_scope = "global",
  threshold    = 0.6
)

cat("\n================ 1. single fuzzy pass ===============================\n")
g1 <- multi_stage_search(P, P, "record_id", "record_id", list(fuzzy = fuzzy),
                         self = TRUE, source_by = "year")
cat("entity grouping cols:\n"); print(names(g1))
led <- attr(g1, "ledger"); cat("ledger cols:\n"); print(names(led))
report("single fuzzy", g1)

cat("\n================ 2. staged exact -> fuzzy ===========================\n")
g2 <- multi_stage_search(P, P, "record_id", "record_id",
                         list(exact = exact, fuzzy = fuzzy),
                         self = TRUE, source_by = "year", collapse = "rep")
report("exact->fuzzy", g2)

cat("\n================ 3. staged exact -> fuzzy -> mover ==================\n")
g3 <- multi_stage_search(P, P, "record_id", "record_id",
                         list(exact = exact, fuzzy = fuzzy, mover = mover),
                         self = TRUE, source_by = "year", collapse = "rep")
report("exact->fuzzy->mover", g3)
cat("stage that linked each record:\n"); print(table(as.data.table(g3)$stage))

cat("\n================ 4. collapse rep vs none (name_drift) ===============\n")
none <- multi_stage_search(P, P, "record_id", "record_id",
                           list(exact = exact, fuzzy = fuzzy),
                           self = TRUE, source_by = "year", collapse = "none")
report("collapse=none", none)
report("collapse=rep ", g2)

cat("\n================ 5. entry/exit read off the grouping ================\n")
# covered_sources = number of distinct years the entity spans. Per entity, get
# first/last year and span from the ledger? The grouping carries source per row
# (the year) and covered_sources count. Derive birth/exit per predicted entity.
ent3 <- as.data.table(g3)
yr <- merge(ent3[, .(record_id = id, entity)],
            P[, .(record_id, year)], by = "record_id")
span <- yr[, .(first = min(year), last = max(year),
               n_years = uniqueN(year)), by = entity]
cat("predicted trajectory-span distribution (n distinct years):\n")
print(table(span$n_years))
# ground-truth span for comparison
tspan <- P[, .(first = min(year), last = max(year), n_years = uniqueN(year)),
           by = true_entity]
cat("\ntrue trajectory-span distribution:\n"); print(table(tspan$n_years))
cat(sprintf("\nentries (births) after 2019 - predicted: %d | true: %d\n",
            span[first > 2019, .N], tspan[first > 2019, .N]))
cat(sprintf("exits (deaths) before 2023  - predicted: %d | true: %d\n",
            span[last < 2023, .N], tspan[last < 2023, .N]))

cat("\n================ 6. compare_stages ==================================\n")
cs <- compare_stages(g3, base = P, target = P)
print(cs)
cat("\nDONE.\n")
