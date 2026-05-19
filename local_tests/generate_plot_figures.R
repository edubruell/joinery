# Generate pkgdown/figures/ PNGs for all 14 M7 diagnostic plot functions.
# Run from the package root:
#   Rscript local_tests/generate_plot_figures.R

devtools::load_all()
library(data.table)

dir.create("pkgdown/figures", showWarnings = FALSE, recursive = TRUE)

fig <- function(name, width = 800, height = 500) {
  grDevices::png(file.path("pkgdown/figures", paste0(name, ".png")),
                 width = width, height = height, res = 96)
}

# ---------------------------------------------------------------------------
# Rich realistic fixtures
# ---------------------------------------------------------------------------

set.seed(42)

# --- Strategy_Audit fixture: 60 company-like records, two text columns + region
n <- 60L
first_names <- c(
  "Acme", "Global", "National", "Premier", "Advanced", "United",
  "Allied", "Federal", "Pacific", "Atlantic", "Continental", "Metro"
)
company_types <- c(
  "Corp", "Inc", "LLC", "GmbH", "AG", "KG", "SE",
  "Solutions", "Group", "Partners", "Services", "Systems"
)
cities <- c("Berlin", "Munich", "Hamburg", "Frankfurt", "Cologne",
            "Stuttgart", "Dusseldorf", "Leipzig", "Dresden", "Bremen")

base_60 <- data.table(
  id      = paste0("b", seq_len(n)),
  company = paste(
    sample(first_names, n, replace = TRUE),
    sample(c("Tech", "Finance", "Logistics", "Health", "Media", "Energy",
             "Consulting", "Digital", "Auto", "Retail"), n, replace = TRUE),
    sample(company_types, n, replace = TRUE)
  ),
  city    = sample(cities, n, replace = TRUE, prob = c(0.35, 0.20, 0.15, 0.10, 0.08, 0.04, 0.03, 0.02, 0.02, 0.01))
)

# Target for vocab overlap: 30 records sharing ~40% of company tokens
target_30 <- data.table(
  id      = paste0("t", seq_len(30L)),
  company = paste(
    sample(c(first_names, "New", "Old", "Modern", "Classic"), 30L, replace = TRUE),
    sample(c("Tech", "Finance", "Logistics", "Health", "Media", "Building",
             "Transport", "Trade", "Law", "Science"), 30L, replace = TRUE),
    sample(company_types, 30L, replace = TRUE)
  ),
  city    = sample(cities, 30L, replace = TRUE)
)

strat_no_block <- search_strategy(
  company ~ normalize_text() + word_tokens(min_nchar = 3L),
  city    ~ normalize_text() + word_tokens(),
  threshold = 0.6
)
strat_block <- search_strategy(
  company ~ normalize_text() + word_tokens(min_nchar = 3L),
  city    ~ normalize_text() + word_tokens(),
  block_by  = "city",
  threshold = 0.6
)

sa         <- audit_strategy(base_60, "id", strat_no_block)
sa_block   <- audit_strategy(base_60, "id", strat_block)
sa_tgt     <- audit_strategy(base_60, "id", strat_no_block, target = target_30)

# --- Duplicate-match fixture: ~40 pairs with realistic score distribution
# Introduce duplicate groups via near-identical records
n_base <- 80L
first_names2 <- c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta",
                  "Eta", "Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu",
                  "Xi", "Omicron", "Pi", "Rho", "Sigma", "Tau", "Upsilon")
dup_names_fixed <- c(
  # Groups of 2-4 near-duplicates (26 records)
  "Acme Corp GmbH", "Acme Corporation GmbH", "Acme Corp",
  "Global Tech Inc", "Global Technology Inc", "Global Tech",
  "Premier Finance AG", "Premier Finance AG SE",
  "National Solutions LLC", "National Solutions",
  "Advanced Systems Corp", "Advanced System Corp",
  "United Services Group", "United Services",
  "Federal Consulting KG", "Federal Consulting",
  "Pacific Digital GmbH", "Pacific Digital AG",
  "Atlantic Energy Inc", "Atlantic Energy",
  "Metro Health Systems", "Metro Health System",
  "Continental Auto AG", "Continental Automotive AG",
  "Allied Retail GmbH", "Allied Retail AG"           # 2 more to reach 26
)
# Non-duplicates (n_base - 26 = 54 records)
dup_non_dup <- paste(sample(first_names2, n_base - 26L, replace = TRUE),
                     sample(company_types, n_base - 26L, replace = TRUE))
dup_base <- data.table(
  id      = paste0("r", seq_len(n_base)),
  company = c(dup_names_fixed, dup_non_dup),
  city    = sample(cities, n_base, replace = TRUE)
)
strat_dup <- search_strategy(
  company ~ normalize_text() + word_tokens(min_nchar = 3L),
  threshold = 0.5
)
dup_matches <- detect_duplicates(dup_base, "id", strat_dup)
dup_ov <- summarise_matches(dup_matches, base = dup_base)

# --- Candidate-match fixture: clear bimodal score distribution
n_cand_base   <- 50L
n_cand_target <- 50L
cand_base <- data.table(
  id      = paste0("b", seq_len(n_cand_base)),
  company = c(
    # Strong matches (will pair well with target)
    "Acme Corp GmbH", "Global Tech Inc", "Premier Finance AG",
    "National Solutions LLC", "Advanced Systems Corp",
    "United Services Group", "Federal Consulting KG",
    "Pacific Digital GmbH", "Atlantic Energy Inc",
    "Metro Health Systems",
    # Weaker partial matches
    paste(sample(first_names, n_cand_base - 10L, replace = TRUE),
          sample(company_types, n_cand_base - 10L, replace = TRUE))
  ),
  city = sample(cities, n_cand_base, replace = TRUE)
)
cand_target <- data.table(
  id      = paste0("t", seq_len(n_cand_target)),
  company = c(
    "Acme Corporation GmbH", "Global Technology Inc", "Premier Finance AG SE",
    "National Solutions GmbH", "Advanced Systems Corporation",
    "United Services AG", "Federal Consulting AG KG",
    "Pacific Digital AG", "Atlantic Energy GmbH",
    "Metro Health System Corp",
    paste(sample(first_names, n_cand_target - 10L, replace = TRUE),
          sample(company_types, n_cand_target - 10L, replace = TRUE))
  ),
  city = sample(cities, n_cand_target, replace = TRUE)
)
strat_cand <- search_strategy(
  company ~ normalize_text() + word_tokens(min_nchar = 3L),
  threshold = 0.4
)
cand_matches <- search_candidates(cand_base, cand_target, "id", "id", strat_cand)
cand_ov      <- summarise_matches(cand_matches, base = cand_base, target = cand_target)

# --- Match_Explanation fixture: pair with multiple tokens from multiple columns
expl_base <- data.table(
  id      = c("r1", "r2", "r3", "r4"),
  company = c("Acme Tech Solutions Corp",
              "Acme Technology Solutions Corporation",
              "Global Finance Group AG",
              "Random Unrelated Business"),
  city    = c("Berlin", "Berlin", "Munich", "Hamburg")
)
strat_expl <- search_strategy(
  company ~ normalize_text() + word_tokens(min_nchar = 3L),
  city    ~ normalize_text() + word_tokens(),
  threshold = 0.3
)
expl_matches <- detect_duplicates(expl_base, "id", strat_expl)
expl_obj <- explain_match(
  expl_matches, strat_expl, base = expl_base, id = "id",
  match_id = expl_matches$duplicate_group[1L]
)

# --- Stage comparison fixture: three stages with realistic coverage
n_ms <- 100L
ms_base   <- data.table(id = paste0("b", seq_len(n_ms)),
                        company = paste(sample(first_names, n_ms, replace=TRUE),
                                        sample(company_types, n_ms, replace=TRUE)))
ms_target <- data.table(id = paste0("t", seq_len(n_ms)),
                        company = paste(sample(first_names, n_ms, replace=TRUE),
                                        sample(company_types, n_ms, replace=TRUE)))

# Manually build multi-stage matches with realistic coverage
# Stage 1 (token): high-confidence matches, 30 pairs
s1_base_ids <- paste0("b", 1:30)
s1_tgt_ids  <- paste0("t", 1:30)
s1_scores   <- round(runif(30L, 0.80, 0.98), 3L)

# Stage 2 (fuzzy): medium-confidence, 20 new pairs
s2_base_ids <- paste0("b", 31:50)
s2_tgt_ids  <- paste0("t", 31:50)
s2_scores   <- round(runif(20L, 0.55, 0.79), 3L)

# Stage 3 (blocking fallback): low-confidence, 10 new pairs
s3_base_ids <- paste0("b", 51:60)
s3_tgt_ids  <- paste0("t", 51:60)
s3_scores   <- round(runif(10L, 0.40, 0.54), 3L)

make_stage_rows <- function(base_ids, tgt_ids, scores, stage_name) {
  n_pairs <- length(base_ids)
  data.table(
    match_id = seq(1L, n_pairs) + switch(stage_name, token = 0L, fuzzy = 30L, fallback = 50L),
    score    = rep(scores, each = 2L),
    stage    = stage_name,
    source   = rep(c("base", "target"), times = n_pairs),
    id       = as.vector(rbind(base_ids, tgt_ids)),
    rank     = 1L
  )
}

ms_matches <- rbind(
  make_stage_rows(s1_base_ids, s1_tgt_ids, s1_scores, "token"),
  make_stage_rows(s2_base_ids, s2_tgt_ids, s2_scores, "fuzzy"),
  make_stage_rows(s3_base_ids, s3_tgt_ids, s3_scores, "fallback")
)
stage_obj <- compare_stages(ms_matches, base = ms_base, target = ms_target)

# Sample object
smp_obj <- sample_matches(dup_matches, mode = "borderline",
                          n = 20L, threshold = 0.65)

# ---------------------------------------------------------------------------
# Generate figures
# ---------------------------------------------------------------------------

fig("rarity_histogram");        rarity_histogram(sa);           dev.off()
fig("token_frequency_plot");    token_frequency_plot(sa);       dev.off()
fig("block_size_plot");         block_size_plot(sa_block);      dev.off()
fig("vocab_overlap_plot");      vocab_overlap_plot(sa_tgt);     dev.off()
fig("score_histogram");         score_histogram(dup_ov, threshold = 0.5); dev.off()
fig("score_density");           score_density(dup_ov, threshold = 0.5);   dev.off()
fig("coverage_plot");           coverage_plot(cand_ov);         dev.off()
fig("cluster_size_plot");       cluster_size_plot(dup_ov);      dev.off()
fig("ambiguity_plot");          ambiguity_plot(cand_ov);        dev.off()
fig("top_gap_density");         top_gap_density(cand_ov);       dev.off()
fig("contribution_plot");       contribution_plot(expl_obj);    dev.off()
fig("token_contribution_plot"); token_contribution_plot(expl_obj); dev.off()
fig("stage_coverage_plot");     stage_coverage_plot(stage_obj); dev.off()
fig("stage_score_plot");        stage_score_plot(stage_obj);    dev.off()

cat("Done — figures written to pkgdown/figures/\n")
