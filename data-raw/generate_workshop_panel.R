# =============================================================================
# data-raw/generate_workshop_panel.R
# -----------------------------------------------------------------------------
# P1 example data (notes/v09/06). A small multi-year panel that extends the SAME
# woodworking-workshop universe as workshop_register / workshop_listings.
#
# SEEDED + OFFLINE. Draws a few hundred core workshops from the shipped
# workshop_register (so the entities are the frozen ones, not a fresh universe),
# gives each a year span over 2019-2023, and emits one messy directory-style row
# per (entity, year). Across years there is entry/exit, a few postcode_area moves
# (relocations), gradual name drift, and a phonetic-twin tail. A fixed set.seed()
# makes it fully regenerable.
#
# Emits one package dataset:
#   workshop_panel   pooled-long panel, one row per (workshop, year)
#
# GROUND TRUTH (documented evaluation-only columns):
#   workshop_panel$true_entity   the stable entity key (the register reg_no);
#                                 every year-row of one workshop shares it
#   workshop_panel$change_tier   which cross-year challenge the entity carries,
#                                 so an article can slice "just the movers"
#
# CHANGE TIERS (entity-level):
#   stable      light per-year noise only                  -> baseline cross-year
#   name_drift  a structural name change part-way through   -> fuzzy bridges it
#   mover       postcode_area changes from a switch year on  -> block_on_tokens
#   phonetic    stem swapped to a code-preserving twin       -> as_cologne + drop_short_tokens
#
# RUN:  Rscript data-raw/generate_workshop_panel.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(42)

stopifnot(file.exists("data/workshop_register.rda"))
load("data/workshop_register.rda")

YEARS         <- 2019:2023
N_ENTITIES    <- 320L                                  # distinct workshops
TIER_WEIGHTS  <- c(stable = 0.55, name_drift = 0.22,   # entity-level mix
                   mover = 0.15, phonetic = 0.08)

# Area -> representative town, taken from the register itself (one universe).
area_town <- workshop_register |>
  distinct(postcode_area, town) |>
  group_by(postcode_area) |>
  slice_head(n = 1) |>
  ungroup()

# =============================================================================
# 1. Noise + distortion helpers (same grammar as generate_workshop_example.R)
# =============================================================================

legalform_drift <- function(name) {
  if (runif(1) < 0.5) return(name)
  name |>
    str_replace_all("\\bLtd\\b", sample(c("Limited", "Ltd."), 1)) |>
    str_replace_all("\\bLLP\\b", "L.L.P.")
}

punct_noise <- function(name) {
  if (runif(1) < 0.4) name <- str_replace_all(name, " & ", sample(c(" and ", " + "), 1))
  if (runif(1) < 0.2) name <- toupper(name)
  name
}

typo <- function(x) {
  if (runif(1) < 0.88 || nchar(x) < 5) return(x)
  pos <- sample(2:(nchar(x) - 1), 1)
  substr(x, pos, pos) <- sample(letters, 1)
  x
}

owner_messify <- function(proprietor) {
  parts <- str_split(str_squish(proprietor), "\\s+")[[1]]
  if (length(parts) < 2) return(proprietor)
  first <- parts[[1]]; last <- parts[[length(parts)]]
  r <- runif(1)
  if (r < 0.30) paste0(substr(first, 1, 1), ". ", last)
  else if (r < 0.45) paste0(last, ", ", substr(first, 1, 1), ".")
  else if (r < 0.55) last
  else paste(first, last)
}

# A structural name change a directory makes over time: drop a trailing
# descriptor, or abbreviate "& Sons" / append a generic tail.
name_restructure <- function(name) {
  r <- runif(1)
  if (r < 0.34 && str_detect(name, "\\s")) {
    # drop the last token (e.g. legal form / descriptor)
    str_replace(name, "\\s+\\S+$", "")
  } else if (r < 0.67) {
    str_replace_all(name, "\\b& Sons\\b", "& Son")
  } else {
    paste(name, sample(c("Workshop", "Studio", "Co"), 1))
  }
}

have_phonics <- requireNamespace("phonics", quietly = TRUE)
soundex1 <- function(x) if (have_phonics) phonics::soundex(x, clean = FALSE) else x
cologne1 <- function(x) if (have_phonics) phonics::cologne(x, clean = FALSE) else x

phonetic_twin <- function(stem) {
  cand <- unique(c(
    str_replace(stem, "e$", ""),
    str_replace(stem, "ck", "k"),
    str_replace(stem, "ph", "f"),
    str_replace(stem, "([bcdfgklmnprst])\\1", "\\1"),
    str_replace(stem, "^Mac", "Mc"),
    str_replace(stem, "ie", "y"),
    str_replace(stem, "y$", "ey")
  ))
  cand <- cand[cand != stem & nchar(cand) >= 3]
  if (!length(cand)) return(NA_character_)
  ok <- cand[soundex1(cand) == soundex1(stem) & cologne1(cand) == cologne1(stem)]
  if (length(ok)) ok[[1]] else NA_character_
}

sample_other_area <- function(area) {
  pool <- area_town[area_town$postcode_area != area, ]
  pool[sample(nrow(pool), 1), ]
}

# =============================================================================
# 2. Sample entities + assign year spans and a change tier
# =============================================================================

entities <- workshop_register |>
  filter(gen_tier == "core") |>
  slice_sample(n = N_ENTITIES) |>
  transmute(
    true_entity = reg_no,
    base_name   = workshop,
    proprietor,
    trade,
    postcode_area,
    town,
    established,
    change_tier = sample(names(TIER_WEIGHTS), n(), replace = TRUE, prob = TIER_WEIGHTS)
  )

# A contiguous presence window inside YEARS: most are present several years,
# a realistic minority enter late or exit early.
assign_span <- function() {
  start <- sample(YEARS, 1, prob = c(0.40, 0.20, 0.18, 0.12, 0.10))
  len   <- sample(seq_len(length(YEARS)), 1,
                  prob = c(0.10, 0.16, 0.22, 0.24, 0.28))
  present <- start:min(start + len - 1L, max(YEARS))
  present
}

# =============================================================================
# 3. Emit one messy row per (entity, year)
# =============================================================================

build_entity_rows <- function(e) {
  present <- assign_span()
  if (e$change_tier == "mover" && length(present) >= 2) {
    switch_year <- present[sample(seq.int(2L, length(present)), 1)]
  } else switch_year <- NA_integer_

  # phonetic: pick the switch year and the twin once, applied from then on
  stem <- str_split(e$base_name, "\\s+")[[1]][[1]]
  twin <- if (e$change_tier == "phonetic") phonetic_twin(stem) else NA_character_
  if (e$change_tier == "phonetic" && !is.na(twin) && length(present) >= 2) {
    ph_year <- present[sample(seq.int(2L, length(present)), 1)]
  } else ph_year <- NA_integer_

  # name_drift: a one-off structural change from a switch year on
  if (e$change_tier == "name_drift" && length(present) >= 2) {
    nd_year   <- present[sample(seq.int(2L, length(present)), 1)]
    drift_name <- name_restructure(e$base_name)
  } else { nd_year <- NA_integer_; drift_name <- e$base_name }

  map_dfr(present, function(yr) {
    area <- e$postcode_area; town <- e$town; name <- e$base_name

    if (!is.na(switch_year) && yr >= switch_year) {
      g <- sample_other_area(area); area <- g$postcode_area; town <- g$town
    }
    if (!is.na(nd_year) && yr >= nd_year) name <- drift_name
    if (!is.na(ph_year) && yr >= ph_year) {
      name <- str_replace(name, fixed(stem), twin)
    }

    # light per-year noise so no two year-rows are byte-identical
    name <- name |> legalform_drift() |> punct_noise() |> typo()

    tibble(
      year          = yr,
      workshop      = name,
      proprietor    = owner_messify(e$proprietor),
      trade         = e$trade,
      postcode_area = area,
      town          = town,
      established   = e$established,
      true_entity   = e$true_entity,
      change_tier   = e$change_tier
    )
  })
}

panel_rows <- entities |>
  rowwise() |>
  group_split() |>
  map_dfr(build_entity_rows)

workshop_panel <- panel_rows |>
  arrange(true_entity, year) |>
  mutate(record_id = sprintf("YR-%05d", row_number()), .before = 1) |>
  select(record_id, year, workshop, proprietor, trade, postcode_area, town,
         established, true_entity, change_tier)

# =============================================================================
# 4. Sanity checks
# =============================================================================

cat("== sanity ==\n")
stopifnot(!any(duplicated(workshop_panel$record_id)))
stopifnot(all(workshop_panel$true_entity %in% workshop_register$reg_no))
stopifnot(all(workshop_panel$year %in% YEARS))

# movers must actually change postcode_area within their trajectory
mover_ent <- workshop_panel |>
  filter(change_tier == "mover") |>
  group_by(true_entity) |>
  summarise(n_area = n_distinct(postcode_area), n_year = n(), .groups = "drop")
cat(sprintf("mover entities with >1 area : %d / %d (single-year movers cannot move)\n",
            sum(mover_ent$n_area > 1), nrow(mover_ent)))

# phonetic twins must be code-preserving where applied
cat(sprintf("phonics available (verified) : %s\n", have_phonics))

# =============================================================================
# 5. Summary + save
# =============================================================================

cat("\n== workshop_panel ==\n")
cat(sprintf("rows: %d | entities: %d | years: %s\n",
            nrow(workshop_panel), n_distinct(workshop_panel$true_entity),
            paste(range(workshop_panel$year), collapse = "-")))
cat("rows per year:\n"); print(count(workshop_panel, year, name = "n"))
cat("entities per change tier:\n")
print(workshop_panel |> distinct(true_entity, change_tier) |> count(change_tier, name = "n"))
cat("trajectory length distribution:\n")
print(workshop_panel |> count(true_entity) |> count(n, name = "entities"))

usethis::use_data(workshop_panel, overwrite = TRUE)
cat("\nDone. workshop_panel written to data/.\n")
