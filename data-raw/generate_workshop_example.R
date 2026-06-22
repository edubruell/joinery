# =============================================================================
# data-raw/generate_workshop_example.R
# -----------------------------------------------------------------------------
# Stage 2 of 2 for the joinery example data (notes/v09/06).
#
# SEEDED + OFFLINE. Reads the frozen LLM seed (Stage 1, llm_workshop_seed.R):
#   data-raw/workshop_seed_entities.csv   distinct fictional UK workshops
#   data-raw/workshop_seed_variants.csv   hard, slogan-stuffed messy renderings
# and owns the identity ledger -- ids, geography, the chamber-roll colour
# columns, planted duplicates, and every "feature-exercise tier" (the structures
# that make joinery's advanced features necessary). No network, no API key; a
# fixed set.seed() makes the two shipped datasets fully regenerable.
#
# Emits two package datasets:
#   workshop_register  base, the clean guild roll  (id = reg_no)
#   workshop_listings  target, a messier external directory (actual_link -> reg_no)
#
# GROUND TRUTH (documented evaluation-only columns):
#   workshop_register$true_entity   same-entity key (planted dupes share it;
#                                    homonyms get distinct keys)
#   workshop_listings$actual_link   the reg_no a listing refers to (NA = new)
#   *$gen_tier                      which feature-exercise tier the row belongs
#                                   to, so an article can slice "just the movers"
#
# FEATURE-EXERCISE TIERS (see notes/v09/06 "Feature-exercise tiers"):
#   clean        light rule-noise, same block            -> baseline fuzzy
#   slogan       listing = register tokens + extra        -> exact containment="forward"
#   variant      LLM slogan-stuffed messy rendering       -> fuzzy + feedback_strength
#   mover        same workshop, DIFFERENT postcode_area   -> block_on_tokens + rarity_scope="global"
#   phonetic     stem swapped to a code-preserving twin   -> as_cologne/as_soundex + drop_short_tokens
#   hub_member   "<workshop>, <shared venue>"             -> the hot/boilerplate token
#   hub_trap     bare shared-venue row (actual_link NA)   -> min_base_rarity / max_token_df guard
#   category_trap bare "<The Trade>" row (actual_link NA) -> min_containment_tokens guard
#   homonym_*    common-surname collisions (3 difficulties) -> ambiguity + calibration
#   new          workshop absent from the register        -> one-sided residual
#
# RUN:  Rscript data-raw/generate_workshop_example.R
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(42)

here          <- "data-raw"
entities_in   <- file.path(here, "workshop_seed_entities.csv")
variants_in   <- file.path(here, "workshop_seed_variants.csv")
stopifnot(file.exists(entities_in), file.exists(variants_in))

entities <- read_csv(entities_in, show_col_types = FALSE)
variants <- read_csv(variants_in, show_col_types = FALSE)

# Mix knobs (shares of the MATCHED, non-special listings). Tune here.
P_MATCHED      <- 0.72   # share of register workshops that appear in listings
P_NEW          <- 0.16   # target-only listings, as a share of matched count
TIER_WEIGHTS   <- c(clean = 0.45, slogan = 0.15, variant = 0.22,
                    mover = 0.10, phonetic = 0.08)
N_REG_DUPS     <- 65L    # planted within-register duplicates (for detect_duplicates)
N_HUBS         <- 6L     # shared-venue hub clusters (containment-blocker trap)
N_CATEGORY     <- 8L     # bare-category listings (min_containment_tokens trap)
N_HOMONYM_SET  <- 14L    # area/total homonym clusters per tier (do not co-block)
N_HOMONYM_BLOCK <- 26L   # co-blocking homonym clusters (same block, different owner)
K_HOMONYM_BLOCK <- 3L    # siblings per co-blocking cluster -> within-block disambiguation

# =============================================================================
# 1. Vocabulary -- UK geography, SIC, addresses, hubs, homonym + phonetic tables
# =============================================================================

# Postcode AREA (outward-code letters) -> a representative town. The block key is
# (postcode_area, trade), the faithful sibling of the YP (plz2, wz08_3).
uk_geo <- tribble(
  ~postcode_area, ~town,
  "LS", "Leeds",        "BD", "Bradford",     "BS", "Bristol",
  "EX", "Exeter",       "PL", "Plymouth",     "TR", "Truro",
  "CA", "Carlisle",     "EH", "Edinburgh",    "CF", "Cardiff",
  "NR", "Norwich",      "NE", "Newcastle",    "IV", "Inverness",
  "SO", "Southampton",  "SY", "Shrewsbury",   "IP", "Ipswich",
  "PR", "Preston",      "GL", "Gloucester",   "DT", "Dorchester",
  "AB", "Aberdeen",     "SA", "Swansea",      "TA", "Taunton",
  "CM", "Chelmsford",   "GU", "Guildford",    "HR", "Hereford",
  "LN", "Lincoln",      "DE", "Derby",        "DH", "Durham",
  "LL", "Llandudno",    "KA", "Kilmarnock",   "TD", "Galashiels",
  "KY", "Kirkcaldy",    "BT", "Belfast",      "WR", "Worcester"
)

# UK SIC 2007 code per trade (the colour column that looks like a chamber roll).
trade_sic <- tribble(
  ~trade,                  ~sic,
  "Joiner",                "43320",
  "Carpenter",             "43320",
  "Cabinet Maker",         "31090",
  "Shopfitter",            "43320",
  "Staircase Specialist",  "16230",
  "Wood Turner",           "16290",
  "Boat Builder",          "30120",
  "French Polisher",       "95240"
)

uk_streets <- c(
  "High Street", "Mill Lane", "Station Road", "Church Street", "Victoria Road",
  "Queens Road", "Trinity Street", "Forge Lane", "Kiln Road", "Bridge Street",
  "Market Place", "Albion Works", "Canal Wharf", "The Old Yard", "Bank Street",
  "King Street", "George Street", "Wharf Road", "Foundry Lane", "Tanyard Close",
  "Cooper's Row", "Carpenter's Walk", "St John's Road", "West End", "Northgate"
)

# Shared workshop venues -- a single hub token spans several unrelated workshops.
# Disjoint from COLLISION_VENUES below: those three venue names are reserved for
# the register-side containment trap and must each live in exactly one block.
hub_venues <- c(
  "The Old Sawmill", "Maker's Yard", "The Timber Yard", "Wenlock Workshops",
  "Canalside Studios", "Bridgewater Mews", "Kiln Lane Studios", "Stanhope Yard"
)

# Common UK surnames for planted homonyms + target-only "new" listings. These are
# deliberately frequent (the opposite of Stage 1's distinctive stems) so two
# different workshops genuinely collide on name.
common_surnames <- c(
  "Walker", "Hughes", "Clarke", "Bell", "Knight", "Reed", "Shaw", "Wright",
  "Brooks", "Webb", "Hill", "Cooper", "Turner", "Ward", "Cox", "Gray"
)
common_first <- c(
  "James", "David", "Paul", "Mark", "Andrew", "Stephen", "Ian", "Robert",
  "Susan", "Claire", "Helen", "Karen", "Thomas", "Daniel", "Gary", "Neil"
)

# =============================================================================
# 2. Noise + distortion helpers
# =============================================================================

# Drift a legal form the way a scrappy directory does (Ltd <-> Limited, etc).
legalform_drift <- function(name) {
  if (runif(1) < 0.5) return(name)
  name |>
    str_replace_all("\\bLtd\\b", sample(c("Limited", "Ltd."), 1)) |>
    str_replace_all("\\bLLP\\b", "L.L.P.")
}

punct_noise <- function(name) {
  if (runif(1) < 0.5) name <- str_replace_all(name, " & ", sample(c(" and ", " + "), 1))
  if (runif(1) < 0.3) name <- toupper(name)
  name
}

typo <- function(x) {
  if (runif(1) < 0.85 || nchar(x) < 5) return(x)
  pos <- sample(2:(nchar(x) - 1), 1)
  substr(x, pos, pos) <- sample(letters, 1)
  x
}

owner_messify <- function(first, last) {
  r <- runif(1)
  if (r < 0.30) paste0(substr(first, 1, 1), ". ", last)        # J. Hartley
  else if (r < 0.45) paste0(last, ", ", substr(first, 1, 1), ".")  # Hartley, J.
  else if (r < 0.55) last                                       # bare surname
  else paste(first, last)
}

tok <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish() |>
    str_split("\\s+") |>
    map(~ .x[nzchar(.x)])
}

# Phonetic distortion that is VERIFIED code-preserving against the same backend
# joinery's as_soundex()/as_cologne() use (phonics). Returns a surface-different
# string with an identical soundex+cologne key, or NA if no safe edit exists.
have_phonics <- requireNamespace("phonics", quietly = TRUE)
soundex1 <- function(x) if (have_phonics) phonics::soundex(x, clean = FALSE) else x
cologne1 <- function(x) if (have_phonics) phonics::cologne(x, clean = FALSE) else x

phonetic_twin <- function(stem) {
  cand <- unique(c(
    str_replace(stem, "e$", ""),                    # Steele -> Steel
    str_replace(stem, "ck", "k"),                   # Brock  -> Brok
    str_replace(stem, "ph", "f"),                   # Ralph  -> Ralf
    str_replace(stem, "([bcdfgklmnprst])\\1", "\\1"),  # un-double a consonant
    str_replace(stem, "^Mac", "Mc"),
    str_replace(stem, "ie", "y"),
    str_replace(stem, "y$", "ey")
  ))
  cand <- cand[cand != stem & nchar(cand) >= 3]
  if (!length(cand)) return(NA_character_)
  ok <- cand[soundex1(cand) == soundex1(stem) & cologne1(cand) == cologne1(stem)]
  if (length(ok)) ok[[1]] else NA_character_
}

# Slogan descriptors bolted onto a name to manufacture a proper token-SUPERSET.
slogan_tail <- function(trade) {
  base <- switch(trade,
    "Cabinet Maker"        = c("Bespoke Furniture", "Fine Cabinetry"),
    "Joiner"               = c("Bespoke Joinery", "Kitchens & Staircases"),
    "Carpenter"            = c("Carpentry & Joinery", "Building & Refurbishment"),
    "Shopfitter"           = c("Retail Fit-Out", "Commercial Interiors"),
    "Staircase Specialist" = c("Bespoke Staircases", "Stairs & Balustrades"),
    "Wood Turner"          = c("Spindles & Turnery", "Architectural Turning"),
    "Boat Builder"         = c("Traditional Wooden Boats", "Marine Joinery"),
    "French Polisher"      = c("Antique Restoration", "Polishing & Refinishing"),
    c("Fine Woodwork"))
  paste("-", sample(base, 1))
}

sample_geo <- function(n) uk_geo[sample(nrow(uk_geo), n, replace = TRUE), ]

# A single geography row whose postcode_area differs from `area` (for movers).
sample_geo_other <- function(area) {
  pool <- uk_geo[uk_geo$postcode_area != area, ]
  pool[sample(nrow(pool), 1), ]
}

chamber_cols <- function(df) {
  df |>
    left_join(trade_sic, by = "trade") |>
    mutate(
      established  = sample(1958:2021, n(), replace = TRUE),
      employees    = pmax(1L, round(rnorm(n(),
                       mean = recode(legal_form, "Sole Trader" = 2, "Partnership" = 5,
                                     "LLP" = 9, "Ltd" = 14, .default = 6), sd = 4))),
      apprentices  = pmin(employees, rpois(n(), lambda = 0.8)),
      guild_member = runif(n()) < 0.85,
      address      = paste(sample(1:180, n(), replace = TRUE), sample(uk_streets, n(), replace = TRUE))
    )
}

# =============================================================================
# 3. Register (base) -- the clean guild roll, one row per real workshop
# =============================================================================

register_core <- entities |>
  transmute(
    seed_id, stem,
    workshop   = business,
    proprietor = str_squish(paste(proprietor_first, proprietor_last)),
    first = proprietor_first, last = proprietor_last,
    trade, legal_form
  ) |>
  bind_cols(sample_geo(nrow(entities))) |>
  mutate(reg_no = sprintf("GMC-%05d", row_number()), true_entity = reg_no,
         gen_tier = "core") |>
  chamber_cols()

# --- planted within-register duplicates (a workshop entered twice) ------------
dup_src <- register_core |> slice_sample(n = N_REG_DUPS)
register_dups <- dup_src |>
  mutate(
    reg_no   = sprintf("GMC-D%04d", row_number()),
    gen_tier = "register_dup",        # true_entity stays the source's -> known dupe
    workshop = workshop |> map_chr(legalform_drift) |> map_chr(punct_noise),
    proprietor = map2_chr(first, last, owner_messify),
    address  = paste(sample(1:180, n(), replace = TRUE), sample(uk_streets, n(), replace = TRUE))
  )

# =============================================================================
# 4. Homonym clusters -- distinct entities that collide on a common surname.
#    Three difficulty tiers; each cluster's siblings get DISTINCT true_entity,
#    so a name-only merge is provably wrong.
# =============================================================================

make_homonym_cluster <- function(surname, trade, tier, k = 2L) {
  # tier1 (area):  different postcode_area, so the block separates the siblings;
  # tier2 (block): same block, different owner -> only colour columns separate;
  # tier3 (total): near-total homonym (same name + given name), different address.
  diff_area <- identical(tier, "homonym_area")
  geo   <- sample_geo(if (diff_area) k else 1L)
  first <- if (identical(tier, "homonym_total")) rep(sample(common_first, 1), k)
           else sample(common_first, k)
  tibble(
    surname = surname, trade = trade, tier = tier, first = first,
    postcode_area = if (diff_area) geo$postcode_area else rep(geo$postcode_area, k),
    town          = if (diff_area) geo$town          else rep(geo$town, k),
    sib = seq_len(k)
  )
}

# Per-tier cluster counts and sibling counts. The block tier is enriched (more
# clusters, k = 3 siblings) so listings retrieve several same-name candidates and
# the within-block disambiguation features (cnt/icnt/ipos) carry signal; the
# area/total tiers stay k = 2 since their siblings deliberately do not co-block.
# The cluster key is a per-tier counter (not surname+trade), so two clusters that
# happen to reuse a surname/trade pair stay distinct entities with distinct reg_no.
build_homonym_tier <- function(tier, n_clusters, k) {
  pmap_dfr(tibble(j = seq_len(n_clusters)), function(j) {
    cl <- make_homonym_cluster(
      surname = common_surnames[(j - 1) %% length(common_surnames) + 1],
      trade   = trade_sic$trade[(j - 1) %% nrow(trade_sic) + 1],
      tier    = tier, k = k)
    cl$cluster <- paste(tier, j, sep = "|")
    cl
  })
}

homonym_spec <- bind_rows(
  build_homonym_tier("homonym_area",  N_HOMONYM_SET,    2L),
  build_homonym_tier("homonym_block", N_HOMONYM_BLOCK,  K_HOMONYM_BLOCK),
  build_homonym_tier("homonym_total", N_HOMONYM_SET,    2L)
)

homonym_reg <- homonym_spec |>
  mutate(
    legal_form = sample(c("Ltd", "Sole Trader", "Partnership", "LLP"), n(), replace = TRUE),
    workshop   = paste(surname, trade),
    proprietor = paste(first, surname),
    first = first, last = surname, stem = surname, seed_id = NA_character_
  ) |>
  group_by(cluster) |>
  mutate(reg_no = sprintf("GMC-H%03d%d", cur_group_id(), sib)) |>
  ungroup() |>
  mutate(true_entity = reg_no, gen_tier = tier) |>     # each sibling distinct
  chamber_cols()

register <- bind_rows(register_core, register_dups, homonym_reg) |>
  select(reg_no, workshop, proprietor, trade, legal_form, postcode_area, town,
         address, established, employees, apprentices, guild_member, sic,
         true_entity, gen_tier)

# =============================================================================
# 5. Listings (target) -- the messier external directory
# =============================================================================

variant_lookup <- variants |> distinct(seed_id, .keep_all = TRUE)

# --- 5a. matched listings of register_core, one per chosen workshop -----------
matched <- register_core |>
  slice_sample(prop = P_MATCHED) |>
  mutate(tier = sample(names(TIER_WEIGHTS), n(), replace = TRUE, prob = TIER_WEIGHTS))

build_matched_listing <- function(row) {
  trade <- row$trade; area <- row$postcode_area; town <- row$town
  tier  <- row$tier
  name  <- row$workshop
  owner <- owner_messify(row$first, row$last)

  if (tier == "clean") {
    name <- name |> legalform_drift() |> punct_noise() |> typo()

  } else if (tier == "slogan") {                       # guaranteed token-superset
    name <- paste(punct_noise(name), slogan_tail(trade))

  } else if (tier == "variant") {                      # LLM messy rendering
    v <- variant_lookup[variant_lookup$seed_id == row$seed_id, ]
    if (nrow(v) == 1 && nzchar(v$listing_name)) {
      name <- v$listing_name; owner <- v$listing_owner
    } else {                                           # fall back to a superset
      name <- paste(punct_noise(name), slogan_tail(trade)); tier <- "slogan"
    }

  } else if (tier == "mover") {                        # SAME workshop, NEW area
    g <- sample_geo_other(area); area <- g$postcode_area; town <- g$town
    name <- name |> legalform_drift() |> typo()

  } else if (tier == "phonetic") {                     # code-preserving stem twin
    twin <- phonetic_twin(row$stem)
    if (!is.na(twin)) name <- str_replace(name, fixed(row$stem), twin)
    else { name <- legalform_drift(typo(name)); tier <- "clean" }
  }
  tibble(actual_link = row$reg_no, listing_name = name, listing_owner = owner,
         trade = trade, postcode_area = area, town = town, gen_tier = tier)
}

matched_listings <- matched |>
  rowwise() |>
  group_split() |>
  map_dfr(build_matched_listing)

# --- 5b. homonym listings (one clean listing per homonym register row) --------
homonym_listings <- homonym_reg |>
  transmute(
    actual_link = reg_no,
    listing_name = map_chr(workshop, ~ typo(punct_noise(.x))),
    listing_owner = map2_chr(first, surname <- last, owner_messify),
    trade, postcode_area, town, gen_tier
  )

# --- 5c. hub clusters: co-located workshops share a venue token ---------------
#   Pick (area,trade) blocks with >= 4 register rows; each becomes one hub. Member
#   listings carry "<workshop>, <venue>"; a bare "<venue>" row (no actual_link)
#   is a listing-side hub_trap (low-rarity shared token).
hub_candidates <- register_core |>
  count(postcode_area, trade, name = "k") |>
  filter(k >= 4)
hub_blocks <- hub_candidates |> slice_sample(n = min(N_HUBS, nrow(hub_candidates)))

hub_listings <- pmap_dfr(hub_blocks, function(postcode_area, trade, k) {
  venue   <- sample(hub_venues, 1)
  members <- register_core |>
    filter(postcode_area == !!postcode_area, trade == !!trade) |>
    slice_head(n = 4)
  member_rows <- members |>
    transmute(
      actual_link  = reg_no,
      listing_name = paste0(workshop, ", ", venue),
      listing_owner = map2_chr(first, last, owner_messify),
      trade, postcode_area, town, gen_tier = "hub_member")
  bare_row <- tibble(
    actual_link = NA_character_, listing_name = venue, listing_owner = NA_character_,
    trade = trade, postcode_area = postcode_area,
    town = members$town[[1]] %||% uk_geo$town[match(postcode_area, uk_geo$postcode_area)],
    gen_tier = "hub_trap")
  bind_rows(member_rows, bare_row)
})

# --- 5c.2. Hub-collision clusters: the shared-venue trap (register side) -------
#   The YP-mall problem: a shared workspace is itself registered as a guild member
#   ("Trinity Workshops Ltd"). Forward containment then makes the venue's short
#   token set a subset of every "<member>, Trinity Workshops" listing — false
#   positives. min_containment_tokens = 3 blocks this: the venue has 2 tokens
#   (after min_nchar = 3), which is below the guard; real workshop names carry 3+.
#
#   Three fixed venue names, each anchored to one trade for a clean block key.
#   Each cluster: 3 hub_member listings + 1 bare hub_trap listing + 1 register entry.

COLLISION_VENUES <- data.frame(
  venue_name = c("Trinity Workshops", "The Forge",  "Riverside Works"),
  trade      = c("Joiner",            "Carpenter",  "Shopfitter"),
  stringsAsFactors = FALSE
)

collision_clusters <- lapply(seq_len(nrow(COLLISION_VENUES)), function(i) {
  venue <- COLLISION_VENUES$venue_name[i]
  tr    <- COLLISION_VENUES$trade[i]

  cands <- register_core |>
    filter(trade == tr) |>
    count(postcode_area, town, name = "k") |>
    filter(k >= 3)
  if (nrow(cands) == 0) return(list(listings = tibble(), reg = tibble()))

  blk     <- cands[sample(nrow(cands), 1), ]
  members <- register_core |>
    filter(trade == tr, postcode_area == blk$postcode_area) |>
    slice_head(n = 3)

  listings <- bind_rows(
    members |>
      transmute(
        actual_link   = reg_no,
        listing_name  = paste0(workshop, ", ", venue),
        listing_owner = map2_chr(first, last, owner_messify),
        trade, postcode_area, town, gen_tier = "hub_member"
      ),
    tibble(
      actual_link = NA_character_, listing_name = venue,
      listing_owner = NA_character_, trade = tr,
      postcode_area = blk$postcode_area, town = blk$town,
      gen_tier = "hub_trap"
    )
  )

  # The venue itself is a guild-registered entity (the "mall" in the register).
  reg_entry <- tibble(
    reg_no        = sprintf("GMC-V%04d", i),
    workshop      = venue,
    proprietor    = NA_character_,
    trade         = tr,
    legal_form    = "Ltd",
    postcode_area = blk$postcode_area,
    town          = blk$town,
    address       = paste(sample(1:180, 1), sample(uk_streets, 1)),
    established   = sample(1985:2015, 1),
    employees     = sample(3:8, 1),
    apprentices   = 0L,
    guild_member  = TRUE,
    sic           = trade_sic$sic[trade_sic$trade == tr][1],
    true_entity   = sprintf("GMC-V%04d", i),
    gen_tier      = "hub_trap"
  )

  list(listings = listings, reg = reg_entry)
})

collision_listings <- bind_rows(lapply(collision_clusters, `[[`, "listings"))
collision_register <- bind_rows(lapply(collision_clusters, `[[`, "reg"))

# --- 5d. bare-category traps (single generic token; min_containment_tokens) ---
#   Use single-word trades so the name reduces to ONE non-stopword token, the
#   exact case the min_containment_tokens cardinality guard exists to block.
single_word_trades <- trade_sic$trade[!str_detect(trade_sic$trade, " ")]
category_listings <- tibble(
  trade = sample(single_word_trades, N_CATEGORY, replace = TRUE)
) |>
  bind_cols(sample_geo(N_CATEGORY)) |>
  mutate(
    actual_link = NA_character_,
    listing_name = paste(sample(c("The", "Bespoke", "Quality"), n(), replace = TRUE), trade),
    listing_owner = NA_character_, gen_tier = "category_trap") |>
  select(actual_link, listing_name, listing_owner, trade, postcode_area, town, gen_tier)

# --- 5e. target-only "new" listings (workshops absent from the register) ------
n_new <- round(P_NEW * nrow(matched))
new_listings <- tibble(
  trade = sample(trade_sic$trade, n_new, replace = TRUE),
  surname = sample(common_surnames, n_new, replace = TRUE),
  first   = sample(common_first, n_new, replace = TRUE),
  legal   = sample(c("Ltd", "Limited", "& Co", ""), n_new, replace = TRUE)
) |>
  bind_cols(sample_geo(n_new)) |>
  mutate(
    actual_link = NA_character_,
    listing_name = str_squish(paste(surname, trade, legal)) |> map_chr(punct_noise),
    listing_owner = map2_chr(first, surname, owner_messify),
    gen_tier = "new") |>
  select(actual_link, listing_name, listing_owner, trade, postcode_area, town, gen_tier)

# --- 5f. assemble --------------------------------------------------------------
# The matchable fields (workshop, proprietor, trade, postcode_area, town) share
# names with workshop_register so an article writes ONE formula for both tables.
workshop_listings <- bind_rows(
  matched_listings, homonym_listings,
  hub_listings, collision_listings,
  category_listings, new_listings
) |>
  mutate(listing_id = sprintf("L%05d", sample(seq_len(n()))), .before = 1) |>
  arrange(listing_id) |>
  rename(workshop = listing_name, proprietor = listing_owner) |>
  select(listing_id, workshop, proprietor, trade, postcode_area, town,
         actual_link, gen_tier)

workshop_register <- bind_rows(register, collision_register)

# =============================================================================
# 6. Sanity checks -- assert each tier's defining token-set relationship holds
# =============================================================================

reg_by_id <- workshop_register |> select(reg_no, reg_workshop = workshop,
                                         reg_area = postcode_area)
chk <- workshop_listings |>
  left_join(reg_by_id, by = c("actual_link" = "reg_no"))

# slogan: register tokens must be a SUBSET of the listing tokens (forward containment)
slog <- chk |> filter(gen_tier == "slogan")
slog_ok <- map2_lgl(tok(slog$reg_workshop), tok(slog$workshop),
                    ~ all(.x %in% .y))
# mover: postcode_area must DIFFER from the register row (plain block misses it)
mov <- chk |> filter(gen_tier == "mover")
mov_ok <- mov$postcode_area != mov$reg_area
# category_trap: exactly one non-stopword token (single generic token)
cat_tok <- tok(category_listings$listing_name) |>
  map(~ setdiff(.x, c("the", "bespoke", "quality")))

cat("== sanity ==\n")
cat(sprintf("slogan forward-containment holds : %d / %d\n", sum(slog_ok), length(slog_ok)))
cat(sprintf("mover postcode_area differs      : %d / %d\n", sum(mov_ok, na.rm = TRUE), length(mov_ok)))
cat(sprintf("category traps = 1 token         : %d / %d\n",
            sum(lengths(cat_tok) == 1), length(cat_tok)))
cat(sprintf("phonetic listings planted        : %d\n", sum(workshop_listings$gen_tier == "phonetic")))
cat(sprintf("phonics available (verified)     : %s\n", have_phonics))
stopifnot(all(slog_ok), all(mov_ok, na.rm = TRUE))
stopifnot(all(workshop_listings$actual_link %in% c(NA, workshop_register$reg_no)))
stopifnot(!any(duplicated(workshop_register$reg_no)),
          !any(duplicated(workshop_listings$listing_id)))

# =============================================================================
# 7. Summary + save
# =============================================================================

cat("\n== workshop_register ==\n")
cat(sprintf("rows: %d | distinct true_entity: %d\n",
            nrow(workshop_register), n_distinct(workshop_register$true_entity)))
print(count(workshop_register, gen_tier, name = "n"))

cat("\n== workshop_listings ==\n")
cat(sprintf("rows: %d | matched: %d | new: %d\n", nrow(workshop_listings),
            sum(!is.na(workshop_listings$actual_link)),
            sum(is.na(workshop_listings$actual_link))))
print(count(workshop_listings, gen_tier, name = "n"))

usethis::use_data(workshop_register, overwrite = TRUE)
usethis::use_data(workshop_listings, overwrite = TRUE)

cat("\nDone. workshop_register / workshop_listings written to data/.\n")
