# =============================================================================
# data-raw/llm_workshop_seed.R
# -----------------------------------------------------------------------------
# Stage 1 of 2 for the joinery example data (notes/v09/06).
#
# Produces a FROZEN synthetic seed for a UK guild register of joinery & carpentry
# workshops. This is the only non-deterministic, API-dependent piece: it calls an
# open model via OpenRouter (tidyllm) to invent plausible UK workshops with the
# realistic "grammar of the mess" (slogan-stuffed names, owner reorders, dropped
# tokens, legal-form drift), then writes the result to two cached CSVs in
# data-raw/. Stage 2 (generate_workshop_example.R) is seeded and offline: it
# reads these CSVs and owns the identity ledger (actual_link, planted
# duplicates, sizes, geography).
#
# REPRODUCIBILITY MODEL (mirrors localwip/yp_panel/build_wz08_map.R -> atom_map):
#   The frozen CSVs ARE the reproducible artifact, not the API call. Run this
#   once; commit the CSVs to data-raw/; never let an .rda rebuild depend on a
#   live key. Per-batch results are cached under data-raw/workshop_seed_cache/
#   so a run is resumable. Delete the cache + CSVs to regenerate from scratch.
#
# WHY LLM, NOT RULES:
#   Real trade listings bury a rare, distinctive stem inside boilerplate and
#   slogans ("Hartley Bespoke Joinery - Fine Handmade Furniture - Est. 1987").
#   No typo()/drop_vowel() chain produces that. The mess GRAMMAR (bare surname ->
#   trade-first -> owner reorder -> slogan-stuffed -> legal-form drift) was
#   distilled from a real listings corpus; it is language-agnostic and re-
#   expressed here in UK English via authored few-shots. No real business is
#   shipped verbatim; everything generated is fictional.
#
# RUN:
#   Sys.setenv(OPENROUTER_API_KEY = "...")   # if not already in the environment
#   Rscript data-raw/llm_workshop_seed.R
# =============================================================================

# =============================================================================
# 0. Setup
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(jsonlite)
  library(tidyllm)
})

if (!nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
  stop("OPENROUTER_API_KEY environment variable is not set.")
}

here       <- "data-raw"
cache_dir  <- file.path(here, "workshop_seed_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

entities_out <- file.path(here, "workshop_seed_entities.csv")
variants_out <- file.path(here, "workshop_seed_variants.csv")

# Open model via OpenRouter. A ~31B instruct model is the sweet spot here:
# strong enough for varied, plausible UK names + clean JSON, and so cheap the
# whole run costs well under a dollar. Gemma-4-31b was chosen after a live probe
# (qwen3.5-27b returned empty via this path; gemma-4 produced perfect JSON
# arrays). FALLBACK_MODELS routes to a sibling if the primary is unavailable
# (OpenRouter `.models`). Browse options with tidyllm::openrouter_list_models().
#
# We do NOT use OpenRouter structured outputs (.json_schema): open models honour
# it inconsistently (one probe gave column-oriented arrays + a hallucinated
# field). Instead we prompt for a plain JSON array with a worked example and
# parse defensively below -- robust across providers.
MODEL           <- "google/gemma-4-31b-it"
FALLBACK_MODELS <- c("google/gemma-4-26b-a4b-it")   # same family, cheaper sibling

# Universe size. Stage 2 carves base/target/duplicates out of this pool, so this
# is the number of DISTINCT true entities. The model reuses a small set of
# "characterful" surnames across batches, so the raw->distinct collision is high
# (~60%); we counter it with a live avoid-list (see AVOID logic below) and a
# generous N_ENTITIES so the distinct pool lands comfortably above the
# base/target split. Scale freely -- at these prices the cost is negligible.
N_ENTITIES     <- 2600
GEN_BATCH_SIZE <- 25
N_VARIANTS     <- 600         # size of the hard-tier coreferent-variant pool

set.seed(42)                  # fixes batch theme nudges only; LLM output varies

# =============================================================================
# 1. Theme vocabulary
# -----------------------------------------------------------------------------
# TRADES double as the blocking key (postcode_area, trade) in Stage 2 and as the
# rarity contrast: "Joiner"/"Carpenter" are common; "French Polisher"/"Wood
# Turner"/"Boat Builder" are rare. Weights skew generation toward the common
# trades so the rarity story is realistic.
# =============================================================================

TRADES <- tribble(
  ~trade,                  ~weight,
  "Joiner",                 0.26,
  "Carpenter",              0.22,
  "Cabinet Maker",          0.15,
  "Shopfitter",             0.10,
  "Staircase Specialist",   0.08,
  "Wood Turner",            0.07,
  "Boat Builder",           0.06,
  "French Polisher",        0.06
)

# UK regions / counties used as per-batch nudges -- they steer the model toward a
# locale's naming flavour and spread variation across the country. Stage 2 owns
# geography, so these feed nothing downstream; they only diversify the names.
REGIONS <- c(
  "Yorkshire", "Devon", "Kent", "Cumbria", "the Lothians around Edinburgh",
  "South Wales", "Norfolk", "Cornwall", "Cheshire", "Tyne and Wear",
  "Bristol and the West Country", "the Scottish Highlands", "Hampshire",
  "Shropshire", "Suffolk", "Lancashire", "Gloucestershire", "Dorset",
  "Aberdeenshire", "Pembrokeshire", "Northumberland", "Somerset",
  # broadened coverage -- wider locales widen the plausible-surname space and so
  # cut cross-batch stem collision
  "Essex", "Surrey", "the Cotswolds", "Herefordshire", "Lincolnshire",
  "Derbyshire", "County Durham", "North Wales", "the Isle of Wight",
  "Ayrshire", "the Scottish Borders", "Fife", "Antrim", "County Down",
  "Worcestershire", "Staffordshire", "Nottinghamshire", "Leicestershire"
)

LEGAL_FORMS <- c("Ltd", "LLP", "Partnership", "Sole Trader")

# Surnames the model over-reaches for: from the first pass these few invented
# stems alone drove most of the collision (vane x32, holloway x29, ...). Pin them
# as permanent avoids; the live avoid-list (cached stems) handles the long tail.
HOT_STEMS <- c(
  "vane", "holloway", "sloane", "sterling", "thorne", "wickham", "mallow",
  "fairweather", "grimshaw", "kemp", "penhaligon", "sutherland", "hargreaves",
  "lowther", "ashworth", "thornbury", "blackwood", "hartley", "ashcroft"
)

# Mess-grammar exemplars, distilled from a real trade-listings corpus and recast
# in UK idiom. They teach BOTH jobs: how a clean canonical name looks, and the
# realistic ways the same business gets rewritten in a scrappier directory.
MESS_GRAMMAR <- '
Real trade listings render the SAME business in wildly different shapes. Study
these UK examples (the structural patterns, not the specific words):

  Canonical (guild register)        Messy external listing
  --------------------------        ----------------------
  Hartley Joinery Ltd               Hartley, J. - Bespoke Joiners & Cabinet Makers
  Oakfield Cabinet Makers           Oakfield Fine Furniture, t/a M. Oakes
  Pennine Carpentry & Joinery Ltd   PENNINE Carpentry Ltd - Kitchens|Staircases|Decking
  R. Blackwood & Sons               Blackwood and Sons (est. 1962) Master Joiners
  Trewin Boatbuilders               Trewin Boat Building - Traditional Wooden Boats, Falmouth
  Ashcroft Staircases               The Staircase Workshop (Ashcroft) Ltd
  Margaret Coe French Polishing     M Coe - French Polisher & Antique Restoration

Patterns at work: bare surname; trade-word first vs last; owner name reordered or
initialised or dropped; "& Sons" / "t/a" / "(Inh.)" annotations; slogans and
service lists bolted on; legal-form drift (Ltd <-> Limited, & Co <-> and Company);
casing noise (ALLCAPS, Title Case); separators (-, |, ,); the occasional honest
typo. The distinctive STEM (Hartley, Trewin, Ashcroft) survives every rewrite --
that is the signal the matcher must find under the boilerplate.
'

# =============================================================================
# 2. Call + defensive JSON-array parse
# -----------------------------------------------------------------------------
# or_call() returns the model's raw text reply. parse_json_rows() turns that
# into a tibble with exactly `required` columns, coping with the three shapes an
# open model emits: a clean array of objects (the common case), a column-
# oriented object ({stem:[...], business:[...]}), and prose-wrapped or fenced
# JSON. A batch that doesn't yield all required columns returns empty (skipped,
# not cached) so a later re-run retries it.
# =============================================================================

or_call <- function(prompt, temperature, max_tokens = 8192) {
  # The whole call is wrapped: OpenRouter occasionally returns a truncated HTTP
  # body, which surfaces as a "premature EOF" JSON parse error *inside*
  # openrouter_chat() (httr2::resp_body_json), before get_reply() ever runs.
  # Degrade any such transport/parse failure to an empty reply so the batch
  # yields an empty tibble (skipped, not cached) and a re-run retries it,
  # rather than aborting the entire pmap.
  tryCatch({
    res <- llm_message(prompt) |>
      openrouter_chat(
        .model       = MODEL,
        .models      = FALLBACK_MODELS,
        .temperature = temperature,
        .max_tokens  = max_tokens
      )
    get_reply(res)
  }, error = function(e) "")
}

parse_json_rows <- function(txt, required) {
  if (is.null(txt) || !nzchar(str_trim(txt))) return(tibble())
  clean <- str_replace_all(txt, "```(json)?", "")
  arr   <- str_extract(clean, "(?s)\\[.*\\]")          # outermost array, if any
  clean <- if (!is.na(arr)) arr else clean
  j <- tryCatch(fromJSON(clean, simplifyDataFrame = TRUE),
                error = function(e) NULL)
  if (is.null(j)) return(tibble())

  df <- tryCatch({
    if (is.data.frame(j))            as_tibble(j)
    else if (all(required %in% names(j))) as_tibble(j[required])  # column-oriented
    else                             map_dfr(j, ~ as_tibble(as.list(.x)))
  }, error = function(e) tibble())

  if (nrow(df) == 0L || !all(required %in% names(df))) return(tibble())
  df |> select(all_of(required))
}

ENTITY_COLS  <- c("stem", "business", "proprietor_first",
                  "proprietor_last", "trade", "legal_form")
VARIANT_COLS <- c("seed_id", "listing_name", "listing_owner")

# =============================================================================
# 3. Job 1 -- synthetic canonical entity universe
# =============================================================================

ENTITY_EXAMPLE <- paste0(
  '{"stem":"Hartley","business":"Hartley Joinery Ltd",',
  '"proprietor_first":"James","proprietor_last":"Hartley",',
  '"trade":"Joiner","legal_form":"Ltd"}')

n_batches  <- ceiling(N_ENTITIES / GEN_BATCH_SIZE)
batch_plan <- tibble(
  batch_idx = seq_len(n_batches),
  k         = pmin(GEN_BATCH_SIZE, N_ENTITIES - (seq_len(n_batches) - 1) * GEN_BATCH_SIZE),
  region    = REGIONS[((seq_len(n_batches) - 1) %% length(REGIONS)) + 1],
  emphasis  = sample(TRADES$trade, n_batches, replace = TRUE, prob = TRADES$weight)
)

allowed_trades <- paste(TRADES$trade, collapse = ", ")

gen_prompt <- function(k, region, emphasis, avoid = character()) {
  avoid_block <- if (length(avoid)) glue('
These surnames are ALREADY used in the dataset. Do NOT use any of them, nor a
near-variant (no plural, no different spelling of the same root). Invent fresh,
distinctive surnames instead:
{paste(sort(unique(avoid)), collapse = ", ")}
') else ""
  glue('
You are building a small SYNTHETIC dataset of UK joinery and carpentry workshops
that looks like an excerpt from a Guild of Master Craftsmen trade register.

Invent {k} FICTIONAL workshops based in {region}, UK, drawing on how real wood-
trade businesses there actually name themselves: surname patterns, trade
descriptors, "& Sons", legal forms, and the distinctive marks that set one
workshop apart. Do NOT reproduce any real business, owner, or address -- invent
plausible new ones. Bias the trades toward "{emphasis}" but include a spread
across the allowed trades.

Make the names varied and realistic:
- some are a bare surname, some lead with the trade word, some end with it;
- vary the legal forms; some are "& Sons" partnerships, some sole traders;
- the STEM (the rare, distinctive core) must be separable from the boilerplate
  trade words and legal form -- that is the whole point of the dataset.

Favour UNCOMMON, regionally distinctive surnames (Welsh, Scottish, Cornish, and
less common English names suit "{region}") over the most frequent UK surnames
(Smith, Jones, Williams, Brown, Taylor). Vary the proprietor given names widely
too -- do not repeat the same first names across rows.
{avoid_block}
Allowed trades (use these exact strings): {allowed_trades}
Legal forms (use these exact strings): Ltd, LLP, Partnership, Sole Trader

Return ONLY a JSON array of {k} objects -- no prose, no markdown fences. Each
object has EXACTLY these keys: stem, business, proprietor_first,
proprietor_last, trade, legal_form. Example of ONE object:
{ENTITY_EXAMPLE}
')
}

# Live avoid-list: every already-cached batch has written its rds, so a new batch
# can read the stems used so far and steer clear of them. Because pmap runs
# batches in order, this accumulates within a single from-scratch run too -- not
# just on a cached top-up. HOT_STEMS are pinned permanently.
used_stems_so_far <- function() {
  done <- list.files(cache_dir, "^entities_\\d+\\.rds$", full.names = TRUE)
  cached <- if (length(done))
    str_to_lower(unlist(map(done, ~ read_rds(.x)$stem))) else character()
  unique(c(HOT_STEMS, cached[nzchar(cached)]))
}

generate_entities_batch <- function(batch_idx, k, region, emphasis) {
  cache_file <- file.path(cache_dir, sprintf("entities_%04d.rds", batch_idx))
  if (file.exists(cache_file)) {
    cat(glue("[ent {batch_idx}/{n_batches}] cached"), "\n")
    return(read_rds(cache_file))
  }

  # Show the model a deterministic sample of already-used surnames to avoid. Keep
  # it bounded so the prompt stays cheap; reseed locally and restore the global
  # RNG so the avoid sampling never perturbs anything downstream.
  pool <- used_stems_so_far()
  tail_pool <- setdiff(pool, HOT_STEMS)         # the long tail, minus pinned
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(20260616L + batch_idx)
  # always show every pinned hot stem (cheap, ~20 names, the worst offenders);
  # sample the long tail so the prompt stays bounded.
  avoid <- c(HOT_STEMS,
             if (length(tail_pool)) sample(tail_pool, min(80L, length(tail_pool)))
             else character())
  if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)

  cat(glue("[ent {batch_idx}/{n_batches}] {region} / {emphasis} -- {k} workshops",
           " (avoiding {length(pool)} stems)"), "\n")
  out <- parse_json_rows(
    or_call(gen_prompt(k, region, emphasis, avoid), temperature = 1.0),
    ENTITY_COLS
  )
  if (nrow(out) == 0L) {
    warning(glue("batch {batch_idx} returned no rows; not caching"))
    return(tibble())
  }
  out <- out |> mutate(gen_region = region, gen_emphasis = emphasis)
  write_rds(out, cache_file)
  out
}

entities_raw <- pmap(batch_plan, function(batch_idx, k, region, emphasis)
  generate_entities_batch(batch_idx, k, region, emphasis)) |>
  bind_rows()

# Dedup on the distinctive stem (case-insensitive) so two batches can't ship the
# same fictional workshop; keep only the allowed trades; assign a stable seed_id.
entities <- entities_raw |>
  mutate(across(c(stem, business, proprietor_first, proprietor_last, trade,
                  legal_form), ~ str_squish(as.character(.x)))) |>
  filter(nzchar(stem), nzchar(business), trade %in% TRADES$trade) |>
  distinct(stem_lc = str_to_lower(stem), .keep_all = TRUE) |>
  select(-stem_lc) |>
  mutate(seed_id = sprintf("W%05d", row_number()), .before = 1)

write_csv(entities, entities_out)
cat(glue("Wrote {nrow(entities)} distinct entities -> {entities_out}"), "\n")

# =============================================================================
# 4. Job 2 -- hard-tier coreferent variants
# -----------------------------------------------------------------------------
# For a sampled pool of entities, ask for ONE messy external-directory rendering
# of the SAME workshop, guided by the mess grammar. Stage 2 draws the hard-tier
# matched pairs from this pool; the easy tier gets cheap code-side rule-noise.
# =============================================================================

VARIANT_EXAMPLE <- paste0(
  '{"seed_id":"W00042","listing_name":"Hartley, J. - Bespoke Joiners & Cabinet Makers",',
  '"listing_owner":"J. Hartley"}')

variant_pool <- entities |> slice_sample(n = min(N_VARIANTS, nrow(entities)))
var_batches  <- split(variant_pool,
                      ceiling(seq_len(nrow(variant_pool)) / GEN_BATCH_SIZE))
n_var        <- length(var_batches)

var_prompt <- function(df) {
  rows <- pmap_chr(df, function(seed_id, business, proprietor_first,
                                proprietor_last, ...)
    glue("- seed_id={seed_id} | business=\"{business}\" | owner=\"{proprietor_first} {proprietor_last}\""))
  glue('
{MESS_GRAMMAR}

Below are canonical guild-register workshops. For EACH one, write exactly one
messy external-directory rendering of the SAME business, applying the patterns
above (vary which patterns per row). Keep the distinctive stem recoverable. Copy
each seed_id back unchanged so the rows can be paired. These are UK businesses:
use only UK legal forms (Ltd, Limited, LLP, & Co) -- never American forms like
LLC or Inc.

Workshops:
{paste(rows, collapse = "\n")}

Return ONLY a JSON array -- no prose, no markdown fences -- with one object per
workshop in the same order. Each object has EXACTLY these keys: seed_id,
listing_name, listing_owner. Example of ONE object:
{VARIANT_EXAMPLE}
')
}

generate_variants_batch <- function(batch_idx, df) {
  cache_file <- file.path(cache_dir, sprintf("variants_%04d.rds", batch_idx))
  if (file.exists(cache_file)) {
    cat(glue("[var {batch_idx}/{n_var}] cached"), "\n")
    return(read_rds(cache_file))
  }
  cat(glue("[var {batch_idx}/{n_var}] {nrow(df)} renderings"), "\n")
  out <- parse_json_rows(
    or_call(var_prompt(df), temperature = 0.9),
    VARIANT_COLS
  )
  if (nrow(out) == 0L) {
    warning(glue("variant batch {batch_idx} returned no rows; not caching"))
    return(tibble())
  }
  write_rds(out, cache_file)
  out
}

variants <- imap(var_batches, \(df, i) generate_variants_batch(as.integer(i), df)) |>
  bind_rows() |>
  mutate(across(everything(), ~ str_squish(as.character(.x)))) |>
  filter(seed_id %in% entities$seed_id) |>
  distinct(seed_id, .keep_all = TRUE)

write_csv(variants, variants_out)
cat(glue("Wrote {nrow(variants)} coreferent variants -> {variants_out}"), "\n")

# =============================================================================
# 5. Manifest
# =============================================================================

writeLines(c(
  "# workshop seed -- frozen LLM artifact for joinery example data",
  glue("generated: {Sys.time()}"),
  glue("model: {MODEL}  fallbacks: {paste(FALLBACK_MODELS, collapse = ', ')}  (OpenRouter)"),
  glue("entities: {nrow(entities)}  ({entities_out})"),
  glue("variants: {nrow(variants)}  ({variants_out})"),
  "",
  "Consumed by data-raw/generate_workshop_example.R (Stage 2, seeded + offline).",
  "Regenerate by deleting workshop_seed_cache/ and the two CSVs, then re-running",
  "data-raw/llm_workshop_seed.R with OPENROUTER_API_KEY set."
), file.path(here, "workshop_seed_MANIFEST.txt"))

cat("Done. Frozen seed written to data-raw/. Next: generate_workshop_example.R\n")
