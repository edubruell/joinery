# Workshop guild register (base) for record linkage examples

A synthetic register of UK joinery and carpentry workshops, styled like
an excerpt from a Guild of Master Craftsmen trade roll. It is the clean
base table for the linkage examples: distinctive workshop names paired
with boilerplate trade and legal-form terms, so the
rarity-versus-boilerplate behaviour of the matcher is visible. Pairs
with `workshop_listings`, the messier external directory. Block on
`(postcode_area, trade)`.

## Usage

``` r
workshop_register
```

## Format

A tibble with 1,052 rows and 15 variables:

- reg_no:

  Character registration number, the base id. Most are `"GMC-#####"`;
  planted duplicates, homonyms, and shared-venue rows carry
  `"GMC-D####"`, `"GMC-H####"`, and `"GMC-V####"` prefixes respectively.

- workshop:

  Canonical business name (distinctive stem plus trade and legal-form
  boilerplate)

- proprietor:

  Proprietor name

- trade:

  One of eight woodworking trades; half the blocking key

- legal_form:

  Ltd, LLP, Partnership, or Sole Trader

- postcode_area:

  UK outward-code area (e.g. "LS"); half the blocking key

- town:

  Town for the postcode area

- address:

  Street address

- established:

  Year the workshop was established

- employees:

  Headcount, varying with legal form

- apprentices:

  Number of apprentices

- guild_member:

  Logical, whether a current guild member

- sic:

  UK SIC 2007 industry code for the trade

- true_entity:

  Evaluation only. Same-entity key: planted duplicate rows share it,
  homonym workshops get distinct keys.

- gen_tier:

  Evaluation only. Which generation tier the row belongs to. Three rows
  are `hub_trap`: short-named shared venues ("Trinity Workshops", "The
  Forge", "Riverside Works") that are themselves guild registered. Their
  two-token names are a forward-containment subset of every ", "
  listing, so they bait an exact containment strategy into merging
  unrelated workshops; the `min_containment_tokens` guard blocks them.

## Source

Synthetically generated. Distinct workshop identities come from a frozen
LLM seed (`data-raw/llm_workshop_seed.R`); all geography, colour
columns, planted duplicates, and homonyms are added by the seeded,
offline `data-raw/generate_workshop_example.R`. Ships no real business.

## See also

[`workshop_listings`](https://edubruell.github.io/joinery/reference/workshop_listings.md)
