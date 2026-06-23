# joinery — Developer / Coding-Agent Guide

## Overview

**joinery** is a heuristic, token-based record linkage system for R. It
integrates cleanly with tidyverse workflows and supports:

- **data.table** (main in-memory backend)
- **tibbles** (defers to data.table backend)
- base **data.frames** (defers to data.table backend)
- **DuckDB tables** (batch-based, R-preprocessing pipeline)

The package is built on the **S7 class system**, separating linkage
into:

1.  **A declarative search strategy** — defines *how* text fields should
    be normalized, tokenized, encoded, weighted, scored, and blocked.
2.  **Backend-specific execution** — defines *how* data is matched using
    the IR.

## File layout

Every file under `R/` carries one of eight prefixes that maps to a role.
Use the directory listing as a navigation tool — the prefix tells you
what kind of code is inside before you open the file.

| Prefix | Role |
|----|----|
| `strategy_*.R` | S7 strategy classes & their constructors (`Step`, `Search_Preparer`, smoothing family, `Search_Strategy`, `Embedding_Strategy`, `Exact_Strategy`, and the `Block_On_Tokens` token-blocking spec + `.block_cols()` resolver in `strategy_blocking.R`). |
| `preparer_*.R` | Step preparer functions (text → tokens), grouped by signature shape (`preparer_word.R` is text-in/text-out; `preparer_tokens.R` produces or operates on tokens). |
| `generics_*.R` | S7 generic declarations, grouped by lifecycle era (`generics_core.R`, `generics_calibration.R`, `generics_embedding.R`, `generics_diagnostic.R`). |
| `methods_<backend>_<stage>.R` | Token-backend dispatch, one file per (backend, workflow stage) — e.g. `methods_datatable_prepare.R`, `methods_duckdb_search.R`. Stages: `prepare`, `resolve`, `materialize`, `dedup`, `search`, `multistage`, `inspect`; DuckDB also has `methods_duckdb_batch.R` for the batching machinery. The `resolve` stage holds the shared connected-components entity kernel ([`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md)) that `dedup` delegates to. The `materialize` stage holds [`materialize_records()`](https://edubruell.github.io/joinery/reference/materialize_records.md), the rehydrate-by-id semi-join complement of [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md). |
| `embedding_methods_<backend>.R` | Embedding-backend dispatch (`embedding_methods_datatable.R`, `_duckdb.R`, `_tibble.R`). |

**DuckDB execution control
([`duckdb_control()`](https://edubruell.github.io/joinery/reference/duckdb_control.md),
v0.8 Stage 05).** All DuckDB backend tuning — batch sizes, scoring chunk
key, per-chunk failure policy, progress — lives on **one**
`Duckdb_Control` object (`R/duckdb_control.R`), passed as `control =` to
the DuckDB
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
/
[`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
/
[`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
methods. It **replaced** the old loose `target_batch_size` /
`min_batch_size` / `chunk_strategy` args (clean break). **Chunking is
execution, not semantics — it is never a `Search_Strategy` slot.**
DuckDB-only: the in-memory data.table backend never needed chunking (the
old `chunk_by` there was a matchmaker-era artefact). Two stages, two
atomicity rules: **preprocess batching** (tokenization) is per-row
(`duckdb_batch_plan(atomic_blocks = FALSE)`, any split safe); **scoring
chunking** (the overlap join in `search_candidates`) is *block-atomic*
(`atomic_blocks = TRUE` — a block is indivisible; splitting one drops
cross-pairs). `search_candidates` streams block-atomic chunks with a
per-chunk `tryCatch` boundary (`on_error = skip|retry|stop`), a globally
re-keyed `match_id`, §33 `chunk i/N` progress, and a compact
`attr(result, "failed_chunks")` record. Shared chunk-key resolution /
failure-log helpers live in `R/internal_chunking.R` (reused by
`multi_stage_search`, Stage 07). \| `diagnostic_*.R` \| Diagnostic verbs
(`diagnostic_audit.R`, `_summarise.R`, `_explain.R`, `_sample.R`,
`_compare.R`), their result S7 classes (`diagnostic_classes.R`),
recommendations catalog (`diagnostic_recommendations.R`), and plot
family (`diagnostic_plots.R`). \| \| `calibration_*.R` \| Calibration
verbs, helpers, and result S7 classes — `calibration_features.R` +
`_features_embedding.R`
([`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)),
`calibration_filter.R` + `_tidymodels.R`
([`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
/
[`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)),
`calibration_calibrate.R` + `_recipe.R`
([`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)
/
[`joinery_recipe()`](https://edubruell.github.io/joinery/reference/joinery_recipe.md)),
`calibration_dispatch.R`
([`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md)),
`calibration_labelling.R` (CSV round-trip), `calibration_aip.R` (`aIP`
primitive), `calibration_classes.R` (`Match_Features`, `Filter_Model`,
`Calibrated_Matches`, `Filter_Calibration`). \| \| `internal_*.R` \|
Cross-cutting utilities — `internal_validation.R` (validation helpers +
cli_abort exemplar), `internal_progress.R` (progress bars),
`internal_chunking.R` (DuckDB chunk-key / failure-log helpers),
`internal_staging.R` (the shared staged-linkage engine both
`multi_stage_dedup` and `multi_stage_search` wrap: strategy-list gate,
per-stage edge accumulation over original ids, residual/collapse
carry-forward, the search-grouping finalizer). \|

Conventional names that stay outside the schema: `joinery-package.R`,
`data.R`, and the vendored rlang shims (`import-standalone-*.R`).

`DESCRIPTION`’s `Collate:` field is hand-maintained. Any file rename,
split, or new file requires a manual `Collate:` update; S7 class
definitions and external generics must precede the files that declare
methods on them.

### Where to look for X

- **Add a new preparer function** → `preparer_word.R` if the function
  maps strings to strings, `preparer_tokens.R` if it produces or
  operates on tokens. Document via roxygen; the generic catalog lives in
  `notes/preparers_reference.md`.
- **Add a new diagnostic verb** → new generic in
  `generics_diagnostic.R`, new result class in `diagnostic_classes.R`,
  new verb file `diagnostic_<verb>.R`, plot helpers in
  `diagnostic_plots.R`, threshold rules in
  `diagnostic_recommendations.R`.
- **Add a new backend** → six `methods_<backend>_<stage>.R` files
  (`prepare`, `resolve`, `dedup`, `search`, `multistage`, `inspect`)
  plus optionally `embedding_methods_<backend>.R`. Add `Collate:`
  entries after the existing methods blocks (`resolve` before `dedup`,
  since `dedup` delegates to it).
- **Extend calibration** → verb code in `calibration_<verb>.R`; result
  classes in `calibration_classes.R`; generic declarations in
  `generics_calibration.R`. Tidymodels-specific code is isolated in
  `calibration_tidymodels.R` / `calibration_recipe.R` so the `Suggests`
  dependency boundary is visible.
- **Change error message style** → `internal_validation.R` carries the
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  exemplar; new code should follow that style.
- **Exact (score-1.0) token-set matching** → it is a **strategy**, not a
  verb:
  [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
  builds an `Exact_Strategy` (class + constructor in
  `R/strategy_exact.R`), and the standard apply verbs dispatch on it —
  [`detect_duplicates()`](https://edubruell.github.io/joinery/reference/detect_duplicates.md)
  (dedup face) and
  [`search_candidates()`](https://edubruell.github.io/joinery/reference/search_candidates.md)
  (cross face), implemented per-backend in `R/exact_methods_datatable.R`
  / `R/exact_methods_duckdb.R`, which also hold the shared
  fingerprint/containment kernel. Both return the **standard** schema
  with `score == 1.0`. The fingerprint rides
  [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
  (never a parallel fingerprint); the residual is the existing
  [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md),
  and staging is `multi_stage_dedup` / `multi_stage_search` composing
  `list(exact_strategy(...), search_strategy(...))`. `Exact_Strategy`
  mirrors `Embedding_Strategy` as a sibling strategy class.

## Core S7 Classes

### `Step`

Represents **one preprocessing step** (e.g.,
[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md),
`word_tokens(min_nchar=3)`,
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)).

Stores: - `name` – function name as string - `args` – list of
unevaluated expressions

### `Search_Preparer`

Represents the preprocessing pipeline for **one column**.

Holds: - `column` – column name - `steps` – ordered list of `Step`
objects (pipeline order matters)

### `Search_Strategy`

Top-level IR object defining how matching works.

Contains: - `preparers` – named list of `Search_Preparer` - `weights` –
named numeric vector (optional) - `block_by` – character vector
(optional), **or** a list mixing column names with
[`block_on_tokens()`](https://edubruell.github.io/joinery/reference/block_on_tokens.md)
specs (v0.9; `R/strategy_blocking.R`). A
`block_on_tokens(column, max_df, min_rarity, preparer, min_nchar)` spec
makes a record block on each of its own *rare* tokens of `column`
(region-free, drift-tolerant): in
[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
the token table is exploded against the surviving rare blocking-tokens
into a derived `._btok` block column, so two records sharing any rare
token co-block. The `.block_cols()` / `.plain_block_cols()` resolver in
`R/strategy_blocking.R` is the single source of truth: the token-overlap
join, rarity, fan-out guard, and scoring see `._btok` (via
`.block_cols()`); entity resolution and DuckDB batch/chunk slicing stay
on plain columns (via `.plain_block_cols()`) so a record matched under
several block-tokens resolves into **one** entity. `max_df` (the global
df cap selecting eligible block keys) reuses Feature B’s `df_global`; a
capless token block warns. Plain character `block_by` is unchanged. -
`rarity` – `"inverse_freq"` (default) or `"tfidf"` - `rarity_scope` –
`"block"` (default) or `"global"` (v0.9;
`notes/region_free_linking.md`). Selects whether token rarity is
measured within its block or corpus-wide. **The cost axis stays
block-local regardless:** `df`, `max_token_df`, and the fan-out guard
always read block-local `df`; only the rarity metric (and the
`min_rarity` distinctiveness floor) follows `rarity_scope`. Under
`"global"`,
[`compute_rarity()`](https://edubruell.github.io/joinery/reference/compute_rarity.md)
also computes `freq_global`/`df_global`/`N_global` (grouped without
block columns) and the rarity formula reads those. Global rarity lets a
distinctive brand read as a strong signal anywhere and is the chain
defense (a globally common name gets low global rarity, dropped by
`min_rarity`). Both backends; the `explain_match` round-trip holds under
global scope. - `min_rarity` / `max_token_df` – the two **pre-join** cut
levers (default `0` / `Inf`). Applied in one predicate to the token
table *before* the `(column, token, block)` overlap join on both
backends (`.rarity_prefilter_dt` / `.rarity_prefilter_sql`):
`min_rarity` floors the rarity metric, `max_token_df` caps raw document
frequency.
[`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
reads the distribution to set them. - `max_fanout` / `on_fanout` – the
**always-on fan-out guard** (default `5e7` / `"cap"`; v0.9,
`R/internal_fanout.R`). Where `min_rarity`/`max_token_df` are opt-in,
this is the default protection against a hot/boilerplate token fanning a
dense block into a quadratic overlap join (the v0.9 audit’s recurring
CRITICAL; `notes/v09_performance/`). It estimates the join’s
intermediate-row count (`Σ df·(df−1)` self / `Σ df_b·df_t` cross) from
the **pairs-free df histogram** and, when it busts `max_fanout`,
auto-derives a `df ≤ cut` ceiling dropping the smallest set of
near-zero-rarity hot tokens (`on_fanout="cap"`, loud `cli_warn`) or
aborts (`"abort"`); `"off"` disables. The cut is the **same df axis as
`max_token_df`** and **identical on both backends** (`.fanout_guard_dt`
/ `.fanout_guard_sql`), applied right after `.rarity_prefilter_*` in
dedup + search. It **superseded** the old dedup-only `max_comparisons`
method arg (clean break — block-level `Σ n(n−1)/2` estimate replaced by
the true token-level cost). - `threshold` – default match threshold

Constructed via:

``` r

search_strategy(
  column ~ step1 + step2 + step3,
  ...,
  weights = c(),
  block_by = NULL,
  rarity = "inverse_freq",
  threshold = 0.9
)
```

## Backend Generics

All generics dispatch on backend class:

``` r

prepare_search_data(data, id, strategy)
compute_rarity(tokens, strategy)
detect_duplicates(base_table, id, strategy, threshold = NULL)
search_candidates(base_table, target_table, base_id, target_id, strategy,
                  threshold = NULL, weights = NULL)
deduplicate_table(base_table, duplicates, id)
extract_unmatched(data, id, matches)
materialize_records(data, id, ids)
resolve_entities(edges, id_a, id_b, score = NULL, vertices = NULL, rep_by = NULL)
multi_stage_dedup(table, id, strategies, rep_by = NULL, edge_filter = NULL)
multi_stage_search(base_table, target_table, base_id, target_id, strategies)
```

[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
is the **dedup face** of the staged layer (single `table`, within-table
residual-reblocking passes, one final connected-components via
[`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md));
[`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
is the **search face** (the hard-rename of the former
`multi_stage_match`). Both are thin configs over the shared staged
engine in `R/internal_staging.R`; see the §34 dedup/search naming axis.
[`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)’s
residual carry-forward keeps each found group’s **representative** (a
real id) plus singletons, so a record that drifts away from a cluster
still bridges into it at a later, looser stage.

[`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
is the **central interpreter** for the IR and everything else builds on
its output.

## Search Workflow (7 Core Steps)

1.  **Preprocessing** —
    [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md)
    interprets all `Step` objects, produces long-form tokens, attaches
    block variables.
2.  **Rarity Computation** — measures token informativeness per
    column/block using inverse frequency, TF-IDF, or other metrics.
3.  **Token Overlap Join** — self-join (duplicates) or cross-join
    (candidates) on `(column, token, block_by)` to find record pairs.
4.  **Scoring (rIP)** — relative identification potential:
    `rIP = rarity / sum(rarity)` per record, then
    `score = sum(rIP * weight)`.
5.  **Thresholding** — keep pairs where `score >= threshold` (argument
    or strategy default).
6.  **Residual Generation** — extract unmatched records via
    [`extract_unmatched()`](https://edubruell.github.io/joinery/reference/extract_unmatched.md)
    for multi-pass workflows.
7.  **Staged entity resolution** — an ordered strategy list run as
    successive passes, accumulating links and resolving connected
    components via
    [`resolve_entities()`](https://edubruell.github.io/joinery/reference/resolve_entities.md).
    Two faces (§34):
    [`multi_stage_dedup()`](https://edubruell.github.io/joinery/reference/multi_stage_dedup.md)
    (within one `table`, one final CC, residual carry-forward keeps each
    group’s representative as a bridge) and
    [`multi_stage_search()`](https://edubruell.github.io/joinery/reference/multi_stage_search.md)
    (cross-source directed search → entity grouping + directed ledger,
    generic `source_by`, `collapse="rep"` for collapse-and-continue
    bridging). Both wrap the shared engine in `R/internal_staging.R`.

## Diagnostics

Five diagnostic verbs, organised around four user questions (Q1
will-it-work, Q2 did-it-work, Q3 why-this-pair, Q4 where-to-look) plus
multi-stage:

- [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)
  → `Strategy_Audit` (token strategies) or `Embedding_Audit` (embedding
  strategies) — dispatches on strategy class
- [`summarise_matches()`](https://edubruell.github.io/joinery/reference/summarise_matches.md)
  → `Match_Overview` (unified across dedup/candidates via `match_type`
  slot)
- [`explain_match()`](https://edubruell.github.io/joinery/reference/explain_match.md)
  → `Match_Explanation` (per-token attribution for `Search_Strategy`;
  pair+score only for `Embedding_Strategy`)
- [`sample_matches()`](https://edubruell.github.io/joinery/reference/sample_matches.md)
  → `Match_Sample` (modes: `high`, `low`, `borderline`, `ambiguous`,
  `top_gap`, `random`; also `stratify_by` and `expand_to_block` for
  stratified labelling-set construction)
- [`compare_stages()`](https://edubruell.github.io/joinery/reference/compare_stages.md)
  → `Stage_Comparison` (multi-stage diagnostics)
- [`rarity_distribution()`](https://edubruell.github.io/joinery/reference/rarity_distribution.md)
  → `Rarity_Distribution` (pre-match, read-side; v0.8 Stage 04).
  Scoring-free
  [`prepare_search_data()`](https://edubruell.github.io/joinery/reference/prepare_search_data.md) +
  [`compute_rarity()`](https://edubruell.github.io/joinery/reference/compute_rarity.md)
  only — no overlap join — reporting the per-`(column[, block])`
  df/rarity distribution and the top-df **offender list** (fan-out
  drivers), plus a `suggested_min_rarity` per column. Use it to *set*
  the `min_rarity` / `max_token_df` levers from the real token
  distribution. Lives in `R/diagnostic_rarity.R`; the cheap seed that
  [`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)
  subsumes.
- `plan_strategy(base, strategy, target = NULL, block_candidates, base_id, target_id)`
  → `Strategy_Plan` (pre-match, **pre-strategy**; v0.8 Stage 08). The
  verb that helps you *find* a strategy rather than grade one —
  **upstream of and distinct from
  [`audit_strategy()`](https://edubruell.github.io/joinery/reference/audit_strategy.md)**
  (multi-block, scoring-free, dispatches on `c("base", "strategy")`; the
  strategy supplies only the preparer pipeline, its `block_by` is
  ignored — `block_candidates` is the thing being chosen).
  **Deliberately scoring-free**: no overlap join, no `.score_*`; every
  probe is `O(rows)`/`O(blocks)` arithmetic. Four reads: (1) the
  **blocking-resolution frontier** — per candidate: `#blocks`, size
  distribution, `Σ(na·nb)` brute-pair **count** (cost axis, arithmetic),
  and the share of exact-token-set twins that stay co-blocked (recall
  axis), using A2’s *faithful* fingerprint (`.exact_fp_wide_dt` via the
  exact proxy, never re-rolled); (2) the **exact-set persister rate**
  (A2 yield — does an
  [`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md)
  front absorb enough to stage?); (3) **residual structure** (matchable
  / one-sided / per-column partial-recoverable); (4) **per-column
  discriminativeness** — `.rarity_distribution_core` + a
  `min_rarity → intermediate-overlap-row` cost curve (pure df-histogram
  math; matches an independent overlap-row count exactly) + the §25
  **empty-column score-ceiling** read (`1 − normalized weight(col)`) +
  an **opt-in** §22 containment share (`containment = TRUE` — the one
  read that does a bounded structural join; `NA` by default so the
  scoring-free guarantee holds). DuckDB samples with `SELECT *` and
  delegates to data.table (no pairs touch the connection — the
  scoring-free guard). Lives in `R/plan_strategy.R` (class colocated,
  like `diagnostic_rarity.R`); default
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) is
  [`frontier_plot()`](https://edubruell.github.io/joinery/reference/frontier_plot.md).
  The Part-A spine’s final stage.

Recommendations live in `R/diagnostics_recommendations.R` (signal →
threshold → message), surfaced via inline `cli` warnings in
[`print()`](https://rdrr.io/r/base/print.html) and the
`recommendations(x)` accessor.
[`plan_strategy()`](https://edubruell.github.io/joinery/reference/plan_strategy.md)
adds four: `blocking_knee` (coarser candidate near-lossless for twins
but materially cheaper), `empty_column_ceiling` (§25),
`consider_containment` (§22, opt-in), and `est_comparisons_too_high`
(cheapest candidate still too dense).

Plotting is first-class via `tinyplot` (hard `Imports` dependency).
Diagnostic verbs return data only. Each plot is a separately named
function (no `plot(x, type=...)`); pipe-composable:
`summarise_matches(m) |> score_histogram()`. Default
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods per
class call the most-useful single view.

The `explain_match` round-trip contract
(`sum(per_column_contrib$contribution) × feedback_factor == score`,
exact to 1e-10 with no feedback) is mandatory on both backends — it is
the property test that prevents scoring drift.

## Calibration Primitives

- **`prepare_auxiliary_registry()`** — internal generic. Builds a
  per-column token-occurrence registry on the auxiliary (search /
  target) side. Block-agnostic and cross-table by construction (distinct
  from the per-block retrieval-time
  [`compute_rarity()`](https://edubruell.github.io/joinery/reference/compute_rarity.md)).
- **`compute_aip()`** — internal helper. Implements Doherr (2023)
  eq. (9) on a pair of registries, producing `aip` per
  `(src_column, token)`. Consumed by
  [`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md).
  Lives in `R/aip.R`; design in `notes/calibration_design.md`.
- **[`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
  /
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)**
  — exported verbs. CSV round-trip for manual labelling of a
  `Match_Sample`.
  [`export_for_labelling()`](https://edubruell.github.io/joinery/reference/export_for_labelling.md)
  pre-fills `equal` on block-header rows (base-side rows for candidates,
  rank-1 rows for dedup) using `default_label = 1L` (use `0L` for the
  inverse workflow); writes a flat CSV with `equal` placed first.
  [`import_labels()`](https://edubruell.github.io/joinery/reference/import_labels.md)
  reads back, propagates the block-default `equal` onto unmarked rows,
  validates schema, returns a `data.table` ready for
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  /
  [`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md).
  Format-agnostic; no UI shipped. Lives in `R/labelling.R`; design in
  `notes/calibration_design.md`.
- **[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)**
  — exported verb. Builds a wide one-row-per-pair feature `data.table`
  from a joinery match result. Dispatches on strategy class.
  `Search_Strategy` returns the full token schema (`searched`, `found`,
  `match_id`, `stage`, `score`, `cnt`, `icnt`, `ipos`, `stage_*`,
  `scnt`, `rcnt`, `r1..rn`, `m_<col>_*`, `f_<col>_*`, `s_<col>_*`,
  `sim_sf_<col>`, `sim_fs_<col>`). `Embedding_Strategy` returns the
  reduced schema (core + `stage_*` + `sim_sf_*` / `sim_fs_*` +
  `cosine_sim` + `embedding_norm_s` + `embedding_norm_f`). String
  similarity uses
  [`stringdist::stringsim()`](https://rdrr.io/pkg/stringdist/man/stringsim.html)
  with a single global `method =` argument (default `"jw"`); pass
  `include_string_sim = FALSE` to opt out on minimal installs. Embedding
  norms are L2 norms of the **pre-normalization** embeddings, recomputed
  only over the matched subset under a temporary strategy with
  `normalize = FALSE`. Column order is the public API — additions only,
  never reorder or rename. Lives in `R/match_features.R`; schema in
  `notes/calibration_design.md`.
- **[`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  /
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
  /
  [`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md)**
  — exported verbs. Post-retrieval false-positive filter.
  `fit_filter(features, labels, model = "logistic", class_weighted = FALSE, na_fill = 0)`
  joins a `Match_Features` to a labels `data.table` on
  `(match_id, found)`, fits a logistic `glm`, and returns a
  `Filter_Model` carrying the fitted model plus training probabilities /
  labels / per-stage distribution.
  `apply_filter(features, filter_model, threshold = NULL, matches = NULL)`
  scores features, picks the threshold via Youden’s J on training data
  when `threshold` is `NULL`, and returns a `Calibrated_Matches` whose
  `@matches` slot either holds the enriched features table or — when
  `matches =` is supplied — the original raw matches table with
  `tp_prob` / `predicted_tp` broadcast onto every row of the pair
  (candidates: by `match_id`; duplicates: by
  `(duplicate_group, id == found)` on rank-k rows only).
  `calibrate_matches(matches, strategy, labels, base, id, target, target_id, ...)`
  is the high-level verb that composes
  [`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
  →
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  →
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md);
  it dispatches on `(matches, strategy)` for data.table, DuckDB
  (collect-and-delegate), and tibble / data.frame inputs. Threshold
  selection defaults to Youden’s J; user supplies `threshold =` to
  override. Lives in `R/fit_filter.R` and `R/calibrate_matches.R`;
  design in `notes/calibration_design.md`.
- **[`calibrate()`](https://edubruell.github.io/joinery/reference/calibrate.md)
  / `Filter_Calibration`** — exported verb. Evaluates a fitted
  `Filter_Model` (carried on a `Calibrated_Matches`) on a labelled set
  and returns a `Filter_Calibration` with reliability table, Brier
  score, log-loss, per-class confusion matrix, and threshold sweep
  curve. `calibrate(cm)` uses the training labels stored on the
  `Filter_Model`; `calibrate(cm, labels)` evaluates on an independent
  labelled set (dispatches on candidates vs duplicates by inspecting
  `@matches`). Surfaces `calibration_low_n_warning` from the
  recommendations catalog. Lives in `R/calibrate.R`.
- **[`joinery_recipe()`](https://edubruell.github.io/joinery/reference/joinery_recipe.md) +
  tidymodels
  [`fit_filter()`](https://edubruell.github.io/joinery/reference/fit_filter.md)
  path** — exported. `joinery_recipe(features, labels)` returns a
  [`recipes::recipe`](https://recipes.tidymodels.org/reference/recipe.html)
  with `searched` / `found` / `match_id` tagged as role `"id"` and
  `equal` as the outcome.
  `fit_filter(model = <parsnip spec | fitted parsnip | (un)fitted workflow>)`
  accepts tidymodels objects and wraps them in a `Filter_Model` (backend
  = `"parsnip"` / `"workflow"`); fitted workflows are detected via
  [`workflows::is_trained_workflow()`](https://workflows.tidymodels.org/reference/is_trained_workflow.html)
  and not re-fit.
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
  scores via `predict(type = "prob")` and reads the `.pred_1` column
  (training fixes `equal` to `factor(c(0L, 1L))`). All tidymodels
  packages are in `Suggests`;
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) guards
  keep the baseline glm path dependency-free.
- **Calibration-related recommendations** — four entries in
  `R/diagnostics_recommendations.R`: `consider_calibration_borderline`
  (fires from `summarise_matches(matches, threshold = ...)` when
  `pct_pairs_borderline > 0.10`), `consider_calibration_ambiguity`
  (fires from `Match_Overview` when
  `pct_records_with_ge3_matches > 0.20`), `calibration_low_n_warning`
  (fires from `Filter_Calibration` when training_n \< 500), and
  `calibration_drift_warning` (fires from
  [`apply_filter()`](https://edubruell.github.io/joinery/reference/apply_filter.md)
  when stage-distribution TV distance vs training \> 0.15).
  `summarise_matches(threshold = NULL, borderline_epsilon = 0.05)` is
  the entry point on both data.table and DuckDB backends.

## Expected Output Schemas

### Duplicate Detection

    duplicate_group
    score
    id
    <original columns of base_table>
    rank

### Cross-Table Candidate Matches

    match_id
    score
    source     # "base" or "target"
    id
    <original columns>
    rank

## Key Principles for Coding in joinery

- **Do not assume a specific backend.** Use generics; implement new
  methods as needed.

- **Token tables are the universal interface:**

      id | column | token | row_id | <block_by>

- **Scoring uses rIP internally** — `sum(rarity * weight)`.

- **Thresholding is applied after scoring.**

- **Output must follow the schemas above.**

- **Multi-stage matching is sequential** — matches → extract residuals →
  next strategy.

- **Clean break to 1.0 — no deprecation shims.** joinery is pre-1.0,
  unreleased, and solo-developed; there are no external callers. Renames
  and signature changes are made *outright*: rename the thing, update
  every internal reference (including the untracked `localwip/yp_panel/`
  scripts), delete the old name. **No deprecated aliases, no
  [`lifecycle::deprecate_warn()`](https://lifecycle.r-lib.org/reference/deprecate_soft.html),
  no `lifecycle` dependency, no “kept for compat” code paths, no
  back-compat/deprecation-warning tests.** After a rename,
  `grep -r <old_name> R/ tests/ man/` must be empty. The goal is a
  clean, stable 1.0; every alias kept now is a `@deprecated` we’d carry
  past 1.0.

## Testing Policy

- Run normal package tests with `Rscript -e "devtools::test()"`.
- Run coverage with `Rscript -e "covr::package_coverage()"` when `covr`
  is installed.
- Add ordinary `testthat` tests for small deterministic cases,
  validation errors, backend parity, scoring branches, and output
  schemas.
- Do not put large DuckDB jobs, stress tests, provider-dependent
  embedding tests, or expensive benchmarks in `tests/testthat/`.
- Put those larger checks in `local_tests/`; they are intentionally
  local and excluded from package builds/checks via `.Rbuildignore`.
- Keep `examples/`, `localwip/`, `notes/`, and `joinery.Rproj` local
  unless explicitly requested otherwise.

## Reference Documentation

For detailed guidance on specific topics, consult:

**Core Architecture:** - **`notes/architecture.md`** — Data.table
backend internals, token table schema, rarity & scoring details. -
**`notes/preparers_reference.md`** — Complete catalog of text
normalization, phonetic encoding, token generation, and token
transformation functions.

**DuckDB Backend:** - **`notes/duckdb_status.md`** — Implementation
status, completed features, test coverage, known limitations. -
**`notes/duckdb_coding_guide.md`** — Practical guide for using and
extending the DuckDB backend. - **`notes/duckdb_backend.md`** — Design
philosophy, batch execution architecture, no-SQL-translation approach. -
**`notes/duckdb_performance.md`** — Performance tuning guide, batch size
recommendations, optimization strategies.

**Current Work (v0.9 — documentation push to 1.0):** - **`notes/v09/`**
— **The docs build queue** (the current focus). Where `notes/v08/` was a
build queue for *code*, this is the build queue for *docs*: the public
surface (README, getting-started vignette, articles, reference pages,
the pkgdown site) gating a 1.0 release. `00_index.md` is the
single-source-of-truth status table (Diátaxis frame, house-voice rules);
`TODO_docs_pass.md` is the live checklist. Shipped so far: pkgdown
skeleton, getting-started vignette (`vignettes/joinery.Rmd`), README
refresh, the workshop example data
(`workshop_register`/`workshop_listings`), the features article, the
embeddings article (drafted), and the v0.9 `NEWS.md`. **Remaining 1.0
gates:** the P1 example data (`workshop_panel` +
`match_labels_example`), the calibration and DuckDB articles, glossary
render, navbar wiring, and a docs-QA pass. House voice is mandatory
(\[\[feedback_docs_writing_style\]\],
\[\[feedback_woodworking_voice\]\]): no em dashes, problem-first, plain
language, no internal jargon (`§`/file-path/ stage refs) in user prose.
**Start here for any docs work.** - **`notes/post_release/`** —
Forward-looking scoping notes for *after* 1.0 (ecosystem/positioning,
engine/algorithm extensions, backends/scale/interop, a
probabilistic-strategy addendum, an implementation roadmap). Not 1.0
work; read only when planning beyond the release.

**Background — v0.8 (the now-shipped Part-A code spine + the YP stress
test):** - **`notes/yp_panel_joinery_plan.md`** — Plan for the full
German Yellow-Pages panel (all years, all branches) on joinery’s DuckDB
backend, blocked by `(plz2, wz08_3)`. The first real-life stress test of
v0.8. A **frozen v1 hand-rolled baseline panel now exists** — built by
the `localwip/yp_panel/` scripts (single-shot → fuzzy dedup → Stage-E
one-year extra-merge → `48_build_panel.R`): **51,315,109 rows \|
7,468,975 entities \| 2.0% imputed (K=5)**. v1 is the comparison target
for the declarative rebuild (\[\[v08_second_pass_yp\]\]). **Read this
before doing any YP-related work.** - **`notes/v08/`** — **The build
queue.** Turns `v08_implementation_plan.md` Part A into an ordered set
of PR-sized, buildable implementation stages (`00_index.md` is the
single-source-of-truth status table + conventions; `01`–`08` are the
staged plans). Each stage carries exact <file:line> targets, new
signatures, Collate edits, a test plan, and a fresh-context audit
prompt. The spine (01 `resolve_entities` → … → 08 `plan_strategy`) is
**complete: 01–08 all implemented and fresh-context audited (GO)** as of
2026-06-12. The naming axis (§34): staged verbs pair on dedup/search —
`multi_stage_dedup` (06) / `multi_stage_search` (07, the hard-rename of
the former `multi_stage_match`), both thin configs over
`R/internal_staging.R`. One carried-forward follow-up stays open:
Stage-01 \#2 (DuckDB `resolve_entities` coverage gap for the
empty-without-vertices type + untested `vertices` singleton-fold
INSERT). The remaining v0.8 work is the independent Part B–D track
(calibration/preparer/recommendation polish) and the Part-A acceptance
test. **Start here before implementing any Part-A verb.** -
**`notes/v08_lessons.md`** — Running log of design-relevant insights
surfaced *while* running the YP build. Captures rough edges in
`block_by`, audit verbs, progress reporting, docs framing. The arc lands
at §29–§35: §29–§31 named the **one feature** (staged entity resolution)
that became the now-shipped Part-A spine; §32 (dedup & search are two
tails on one scoring kernel), §33 (chunked processing must announce the
current chunk), §34 (composite verbs name along the dedup/search axis),
§35 (cross-year self-search exact front must be rarity-free,
\[\[feedback_self_search_rarity_free_front\]\]). Most actionable items
have landed (Part A 01–08); the doc is now mostly a historical design
log plus the live §35 lever. - **`notes/v08_implementation_plan.md`** —
Consolidated, actionable v0.8 package plan distilled from the full YP
build. Opens with a status ledger of what already shipped (the §10–§16 /
§18-core dedup pass: empty-result schema, filtered-lazy materialise,
block-aware CC, cardinality recommendation, token-set scoring,
non-unique-id crash). The live backlog is regrouped around one insight
(§29/§30): the open findings are not scattered bugs but **one feature —
staged entity resolution** (exact → fuzzy → residual → resolve) that the
YP scripts hand-rolled. **Part A** is that headline kernel
(`resolve_entities`,
[`exact_strategy()`](https://edubruell.github.io/joinery/reference/exact_strategy.md),
bounded/failure-isolated `search_candidates`, rarity prefilter,
`materialize_records`, `multi_stage_dedup`, `multi_stage_search`,
`plan_strategy`) — **now fully shipped & audited (01–08, see
`notes/v08/00_index.md`)**; **Part B** scorer/calibration correctness;
**Part C** preparer vectorization; **Part D**
recommendations/guardrails. The staged, buildable form of Part A lives
in `notes/v08/`. Each item names the exemplar `localwip/yp_panel/`
script to absorb, plus fix shape, test plan, and a fresh-context audit
prompt. Read this before touching `R/methods_duckdb_*`,
`R/methods_datatable_*`, `R/calibration_*`, or
`R/diagnostic_recommendations.R`. - **`notes/v08_second_pass_yp.md`** —
Blueprint for the *clean rebuild* of the YP panel on the Part-A verbs
(“first do a merge, learn, redo it simpler”). **Now implemented** by
`localwip/yp_panel/second_pass/` (`yp00_common.R` + `yp01`–`yp04`),
slice-validated; full-corpus run staged for the big-disk laptop
(`localwip/yp_panel/MIGRATION.md`). Per-listing `year_row` nodes
carrying full payload (coords, raw branche, contact) end-to-end → no
late raw-data restitch. Three phases: (1) **within-year dedup**
(year-blocked) → independent within-year entities; (2) **multi-stage
`search_candidates`** over all years pooled, within block, with
**collapse-and-continue** between stages (matched groups collapse to one
rep, shrinking the search space for later looser stages) — NOT
`multi_stage_dedup` (cross-year is a directed *search*, where
containment asymmetry + year-pair-per-edge live); (3)
**within-trajectory imputation** (K=5). Core design points: a typed
membership ledger (`year_row → entity`, `within_year`/`cross_year`) is
the caller’s artifact and the source of covered-years/`n_listings`; the
between-stage transition (collapse / rebind / direction) is shared
machinery (§32: dedup & search are two tails on one kernel);
`rebind = accumulate` is the path for incremental panel updates. Read
before designing the rerun or touching the staged verbs. -
**`notes/yp_panel_legacy_workflow.md`** — Retrospective of the legacy
MatchMakeR pipeline that built the existing Hausärzte panel; raw-data
layout, blocking decisions, output schemas, and which legacy decisions
to carry over or revisit. Source data lives at
`/Users/ebr/Dropbox/medlaborsupply/gelbe_seiten_raw/` (raw text + cached
parquet at `.../pqt/`); legacy scripts at
`/Users/ebr/Dropbox/medlaborsupply/gelbe_seiten_raw/read_yellow_pages.R`
and
`/Users/ebr/Dropbox/medlaborsupply/entry_regulations_R/{deduplicate_yp_data,create_yp_panel}.R`. -
**Working folder for YP-test scripts and intermediates:**
`localwip/yp_panel/` (untracked; `localwip/` is in both `.gitignore` and
`.Rbuildignore`). Already contains `wz08_3_labels.csv` (274 WZ08-3 group
codes + English labels, extracted from
`localwip/Labels_SIAB_7523_v2_btr_basis_en.log`). Branche → WZ08-3
classifier (Phase 1) is **built** (`localwip/yp_panel/build_wz08_map.R`,
tidyllm/Claude) and baked into the DuckDB `atom_map` table the build
reads.

**Project Planning:** - **`notes/code_quality_pass.md`** — Completed
code-quality refactor (May 2026): unified naming schema and
error/validation styles now in place across the package. Reference for
the conventions new code should follow. - **`notes/roadmap.md`** —
Strategic roadmap, feature priorities, current phase. -
**`notes/embedding_design.md`** — Implementation design for
embedding-based matching. - **`notes/diagnostics_design.md`** — Design
notes for the diagnostics verbs. - **`notes/region_free_linking.md`** —
Plan for region-free linking (chains & movers): token-blocking
(`block_on_tokens`), `rarity_scope = "global"`, and the temporal-overlap
`edge_filter` guard. Three jobs — reachability / selectivity /
disambiguation — with <file:line> anchors, the df-decoupling subtlety
(block-local cost df vs global rarity df), and a phased rollout.
Motivated by the YP cross-year mover/chain recall tail
(`localwip/yp_panel/yp05–yp09`). - **`notes/calibration_design.md`** —
Calibration design: post-match false-positive filter, `aIP` primitive,
[`match_features()`](https://edubruell.github.io/joinery/reference/match_features.md)
schema,
[`calibrate_matches()`](https://edubruell.github.io/joinery/reference/calibrate_matches.md)
verb, tidymodels boundary. - **`notes/searchengine_lessons_for_v07.md`**
— Distilled lessons from Doherr’s SearchEngine whitepaper that informed
the calibration design. - **`notes/HolisticMatching.md`** — OCR’d full
text of the SearchEngine whitepaper (Doherr 2023) for reference. -
**`notes/batch_duckdb_brittleness.md`** — Investigation and fix notes
for the small-table batch_map brittleness.
