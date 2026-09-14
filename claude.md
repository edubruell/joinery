# joinery — Developer / Coding-Agent Guide

**joinery** is a heuristic, token-based record linkage package for R (CRAN 1.0.1,
main at 1.0.1.9000). Built on S7: a declarative `Search_Strategy` (how text is
normalized, tokenized, weighted, scored, blocked) is executed by backend-specific
methods for data.table (tibble / data.frame defer to it) and DuckDB (batched,
R-preprocessing pipeline). Sibling strategy classes: `Embedding_Strategy`,
`Exact_Strategy`. Architecture, verbs, schemas and design history live in the
wiki (below); this file is the map.

## File layout (`R/` prefixes)

| Prefix | Role |
|---|---|
| `strategy_*.R` | S7 strategy classes + constructors (`Step`, `Search_Preparer`, `Search_Strategy`, `Embedding_Strategy`, `Exact_Strategy`, `Block_On_Tokens` in `strategy_blocking.R`) |
| `preparer_*.R` | Step preparer functions: `preparer_word.R` text→text, `preparer_tokens.R` produces/operates on tokens, `preparer_subword.R` SentencePiece subwords |
| `generics_*.R` | S7 generic declarations by era (`core`, `calibration`, `embedding`, `diagnostic`) |
| `methods_<backend>_<stage>.R` | Token-backend dispatch per (backend, stage): `prepare`, `resolve`, `materialize`, `dedup`, `search`, `multistage`, `inspect`; DuckDB adds `batch` |
| `exact_methods_<backend>.R` | `Exact_Strategy` dispatch (score-1.0 token-set matching) |
| `embedding_methods_<backend>.R` | `Embedding_Strategy` dispatch |
| `diagnostic_*.R`, `plan_strategy.R` | Diagnostic verbs, result classes, recommendations catalog, tinyplot plots |
| `calibration_*.R` | `match_features`, `fit_filter`/`apply_filter`, `calibrate`, labelling round-trip, aIP |
| `internal_*.R` | Cross-cutting: validation (cli_abort exemplar), progress, chunking, fan-out guard, the shared staging engine |
| `duckdb_control.R` | `Duckdb_Control`: all DuckDB batch/chunk/failure/progress tuning |

`DESCRIPTION`'s `Collate:` is hand-maintained: any new or renamed file needs a
manual entry, and class definitions and generics precede the files with methods
on them. Vendored rlang shims are `import-standalone-*.R`.

## Key principles

- Do not assume a backend. Add methods on the generics; keep data.table, DuckDB
  and tibble in lockstep with parity tests.
- Token tables are the universal interface: `id | src_column | token | row_id | <block_by>`.
- Scoring is rIP (`rarity / sum(rarity)` per record, `score = sum(rIP * weight)`; four rarity metrics: `inverse_freq`, `smoothed_inverse_freq`, `tfidf`, `bm25`),
  thresholded after scoring. The `explain_match` round-trip
  (`sum(contribution) × feedback_factor == score`) is a mandatory property test.
  It is asserted at 1e-10 on data.table only; the DuckDB case compares score and
  per-column contributions against data.table at 1e-6 and never sums them. A
  DuckDB-side sum assertion is still owed.
- Output schemas are fixed. The `merge(by = "id")` that attaches the original
  data puts `id` first: dedup `id, duplicate_group, score, rank, <cols>`;
  search `id, match_id, score, source, <cols>, rank`. Empty results keep the
  full typed schema on DuckDB only, see wiki contention C5.
- A matching variant is a strategy class that the standard apply verbs dispatch
  on, never a standalone verb.
- Chunking is execution, never a strategy slot. DuckDB only.
- Public API is frozen since 1.0: add, never rename or break. Before 1.0 the rule
  was clean-break renames with no deprecation shims, no `lifecycle`; keep that
  spirit for internals.

## Testing policy

- `Rscript -e "devtools::test()"` for the suite; `covr::package_coverage()` when
  installed. Small deterministic cases, validation errors, backend parity,
  scoring branches, output schemas go in `tests/testthat/`.
- Large DuckDB jobs, stress tests, provider-dependent embedding tests, and
  benchmarks go in `local_tests/` (tracked, `.Rbuildignore`d, run by hand).
- `sentencepiece` and all tidymodels packages are `Suggests`: every touch point
  gets a `requireNamespace()` guard and tests use `skip_if_not_installed()`.

## Working rules (maintainer feedback)

- Docs voice is a per-change gate: no em dashes anywhere in prose or roxygen,
  plain language, problem-first, no architecture jargon (dispatch, IR, stage
  numbers, file paths) in anything a user reads. Light woodworking motif only at
  the seams. README is the pkgdown home page; include it in every doc pass.
- Commit messages carry no co-author or attribution lines.
- After a stage lands, a fresh-context panel audits it; do not self-evaluate.
  Five lanes plus the wiki lint, the pre-checks and the verdict folder are
  specified in `local_context/wiki/11_audit_protocol.md`. It is a convention, not
  a skill: run the fan-out by hand from that note.
- `internal_progress.R` is a deliberate wrapper over cli; do not flag it as
  redundant.
- Drop orphan `_joinery_*` DuckDB temp tables periodically; the backend leaks
  them on crash.
- tinyplot `xaxl` takes a mapper function, never a character vector.
- Firm-panel calibration: prefer a recall-favouring cut near 0.30 over Youden's J.
- Cross-year self-search: the first (exact) stage must be rarity-free, or
  persistent entities suppress their own tokens.

## Local folders (untracked)

- `local_context/wiki/` the agent-facing memory (below).
- `local_context/notes/` dated journal entries.
- `local_context/legacy/` the pre-wiki planning notes (`notes/`, 80 notes) and
  loose scratch (`localwip/`); indexed in wiki note 09. Read-only history.
- `local_context/yp_panel/` the Yellow Pages panel workbench (6.3 GB: v1 script
  series, calibration CSVs, `second_pass/` declarative rebuild, parquet files).
- `examples/`, `local_tests/`, `docs/` stay where they are.

## Project memory (llmwiki)
- Wiki root: local_context/wiki
- Journal: local_context/notes
- Kind: software
- Sweep cutoff: 2026-09-14
- Trigger cutoff: 2026-09-14
- Trigger: audit | local_context/audits/* | marker:verdict.md
- Schema + workflows: `/llmwiki` skill.
- **Read `local_context/wiki/00_state.md` first in every session.** Open numbered
  notes on demand via its pointers; do not re-derive settled design.
- Before touching design: check `local_context/wiki/contentions.md`; never
  silently resolve an open contention, and never sit on a ripe one: `close`
  brings contentions whose resolve-condition is met (or which 3 sessions have
  not moved) back to the user with a recommendation.
- Corrections: edit the owning note FIRST and stamp it
  (`<!-- swept: anchor date -->`), then the register line as receipt
  (`Affects: [[NN_note#anchor]]`).
- Session end / before compact: run `/llmwiki close`; first thing after a
  `/clear`, run `/llmwiki next` to pick up `00_state.md`'s Next list.
- Artefacts that land (payload rounds, exports, transcripts) can nag until the
  wiki has them: `/llmwiki trigger` sets one, `close` discharges it.
- Run `/llmwiki lint` after: design adoption/supersession, a results round,
  a payload round returning, CLAUDE.md edits, or 5+ sessions without one.
