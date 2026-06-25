## Resubmission

This is a resubmission of the first submission of joinery (version 1.0.0).
The previous automatic incoming check reported one issue on the Debian
flavor: the `detect_duplicates` example used more CPU than elapsed time
(data.table multi-threading). All examples now cap data.table to two
threads (`data.table::setDTthreads(2)` in a `\dontshow{}` preamble), so
example CPU time stays within the elapsed-time limit.

joinery is a heuristic, index-based record linkage system for R. It links
records that refer to the same entity (people, firms, addresses) across messy
sources with no shared key: spelling drift, abbreviations, reordered tokens,
phonetic variants, and partial information.

Highlights:

* **A declarative strategy IR.** A strategy is an S7 object describing how each
  field is normalized, tokenized, encoded, weighted, blocked, and scored. The
  same object drives every backend and verb, so what a join *is* stays separate
  from how it runs.
* **Stepwise linkage.** Exact, fuzzy, and embedding strategies compose as an
  ordered list run as successive passes, carrying residuals forward and
  resolving entities once at the end.
* **Efficient and out-of-core.** data.table by default; the same strategy runs
  on a DuckDB backend with batched, block-atomic execution and an always-on
  cost guard. Used to build a panel of tens of millions of rows.
* **Explainability first-class.** `explain_match()` attributes a score token by
  token, with a sum-to-score round-trip enforced as a property test, plus
  diagnostic verbs and an optional false-positive calibration filter.

Documentation, articles, and a function reference are on the package website:
https://edubruell.github.io/joinery/

## Test environments

* local macOS, R 4.5.3
* win-builder, R-devel (R 4.6.x) and R-release (R 4.6.1)

## R CMD check results

0 errors | 0 warnings | 0 notes locally.

On win-builder (devel and release) the only NOTE is the expected "New
submission" note, as this is the first submission to CRAN.

## Notes for CRAN

* All packages used in examples, tests, and vignettes that are listed in
  Suggests are used conditionally (guarded by `requireNamespace()` or
  skipped in tests when absent), so the package checks cleanly when they
  are not installed.
* Examples that would require network access or a long-running DuckDB job
  are wrapped in `\donttest{}` / `\dontrun{}` as appropriate.
</content>
