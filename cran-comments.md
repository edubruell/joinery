## Submission

This is the first submission of joinery to CRAN (version 1.0.0).

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
* (add: R-hub and win-builder devel/release before submission)

## R CMD check results

0 errors | 0 warnings | 1 note

The one NOTE is "unable to verify current time", an environment artifact of the
local check machine having no network access to a time server. It is unrelated
to the package and does not appear on CRAN's build machines.

## Notes for CRAN

* All packages used in examples, tests, and vignettes that are listed in
  Suggests are used conditionally (guarded by `requireNamespace()` or
  skipped in tests when absent), so the package checks cleanly when they
  are not installed.
* Examples that would require network access or a long-running DuckDB job
  are wrapped in `\donttest{}` / `\dontrun{}` as appropriate.
</content>
