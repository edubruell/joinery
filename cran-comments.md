# joinery 1.1.0

This release adds subword matching (`find_subwords()`, `subword_tokens()`,
`custom_tokens()`), an `identity_by` argument for `match_features()`, and a
fix so that an empty `detect_duplicates()` result keeps its columns.

It also makes joinery compatible with the next S7 release. Tomasz
Kalinowski (S7 maintainer) modified some code to be fully comptabile with the dev S7. 

`sentencepiece` is a new suggested package. All uses are guarded.

## Test environments

* local macOS (arm64), R 4.5.3, with CRAN S7 0.2.2 and with S7 0.2.2.9000

## R CMD check results

0 errors | 0 warnings | 0 notes
