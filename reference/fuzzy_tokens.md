# Collapse near-duplicate tokens to a canonical form

Typos and minor spelling differences split one real token into many
(`"Neumann"`, `"Neumann"` with a slip, `"Neuman"`). `fuzzy_tokens()`
finds tokens within a string distance of each other, groups them, and
rewrites every member of a group to one canonical spelling, so the
variants match. Unlike
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md),
which needs a known synonym list, this discovers the groups from the
data.

## Usage

``` r
fuzzy_tokens(x, max_dist = 2, method = "osa", min_nchar = 1)
```

## Arguments

- x:

  A character vector to tokenize and canonicalize.

- max_dist:

  Maximum string distance for two tokens to be treated as the same. For
  `method = "jw"` this is a Jaro-Winkler distance in `[0, 1]` (smaller
  is stricter); for edit-distance methods it is a count of edits.

- method:

  A
  [`stringdist::stringdist()`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
  method, e.g. `"osa"` (default), `"lv"`, or `"jw"`.

- min_nchar:

  Minimum token length to consider; shorter tokens are dropped before
  grouping.

## Value

A list of character vectors, one per input element, with each token
replaced by its group's canonical form.

## Details

Use it when a field has organic spelling noise and you do not have a
dictionary. The canonical form per group is the longest token, breaking
ties by the most central token, then alphabetically.

When not to use it:

- **High-cardinality columns.** It compares every distinct token against
  every other in one dense distance matrix, so cost and memory grow with
  the square of the number of distinct tokens. On a large vocabulary
  (tens of thousands of distinct tokens and up) it is slow and
  memory-hungry. Normalize aggressively first, and prefer
  [`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)
  when the groups are already known.

- **When over-merging is costly.** Grouping is by connected components,
  so matches chain transitively: if `A` is close to `B` and `B` to `C`,
  all three collapse even when `A` and `C` are far apart. A loose
  `max_dist` or short tokens can fuse genuinely distinct values. Keep
  `max_dist` tight, raise `min_nchar` to drop noise-prone short tokens,
  and check the groups on a sample before trusting them.

## See also

[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)
when the groups are known in advance.

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
[`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)

## Examples

``` r
fuzzy_tokens(c("Neumann", "Neumaxn", "Neuman"), max_dist = 2)
#> [[1]]
#> [1] "NEUMANN"
#> 
#> [[2]]
#> [1] "NEUMANN"
#> 
#> [[3]]
#> [1] "NEUMANN"
#> 
# every row's token becomes "NEUMANN"
```
