# Map tokens to canonical groups with a lookup table

When you already know which tokens mean the same thing (a curated
synonym list, brand-name variants, a code-to-label table),
`use_dictionary()` rewrites each token to its group label so the
variants collapse to one token and match. Use it when the mapping is
known in advance; when you instead want joinery to discover
near-duplicates from the data, use
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md).

## Usage

``` r
use_dictionary(text, dict)
```

## Arguments

- text:

  A character vector of tokens to look up.

- dict:

  A
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  with a `tokens` column and a `token_group` column. Rows whose `tokens`
  value matches an input token supply that token's group label.

## Value

A list of character vectors, one per input element, holding the matched
group labels (empty when the token is not in `dict`).

## Details

Tokens absent from the dictionary return no group, so chain this after a
token generator and keep a sharper field alongside it.

## See also

[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md)
to discover groups from the data instead.

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md),
[`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md)

## Examples

``` r
dict <- data.table::data.table(
  tokens = c("example", "sample"),
  token_group = c("example/sample", "example/sample")
)
use_dictionary("example", dict)
#> [[1]]
#> [1] "example/sample"
#> 
use_dictionary("nonexistent", dict)
#> [[1]]
#> character(0)
#> 
```
