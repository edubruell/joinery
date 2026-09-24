# Extract initials from tokens

Keeps only the first character of each token (`"ANNA"` becomes `"A"`).
Use it to match on initials when full first names are recorded
inconsistently, for example when one source has `"Anna Berta Schmidt"`
and another `"A. B. Schmidt"`.

## Usage

``` r
extract_initials(tokens)
```

## Arguments

- tokens:

  A list of character vectors.

## Value

A list of character vectors of single-character initials.

## Details

It transforms a token column, so it runs after a token generator such as
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md).

## See also

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/dev/reference/drop_numeric_tokens.md),
[`drop_short_tokens()`](https://edubruell.github.io/joinery/dev/reference/drop_short_tokens.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/dev/reference/filter_stopwords.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/dev/reference/fuzzy_tokens.md),
[`token_shapes()`](https://edubruell.github.io/joinery/dev/reference/token_shapes.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/dev/reference/use_dictionary.md)

## Examples

``` r
extract_initials(list(c("Anna", "Berta")))
#> [[1]]
#> [1] "A" "B"
#> 
# list(c("A", "B"))
```
