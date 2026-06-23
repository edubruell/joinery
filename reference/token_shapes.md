# Convert tokens to shape signatures

Reduces each token to its letter/digit pattern: every letter becomes
`"A"`, every digit `"N"`, anything else `"X"`. The signature ignores the
actual characters and keeps only the layout, which is useful for
matching on the format of a code or identifier (postal codes, licence
plates, product codes) rather than its exact value, or as a coarse
blocking key.

## Usage

``` r
token_shapes(tokens)
```

## Arguments

- tokens:

  A list of character vectors.

## Value

A list of character vectors of shape signatures, one signature per input
token.

## Details

It transforms a token column, so it runs after a token generator such as
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md).

## See also

Other token transformers:
[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)

## Examples

``` r
token_shapes(list(c("MUELLER", "A12B")))
#> [[1]]
#> [1] "AAAAAAA" "ANNA"   
#> 
# list(c("AAAAAAA", "ANNA"))
```
