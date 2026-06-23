# Drop numeric (house-number) tokens from token lists

Symmetric inverse of
[`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md):
removes pure-digit tokens (typically house numbers) from a token column.
Operates on the list-of-character token vectors produced by earlier
steps such as
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md),
mirroring
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md).

Useful in address pipelines where the street name carries the matching
signal but the house number is noise (and fans out blocks): tokenize the
street, then `drop_numeric_tokens()` to keep only the name tokens.

## Usage

``` r
drop_numeric_tokens(tokens, keep_letters = TRUE)
```

## Arguments

- tokens:

  A list of character vectors.

- keep_letters:

  Logical. If TRUE (default), number-letter tokens such as "12A" are
  retained; only pure-digit tokens like "12" are dropped. If FALSE, any
  token containing a digit is dropped.

## Value

A list of character vectors with numeric tokens removed.

## See also

[`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md),
its inverse;
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
for the same idea with a named word list.

Other token transformers:
[`drop_short_tokens()`](https://edubruell.github.io/joinery/reference/drop_short_tokens.md),
[`extract_initials()`](https://edubruell.github.io/joinery/reference/extract_initials.md),
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md),
[`fuzzy_tokens()`](https://edubruell.github.io/joinery/reference/fuzzy_tokens.md),
[`token_shapes()`](https://edubruell.github.io/joinery/reference/token_shapes.md),
[`use_dictionary()`](https://edubruell.github.io/joinery/reference/use_dictionary.md)

## Examples

``` r
drop_numeric_tokens(list(c("MAIN", "12", "ST")))
#> [[1]]
#> [1] "MAIN" "ST"  
#> 
# list(c("MAIN", "ST"))

drop_numeric_tokens(list(c("MAIN", "12A")), keep_letters = FALSE)
#> [[1]]
#> [1] "MAIN"
#> 
# list("MAIN")
```
