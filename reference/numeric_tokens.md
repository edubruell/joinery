# Tokenize numeric fields, expanding ranges into individual numbers

Turns numeric/house-number-like text into a list of tokens. Expands
ranges such as "12-14" or "7-9" into c("12","13","14"). Uses original
spacing/separators to detect ranges, while normalization cleans text for
tokenization.

## Usage

``` r
numeric_tokens(text, keep_letters = TRUE, destructive = FALSE)
```

## Arguments

- text:

  Character vector of numeric or address fields.

- keep_letters:

  Logical. If TRUE, retains letter suffixes like "12A". Only applies
  when `destructive = FALSE`.

- destructive:

  Logical. If TRUE, removes all non-digit characters except whitespace.
  If FALSE (default), preserves letters alongside digits.

## Value

A list of character vectors, one per input element. Each vector contains
numeric tokens, with ranges expanded into sequences.

## See also

[`drop_numeric_tokens()`](https://edubruell.github.io/joinery/reference/drop_numeric_tokens.md),
its inverse, to discard numbers from a token column instead.

Other token generators:
[`generate_ngrams()`](https://edubruell.github.io/joinery/reference/generate_ngrams.md),
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)

## Examples

``` r
numeric_tokens("12-14")
#> [[1]]
#> [1] "12" "13" "14"
#> 
# list(c("12", "13", "14"))

numeric_tokens("7A 9B", keep_letters = TRUE)
#> [[1]]
#> [1] "7A" "9B"
#> 
# list(c("7A", "9B"))

numeric_tokens("House 5", destructive = TRUE)
#> [[1]]
#> [1] "5"
#> 
# list("5")
```
