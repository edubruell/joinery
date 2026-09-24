# Tokenize with your own tokenizer

The built-in tokenizers split text into words, n-grams, or dates. When
none of them fits your data, write your own and place it in the strategy
formula with `custom_tokens()`:
`name ~ normalize_text() + custom_tokens(my_tokenizer)`.

## Usage

``` r
custom_tokens(text, tokenizer)
```

## Arguments

- text:

  A character vector to tokenize.

- tokenizer:

  A plain function, or an object with a
  [`tokenize()`](https://edubruell.github.io/joinery/dev/reference/tokenize.md)
  method.

## Value

A list of character vectors, one per input element.

## Details

The tokenizer can be a plain function that takes a character vector and
returns a list of token vectors. It can also be a fitted tokenizer
object with a
[`tokenize()`](https://edubruell.github.io/joinery/dev/reference/tokenize.md)
method, which is how a tokenizer with learned state, such as a trained
subword vocabulary, enters a strategy.

Whichever form you pass, it must return a list with one element per
input string, each element a character vector of that record's tokens
(`character(0)` for a record with none). `custom_tokens()` checks this
and stops with a clear error if the result does not fit.

## See also

[`tokenize()`](https://edubruell.github.io/joinery/dev/reference/tokenize.md),
the method a fitted tokenizer provides;
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md)
for the standard whitespace tokenizer.

Other token generators:
[`generate_ngrams()`](https://edubruell.github.io/joinery/dev/reference/generate_ngrams.md),
[`numeric_tokens()`](https://edubruell.github.io/joinery/dev/reference/numeric_tokens.md),
[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md),
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md)

## Examples

``` r
# A bare-function tokenizer: split on vowels
split_vowels <- function(text) strsplit(text, "[aeiou]+")
custom_tokens(c("plexiglas", "brandenburg"), split_vowels)
#> [[1]]
#> [1] "pl" "x"  "gl" "s" 
#> 
#> [[2]]
#> [1] "br" "nd" "nb" "rg"
#> 
```
