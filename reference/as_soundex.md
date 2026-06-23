# Encode text phonetically with Soundex

Soundex is the classic phonetic code: it keeps the first letter and
reduces the rest to a short digit string (for example `"Robert"` and
`"Rupert"` both become `"R163"`), so spellings that sound alike share
one key. It is coarser and older than Metaphone but widely understood
and a good default for English surnames.

## Usage

``` r
as_soundex(text)
```

## Arguments

- text:

  A character string or vector to encode, or a token list-column (one
  character vector of tokens per row) when the encoder is placed *after*
  a token generator – each token is then encoded in place.

## Value

A character vector of Soundex keys (letter followed by digits), one per
input element.

## Details

Runs on either side of a token generator: ahead of one (on a text
column), or after one (on a token column, encoding each token in place).
As with any phonetic key it favours recall over precision; pair it with
a sharper field rather than matching on the key alone.

## See also

Other phonetic encoders:
[`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md),
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md)

## Examples

``` r
as_soundex("Robert")
#> [1] "R163"
as_soundex(c("Robert", "Rupert"))  # same key
#> [1] "R163" "R163"
```
