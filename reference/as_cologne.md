# Encode text phonetically with the Cologne procedure

The Cologne phonetic procedure (Koelner Phonetik) is the German-language
counterpart to Soundex. It maps text to a digit string by German
pronunciation rules, so variants like `"Meier"`, `"Maier"`, and
`"Mayer"` share one key. Reach for this over
[`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md)
or
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md)
when the data is German.

## Usage

``` r
as_cologne(text)
```

## Arguments

- text:

  A character string or vector to encode, or a token list-column (one
  character vector of tokens per row) when the encoder is placed *after*
  a token generator – each token is then encoded in place.

## Value

A character vector of Cologne phonetic keys (digit strings), one per
input element.

## Details

Returns text, so it slots ahead of a token generator, or use it directly
on a one-word column. Like any phonetic key it favours recall over
precision; pair it with a sharper field rather than matching on the key
alone.

## See also

Other phonetic encoders:
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md),
[`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md)

## Examples

``` r
as_cologne(c("Meier", "Maier", "Mayer"))  # same key
#> [1] "67" "67" "67"
```
