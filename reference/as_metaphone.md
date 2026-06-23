# Encode text phonetically with Metaphone

Names that sound alike are often spelled differently: `"Smith"` and
`"Smyth"`, `"Meyer"` and `"Maier"`. Metaphone encodes text by how it
sounds, so those variants share one key and match even though the
letters differ. Best on single-word fields such as surnames or company
names; it is tuned for English pronunciation (for German, see
[`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md)).

## Usage

``` r
as_metaphone(text)
```

## Arguments

- text:

  A character string or vector to encode, or a token list-column (one
  character vector of tokens per row) when the encoder is placed *after*
  a token generator – each token is then encoded in place.

## Value

A character vector of Metaphone keys, one per input element.

## Details

Runs on either side of a token generator: ahead of one (on a text
column), or after one (on a token column, encoding each token in place).
Phonetic keys are deliberately coarse, so they trade precision for
recall: pair them with a sharper field rather than matching on a
phonetic key alone.

## See also

Other phonetic encoders:
[`as_cologne()`](https://edubruell.github.io/joinery/reference/as_cologne.md),
[`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md)

## Examples

``` r
as_metaphone("Smith")
#> [1] "SM0"
as_metaphone(c("Meyer", "Maier"))  # same key
#> [1] "MYR" "MR" 
```
