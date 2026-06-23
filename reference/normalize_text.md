# Normalize text for matching

The usual first step in a preparer pipeline. Folds text to upper case,
transliterates accented and non-Latin characters to ASCII, drops
anything that is not a letter, digit, or space, and collapses runs of
whitespace. The point is to make superficial differences in case,
accents, and punctuation disappear so that `"Cafe-Conac"` and
`"cafe conac"` reduce to the same text before it is split into tokens.

## Usage

``` r
normalize_text(text, transliteration = "De-ASCII")
```

## Arguments

- text:

  A character string or vector to normalize.

- transliteration:

  A transliteration scheme passed to
  [`stringi::stri_trans_general()`](https://rdrr.io/pkg/stringi/man/stri_trans_general.html),
  defaulting to `"De-ASCII"` (German-aware folding, which expands
  umlauts to digraphs such as `ue` and `oe`). Use `"Latin-ASCII"` for
  plain accent stripping, which drops the diacritic instead of expanding
  it.

## Value

A character vector the same length as `text`: upper-cased, ASCII,
alphanumeric-and-space only, with surrounding and repeated spaces
removed.

## Details

Returns text, so it goes ahead of a token generator such as
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)
in a strategy: `name ~ normalize_text() + word_tokens()`.

## See also

[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md),
the token generator that usually follows.

Other text normalizers:
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md),
[`strip_vowels()`](https://edubruell.github.io/joinery/reference/strip_vowels.md)

## Examples

``` r
normalize_text("Cafe Conac")
#> [1] "CAFE CONAC"
normalize_text("Strasse", transliteration = "Latin-ASCII")
#> [1] "STRASSE"
```
