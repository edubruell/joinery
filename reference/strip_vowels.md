# Strip vowels from text (consonant skeleton)

Reduces text to its consonant skeleton by removing vowels (A, E, I, O,
U, including accented variants). Two spellings that differ only in their
vowels, such as `"MEYER"` and `"MAYER"` or `"MUELLER"` and `"MULLER"`,
collapse to the same skeleton, so they match despite the difference. It
is a lighter-weight alternative to the phonetic encoders
([`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md),
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md))
when you only want to ignore vowel variation.

## Usage

``` r
strip_vowels(text)
```

## Arguments

- text:

  A character vector.

## Value

A character vector with vowels removed, upper-cased and ASCII-folded.

## Details

Returns text, so it goes ahead of a token generator in a pipeline.

## See also

[`as_soundex()`](https://edubruell.github.io/joinery/reference/as_soundex.md)
and
[`as_metaphone()`](https://edubruell.github.io/joinery/reference/as_metaphone.md)
for full phonetic encoding.

Other text normalizers:
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md),
[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md)

## Examples

``` r
strip_vowels("Mueller")   # "MLLR"
#> [1] "MLLR"
strip_vowels("Cafe Noir") # "CF NR"
#> [1] "CF NR"
strip_vowels(c("Anna", "Peter"))
#> [1] "NN"  "PTR"
```
