# Generate character n-grams from text

An n-gram is a sliding window of `n` consecutive characters. Matching on
character n-grams instead of whole words tolerates typos, truncations,
and joined-up spellings, because two strings that differ by a letter
still share most of their windows (`"meier"` and `"maier"` share `"ei"`,
`"er"`, and so on). Reach for it on short, noisy fields where word
tokens are too brittle.

## Usage

``` r
generate_ngrams(text, n)
```

## Arguments

- text:

  A character vector to break into n-grams.

- n:

  The window length (number of characters per n-gram).

## Value

A list of character vectors, one per input element. Strings shorter than
`n` yield an empty vector.

## Details

It tokenizes text directly, so it replaces
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)
rather than following it. The trade-off is fan-out: every string yields
many overlapping tokens, so n-grams cost more to match than words.
Larger `n` is sharper and cheaper, smaller `n` is fuzzier and denser.

## See also

[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)
for whole-word tokens.

Other token generators:
[`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md),
[`word_tokens()`](https://edubruell.github.io/joinery/reference/word_tokens.md)

## Examples

``` r
generate_ngrams("hello", 2)
#> [[1]]
#> [1] "he" "el" "ll" "lo"
#> 
generate_ngrams("an example", 3)
#> [[1]]
#> [1] "an " "n e" " ex" "exa" "xam" "amp" "mpl" "ple"
#> 
```
