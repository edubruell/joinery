# Split text into word tokens

The workhorse tokenizer. It splits each string on whitespace into a
vector of words, the tokens joinery matches on. It almost always follows
[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md),
which strips punctuation and case first so the split is clean:
`name ~ normalize_text() + word_tokens()`.

## Usage

``` r
word_tokens(text, min_nchar = 0)
```

## Arguments

- text:

  A character vector to split into words.

- min_nchar:

  Minimum token length to keep. Tokens shorter than this are dropped.
  Defaults to `0` (keep everything).

## Value

A list of character vectors, one per input element, each holding that
element's word tokens.

## Details

Set `min_nchar` to drop very short tokens (single initials, stray
letters) that match too easily and add noise.

## See also

[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md),
the usual preceding step;
[`filter_stopwords()`](https://edubruell.github.io/joinery/reference/filter_stopwords.md)
to drop common words by name.

Other token generators:
[`generate_ngrams()`](https://edubruell.github.io/joinery/reference/generate_ngrams.md),
[`numeric_tokens()`](https://edubruell.github.io/joinery/reference/numeric_tokens.md)

## Examples

``` r
word_tokens("this is an example")
#> [[1]]
#> [1] "this"    "is"      "an"      "example"
#> 
word_tokens("this is an example", min_nchar = 3)  # drops "is", "an"
#> [[1]]
#> [1] "this"    "example"
#> 
```
