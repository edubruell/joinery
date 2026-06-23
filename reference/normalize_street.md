# Normalize street names across languages

Street names are written many ways for the same place: `"Hauptstr."`,
`"Hauptstrasse"`, `"Haupt Strasse"`. `normalize_street()` collapses
those variants to one canonical spelling so an address column matches on
the street name rather than on its abbreviation. It normalizes Unicode,
folds to ASCII, upper-cases, and cleans whitespace, then rewrites known
street-type tokens from a multilingual dictionary.

## Usage

``` r
normalize_street(
  x,
  lang = NULL,
  drop_house_numbers = FALSE,
  drop_stopwords = FALSE,
  dict = joinery::street_types,
  stopwords = joinery::street_stopwords
)
```

## Arguments

- x:

  A character vector containing street names or address fragments.

- lang:

  Optional language code (e.g., `"de"`, `"en"`, `"fr"`). When provided,
  the dictionary is filtered to that language and safe language-specific
  suffix matching is enabled. It also restricts `drop_stopwords` to that
  language's particle list.

- drop_house_numbers:

  Logical (default `FALSE`). When `TRUE`, drops any token beginning with
  a digit (house numbers like `"12"`, `"12A"`, `"123B"`), keeping only
  the street name. Applied after street-type replacement.

- drop_stopwords:

  Logical (default `FALSE`). When `TRUE`, removes locative particles and
  articles (e.g. German `AN DER`, French `DE LA`) listed in `stopwords`,
  collapsing `"An der Alster"` to `"ALSTER"`. When `lang` is given, only
  that language's particles are removed; otherwise the whole `stopwords`
  set is used.

- dict:

  A dictionary of street-type definitions, typically
  [street_types](https://edubruell.github.io/joinery/reference/street_types.md),
  containing the columns:

  - `canonical`: canonical uppercase form

  - `variant`: lowercased normalized variant form

  - `type`: `"exact"` or `"suffix"`

  - `lang`: ISO language code

- stopwords:

  A street-stopword table, typically
  [street_stopwords](https://edubruell.github.io/joinery/reference/street_stopwords.md),
  with columns `stopword` (uppercase ASCII) and `lang`. Only consulted
  when `drop_stopwords = TRUE`.

## Value

A character vector of normalized street names. `NA` inputs are preserved
as `NA`. Rows reduced to nothing (e.g. a bare house number with
`drop_house_numbers = TRUE`) become `""`.

## Details

Returns text, so it sits where
[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md)
would in a pipeline, ahead of a token generator:
`street ~ normalize_street(lang = "de") + word_tokens()`.

Exact matches (e.g., `"st"`, `"rd."`, `"via"`) are always replaced.
Suffix matches (e.g., German `"strasse"` endings or Dutch `"straat"`)
are applied **only when `lang` is explicitly specified**, which prevents
unsafe substitutions such as rewriting the ending of `"LINCOLN LANE"`.

Normalization steps include:

- Unicode -\> Latin transliteration and ASCII folding
  (`stri_trans_general`)

- Conversion to uppercase

- Removal of non-alphanumeric characters

- Tokenization on spaces and per-token replacement

Exact variants are replaced verbatim with their canonical form. Suffix
variants are replaced only when:

- `lang` is specified, and

- the token ends with a known variant suffix for that language.

## See also

Other text normalizers:
[`normalize_text()`](https://edubruell.github.io/joinery/reference/normalize_text.md),
[`strip_vowels()`](https://edubruell.github.io/joinery/reference/strip_vowels.md)

## Examples

``` r
normalize_street("Muellerstrasse", lang = "de")
#> [1] "MUELLERSTRASSE"
# "MUELLERSTRASSE"

normalize_street("123 Main St.")
#> [1] "123 MAIN STREET"
# "123 MAIN STREET"

normalize_street("Calle Mayor 3", lang = "es")
#> [1] "CALLE MAYOR 3"
# "CALLE MAYOR 3"

normalize_street("Hauptstr. 123A", lang = "de", drop_house_numbers = TRUE)
#> [1] "HAUPTSTRASSE"
# "HAUPTSTRASSE"

normalize_street("An der Alster 5", lang = "de",
                 drop_house_numbers = TRUE, drop_stopwords = TRUE)
#> [1] "ALSTER"
# "ALSTER"
```
