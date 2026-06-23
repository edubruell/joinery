# Multilingual Street-Name Stopwords

Locative particles and articles that recur inside multi-word street
names but carry no discriminative signal for matching - German `AN DER`,
French `DE LA`, Italian `DELLA`, and so on. Used by
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)
when `drop_stopwords = TRUE` to collapse e.g. `"An der Alster"` to
`"ALSTER"`.

## Usage

``` r
street_stopwords
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 58
rows and 2 columns.

## Source

Manually curated from common multi-word street-name patterns across
languages. Expandable as new particles are encountered.

## Details

The list is deliberately tight: only true prepositions and articles,
never adjectives (`"NEUE"`, `"GROSSE"`) or directionals that can
themselves be the distinguishing part of a name. Entries are uppercase
ASCII so they join directly against
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)'s
already-uppercased, transliterated tokens. When a `lang` is supplied to
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md),
only that language's particles are removed.

### Format

A tibble with two columns:

- stopword:

  Character string. The particle in uppercase ASCII (e.g. `"AN"`,
  `"DER"`, `"DE"`, `"DELLA"`).

- lang:

  ISO 639-1 language code (`"de"`, `"en"`, `"fr"`, `"es"`, `"it"`,
  `"pt"`, `"nl"`).

## See also

[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md),
[street_types](https://edubruell.github.io/joinery/reference/street_types.md)
