# Multilingual Street-Type Normalization Dictionary

A curated cross-linguistic dictionary of street-type forms used for
robust address standardization and record linkage. Each entry maps a
*variant* - including abbreviations, orthographic alternatives,
morphological forms, and transliterated spellings - to a *canonical*
street-type label.

## Usage

``` r
street_types
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 143
rows and 4 columns.

## Source

Manually curated based on postal conventions, open datasets, and
commonly observed street-name variations across languages. The
dictionary is periodically expanded as new variants are encountered in
real-world data.

## Details

Unlike simple suffix lists, this dictionary encodes language-specific
normalization rules. Each variant is marked as either:

- **"exact"** - the variant should only match a token when it appears
  *exactly*, e.g. `"st."` → `"STREET"` (English), `"pl"` → `"PLAZA"`
  (Spanish)

- **"suffix"** - the variant may safely match a token *ending with* that
  sequence, e.g. `"gatan"` → `"GATA"` (Swedish), `"strasse"` →
  `"STRASSE"` (German)

By separating exact vs. suffix behaviour and tagging each entry with an
ISO language code, joinery can normalize addresses *without* incorrect
transformations (e.g. preventing `"LINCOLN"` → `"LANE"`, or `"VICTOR"` →
`"RUE"`). This structure enables high-precision multilingual address
cleaning.

### Languages Covered

The dictionary currently includes major street-type systems from:

- **German** - Straße, Gasse, Weg, Platz, Allee, …

- **English** - Street, Road, Avenue, Boulevard, Lane, …

- **French** - Rue, Avenue, Boulevard, Impasse, Quai, Chemin, …

- **Spanish** - Calle, Avenida, Paseo, Plaza, Camino, …

- **Italian** - Via, Piazza, Corso, Viale, …

- **Portuguese** - Rua, Avenida, Praça, Alameda, Travessa, …

- **Polish** - Ulica, Aleja, Plac, Osiedle, …

- **Dutch** - Straat, Laan, Weg, Plein, …

- **Turkish** - Sokak, Cadde, Bulvar, Meydan, …

- **Swedish** - Gata, Gatan, Vägen, Torg, …

- **Danish/Norwegian** - Gade, Vej, Plads, …

- **Greek (transliterated)** - Odos, Leoforos, Plateia

- **Russian (transliterated)** - Ulitsa, Prospekt, Pereulok, …

Additional languages and street-type systems can be incorporated as
needed.

### Use in [`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)

`street_types` is used by
[`normalize_street()`](https://edubruell.github.io/joinery/reference/normalize_street.md)
to:

- standardize street-type tokens to a canonical form,

- optionally apply language-specific suffix rules (`lang = "de"`,
  `"sv"`, etc.),

- avoid over-normalization by matching only valid variants for the
  specified language,

- support multilingual cleaning workflows in data preprocessing and
  record linkage.

### Format

A tibble with four columns:

- canonical:

  Character string. The standardized street-type label in uppercase
  ASCII (e.g. `"STRASSE"`, `"AVENUE"`, `"PLAC"`).

- variant:

  Character string. A lowercase spelling, abbreviation, transliteration,
  or inflected form seen in raw address data (e.g. `"str."`, `"straße"`,
  `"avda"`, `"gatan"`).

- type:

  Either `"exact"` or `"suffix"`, indicating whether the variant should
  match only whole tokens or may safely match as a word-final suffix.

- lang:

  ISO 639-1 language code (e.g. `"de"`, `"en"`, `"fr"`, `"sv"`), used to
  restrict normalization to the appropriate street-type system.
