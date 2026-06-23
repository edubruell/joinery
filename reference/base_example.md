# Base dataset for record linkage example

A dataset containing 3,300 person records with German, Turkish, and
Polish names, including addresses across various German cities.
Approximately 5% of records are intentional duplicates with small
variations to simulate real-world data quality issues.

## Usage

``` r
base_example
```

## Format

A tibble with 3,300 rows and 7 variables:

- id_base:

  Character identifier for base records (B0001-B3150)

- Vorname:

  First name, weighted by ethnic group prevalence

- Nachname:

  Last name, weighted by ethnic group prevalence

- Strasse:

  Street name, including German street types

- Hausnummer:

  House number, some with letter suffixes

- Ort:

  City or town name

- Kreis:

  Administrative district (Kreis)

## Source

Synthetically generated using weighted sampling from common German,
Turkish, and Polish names (93%, 4%, and 3% respectively) and realistic
German geography.
