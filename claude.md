# joinery

## Package info

This project is a heuristic identification based record linkage utility for R
that should integrate nicely in tidyverse style workflows. Its goal is to 
support both `tibbles`, `data.table`, and `duckdb` databases with different join 
backends. Code for an earlier `data.table`-only prototype called `Matchmaker`
is in the `oldMatchmakerCode` folder in the root directory of the repo.  
You can use this as reference for future functionality.

The new package should use the new S7 object-system in R and 
should separate the actual linkage workflow from the search strategy 
definitions for linkage. See the search_strategy class allready implemented.

Below is an example of how  the MatchMakeR prototype works: 
```R
library(MatchMakeR)

# Load base and target tables
yp_base <- readstata13::read.dta13("yellow_pages_hausaerzte.dta")) |> as.data.table()
base_table <- copy(yp_base[year == 2016 & entry_line == 1][, key_base := entry])
target_table <- copy(yp_base[year == 2017 & entry_line == 1][, key_target := entry])

# Define normalization and tokenization strategy
preparers <- search_preparers(
  Nachname ~ normalize_text + word_tokens(.min_length = 3),
  Vorname ~ normalize_text + word_tokens(.min_length = 3),
  Strasse ~ normalize_text + word_tokens,
  Hausnummer ~ normalize_text + word_tokens,
  Ort ~ normalize_text + word_tokens
)
# Prepare search data
search_table_base <- preapare_search_data(preparers, base_table, "key_base")
search_table_target <- preapare_search_data(preparers, target_table, "key_target")

# Detect duplicates within the base table
likely_duplicates <- detect_duplicates(
  .base_table = search_table_base,
  .base_key = "key_base",
  .threshold = 0.8
)

# Deduplicate the base table
deduplicated_base_table <- deduplicate_table(base_table, likely_duplicates, "key_base")

# Search for matching candidates between base and target tables (Please deduplicate and inspect before doing this)
candidates <- search_candidates(
  .base_table = search_table_base,
  .target_table = search_table_target,
  .base_key = "key_base",
  .target_key = "key_target",
  .threshold = 0.6,
  .weights = c(Hausnummer = 0.1, Nachname = 0.5, Vorname = 0.2, Strasse = 0.1, Ort = 0.1),
  .chunksize = 10000
)
```

## Core Search Utility

The core function of the linkage utility was `search_candidates()`. In the old code it
implements a token-based heuristic record linkage. It identifies potential matches between two tables (base and target) by:

1. **Tokenizing**: Using pre-tokenized records coming from `preapare_search_data` function and a search strategy
2. **Calculating rarity**: Each token's rarity is computed as the inverse of its frequency in the base table
3. **Computing identification potential**: For each base-target record pair, shared tokens contribute to an identification score based on their rarity, weighted by column importance
4. **Filtering**: Only pairs exceeding a specified threshold are returned as candidates

The function operates on long-format token tables (with columns: key, column, tokens). 
Column weights allow prioritizing certain fields (e.g., surname over street) in the matching score.


## Original prepare functionality

The original code had implementations of the following functions:

- **`normalize_text()`**: Normalize a text string by converting to uppercase, transliterating special characters, retaining only alphanumeric characters and spaces, and removing extra spaces.
- **`as_metaphone()`**: Convert a text string to its Metaphone encoding.
- **`as_soundex()`**: Convert a text string to its Soundex encoding.
- **`word_tokens()`**: Return a list of word tokens for the input text.
- **`use_dictionary()`**: Group similar tokens together using a pre-defined dictionary.
- **`generate_ngrams()`**: Generate n-gram tokens from a text string.
- **`search_preparers()`**: Create a list of preparer functions based on a formula syntax.
- **`preapare_search_data()`**: Apply the preparer functions to the specified columns in the input data frame to create a search table
- **`search_candidates()`**: Search for matching candidates between a base table and a target table using token-based heuristic linkage.
- **`detect_duplicates()`**: Detect duplicate records within a base table using token-based heuristic linkage.
- **`deduplicate_table()`**: Use the results of `detect_duplicates` to remove duplicate records from a data table.
- **`build_similarity_dict()`**: Build a dicionary of similar tokens for a base and a target table.

## Desired Interface

```r
library(joinery)

# Load base and target tables (merge yellow pages across years)
yellow_pages <- readstata13::read.dta13("yellow_pages_hausaerzte.dta")) 

base_table <- yellow_pages |>
  filter(year==2016, entry_line==1) |>
  rename(id_base = entry)

target_table <- yellow_pages |>
  filter(year==2017, entry_line==1) |>
  rename(id_target = entry)

yp_strategy <- search_strategy(
  Nachname ~ normalize_text + word_tokens(.min_length = 3),
  Vorname ~ normalize_text + word_tokens(.min_length = 3),
  Strasse ~ normalize_text + word_tokens,
  Hausnummer ~ normalize_text + word_tokens,
  Ort ~ normalize_text + word_tokens,
  block_by = "kreis" #New blocking functionality (Only search within a block)
)

# Detect duplicates within the base table 
likely_duplicates <- detect_duplicates(
  base_table = base_table,
  id         = "id_base",
  strategy   = yp_strategy,
  threshold  = 0.8
)

# Deduplicate the base table
deduplicated_base_table <- deduplicate_table(base_table, likely_duplicates, "id_base")

# Search for matching candidates between base and target tables 
candidates <- search_candidates(
  base_table = base_table,
  target_table = target_table,
  base_id = "id_base",
  target_id = "id_target",
  strategy   = yp_strategy,
  threshold = 0.6,
  weights = c(Hausnummer = 0.1, Nachname = 0.5, Vorname = 0.2, Strasse = 0.1, Ort = 0.1)
)


```

In future the preapre step should be done automatically by candidte search and duplicate detection
functions.

The desired output for the above functions, should look somewhat like this: 

```r
likely_duplicates
# A tibble: 12 × 13
   duplicate_group score   id        year entry_line kreis Nachname     Vorname      Strasse         Hausnummer Ort           PLZ    rank
             <int> <dbl>   <chr>    <dbl>      <dbl> <chr> <chr>        <chr>        <chr>           <chr>      <chr>         <chr> <int>
 1               1 0.97    2016-021  2016          1 A     Schneider    Peter        Lindenweg        3          Neustadt      80100     1
 2               1 0.97    2016-389  2016          1 A     Schnyder     Peter        Lindenweg        3          Neustadt      80100     2
 3               1 0.95    2016-402  2016          1 A     Schneider    P.           Lindenweg        3          Neustadt      80100     3
 4               2 0.92    2016-054  2016          1 B     Hoffmann     Julia        Gartenstr.       18         Kleinstadt    80250     1
 5               2 0.92    2016-288  2016          1 B     Hofmann      Julia        Gartenstraße     18         Kleinstadt    80250     2
 6               3 0.90    2016-077  2016          1 B     Braun        Markus       Kirchweg         7A         Oberdorf      80300     1
 7               3 0.90    2016-501  2016          1 B     Braun        M.           Kirchweg         7A         Oberdorf      80300     2
 8               3 0.89    2016-640  2016          1 B     Braune       Markus       Kirchweg         7A         Oberdorf      80300     3
 9               4 0.84    2016-112  2016          1 C     Lehmann      Sophie       Schulstr.        2          Talhausen     80420     1
10               4 0.84    2016-290  2016          1 C     Lehman       Sophie       Schulstraße      2          Talhausen     80420     2


candidates
# A tibble: 12 × 15
   match_id score direction id        year entry_line kreis Nachname   Vorname      Strasse         Hausnummer Ort          PLZ    rank
     <int> <dbl> <chr>     <chr>    <dbl>      <dbl> <chr> <chr>      <chr>        <chr>           <chr>      <chr>        <chr> <int>
 1       1  0.98 base      2016-001  2016          1 A     Müller     Hans         Hauptstr.       12         Musterstadt  80000     1
 2       1  0.98 target    2017-010  2017          1 A     Mueller    Hans         Hauptstrasse    12         Musterstadt  80000     1
 3       2  0.96 base      2016-003  2016          1 A     Schmidt    Anna Marie   Bergstr.        7          Musterstadt  80000     1
 4       2  0.96 target    2017-011  2017          1 A     Schmidt    Anne Marie   Bergstrasse     7          Musterstadt  80000     1
 5       3  0.81 base      2016-005  2016          1 B     Weber      Fritz        Marktplatz      3          Dorfhausen   81000     1
 6       3  0.81 target    2017-012  2017          1 B     Weber      Fritz        Neue Str.       99         Grossstadt   83000     1
 7       4  0.94 base      2016-006  2016          1 B     von Schön  Lukas        Dorfstr.        5A         Dorfhausen   81000     1
 8       4  0.94 target    2017-013  2017          1 B     von Schoen Lukas        Dorfstrasse     5A         Dorfhausen   81000     1
 9       5  0.91 base      2016-007  2016          1 B     Yilmaz     Mehmet       Ringstr.        22         Dorfhausen   81000     1
10       5  0.91 target    2017-014  2017          1 B     Yilmaz     Mehmet       Ringstrasse     22         Dorfhausen   81000     1
11       6  0.83 base      2016-008  2016          1 C     Kowalski   Joanna       Bahnhofstr.     1          Kleinstadt   82000     1
12       6  0.83 target    2017-015  2017          1 C     Kowalski   J.           Bahnhofstrasse  1          Kleinstadt   82000     1
```
 For `duckdb` the equivalent should be output tables in the database specified in the duplciate detection and search candidate functions.
 

 
