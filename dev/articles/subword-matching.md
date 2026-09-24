# Subword tokenizers

You have a chamber register and a trade directory that list the same
workshops. The register writes each trade as one long word,
`Holzbaugesellschaft Wagner mbH`. The directory splits that word apart
and puts the proprietor first, `Wagner Holzbau KG`. Some directory lines
went through a scanner that misread letters on the way. Neither record
holds a typo that a spelling rule could fix. Word tokens still miss the
pair, because `Holzbaugesellschaft` and `Holzbau` are two different
words and the records share only the surname.

Subword tokens close that gap. You learn a vocabulary of frequent word
pieces from your text, then split each record into those pieces. Two
spellings of the same name share most of their pieces even where they
share no whole word, so they meet and score like any other pair.

This article fits such a vocabulary on a small German register, measures
what it recovers against plain word tokens, and shows the two settings
that decide the quality of the result.

Subword tokens use the `sentencepiece` package, which joinery loads only
when you ask for it:

``` r

install.packages("sentencepiece")
```

``` r

library(joinery)
```

## The data

Forty workshops, in one table to deduplicate. Twenty appear twice, once
in the register spelling and once in the directory spelling, and share
at most the proprietor’s name. Ten more appear twice in near-identical
spellings. Word tokens already find those ten, so they show whether a
subword run loses anything that word tokens had. Ten unrelated trades
appear once and have no partner.

``` r

register <- c(
  "Holzbaugesellschaft Wagner mbH",  "Moebelwerkstatt Brandt",
  "Tischlereibetrieb Kaufmann",      "Fensterbaugesellschaft Lehmann",
  "Treppenbauwerkstatt Ritter",      "Parkettlegerbetrieb Sommer",
  "Innenausbaugesellschaft Naumann", "Kuechenmoebelfabrik Wendt",
  "Holzhandelsgesellschaft Gerlach", "Schreinereibetrieb Hoffmann",
  "Dachstuhlbaugesellschaft Kolbe",  "Bautischlereibetrieb Reuter",
  "Ladenbaugesellschaft Piotrowski", "Gartenmoebelwerkstatt Ebeling",
  "Massivholzmoebelfabrik Stein",    "Trockenbaugesellschaft Amrein",
  "Furnierhandelsgesellschaft Zorn", "Bootsbauwerkstatt Marquardt",
  "Saegewerksgesellschaft Uhlig",    "Restaurierungswerkstatt Thiele"
)

directory <- c(
  "Wagner Holzbau KG",              "Werkstatt fuer Moebel Brandt",
  "Kaufmann Tischlerei",            "Lehmann Fensterbau GmbH",
  "Ritter Treppenbau Werkstatt",    "Sommer Parkettleger",
  "Naumann Innenausbau GmbH",       "Wendt Kuechenmoebel Fabrik",
  "Gerlach Holzhandel",             "Hoffmann Schreinerei",
  "Kolbe Dachstuhlbau",             "Reuter Bautischlerei",
  "Piotrowski Ladenbau GmbH",       "Ebeling Gartenmoebel Werkstatt",
  "Stein Massivholzmoebel",         "Amrein Trockenbau GmbH",
  "Zorn Furnierhandel",             "Marquardt Bootsbau",
  "Uhlig Saegewerk",                "Thiele Restaurierung"
)

# The scanner misread every third directory line: rn as m, O as zero, ll as 11.
scanned <- seq(3, length(directory), by = 3)
directory[scanned] <- sub("ll", "11",
  sub("O", "0", sub("rn", "m", directory[scanned])))

# Ten workshops the directory copies almost verbatim.
plain <- c(
  "Tischlerei Baumgartner", "Schreinerei Kowalczyk",
  "Zimmerei Oberlaender",   "Drechslerei Feldmann",
  "Boettcherei Hanselmann", "Parkettstudio Vranken",
  "Holzwerkstatt Dallmann", "Moebelbau Steinhauser",
  "Treppenstudio Wendland", "Holzdesign Kretschmar"
)

# Ten trades that appear once and belong to nobody else.
others <- c(
  "Malerbetrieb Kessler",    "Dachdeckerei Vogt",
  "Elektrotechnik Sander",   "Sanitaertechnik Brunner",
  "Metallbau Hartwig",       "Steinmetzbetrieb Gruber",
  "Glaserei Wiedemann",      "Polsterei Nehring",
  "Zimmerei Achenbach",      "Schlosserei Berndt"
)

workshops <- rbind(
  data.frame(id = sprintf("R%02d", 1:30), firm = c(register, plain)),
  data.frame(id = sprintf("D%02d", 1:30), firm = c(directory, paste(plain, "GmbH"))),
  data.frame(id = sprintf("X%02d", 1:10), firm = others)
)

head(workshops, 3)
#>    id                           firm
#> 1 R01 Holzbaugesellschaft Wagner mbH
#> 2 R02         Moebelwerkstatt Brandt
#> 3 R03     Tischlereibetrieb Kaufmann
```

The ids carry the answer key: `R07` and `D07` are the same workshop, the
`X` rows belong to nobody. We score each run below with two measures and
define them once here. **Pair recall** is the share of the thirty known
pairs whose two rows land in one duplicate group. **Group purity** is
the share of duplicate groups that hold a single workshop. Anything
under 1 means two different workshops were joined.

``` r

entities <- sprintf("%02d", 1:30)

pair_recall <- function(dups, which = entities) {
  if (nrow(dups) == 0) return(0)
  groups <- split(dups$id, dups$duplicate_group)
  mean(vapply(which, function(e) {
    any(vapply(groups, function(ids) all(paste0(c("R", "D"), e) %in% ids), logical(1)))
  }, logical(1)))
}

group_purity <- function(dups) {
  if (nrow(dups) == 0) return(NA_real_)
  groups <- split(sub("^[RD]", "", dups$id), dups$duplicate_group)
  mean(vapply(groups, function(e) length(unique(e)) == 1, logical(1)))
}
```

## Where word tokens stop

Start with the strategy you would reach for first. It cleans the text,
splits it on whitespace and keeps pairs whose shared words are rare
enough to pass the threshold.

``` r

words <- search_strategy(
  firm ~ normalize_text() + word_tokens(),
  threshold = 0.5
)

word_dups <- detect_duplicates(workshops, id = "id", strategy = words)

c(recall = pair_recall(word_dups), purity = group_purity(word_dups))
#> recall purity 
#>    0.4    1.0
```

Each pair it finds is correct, but it finds only twelve of the thirty.
The ten near-identical pairs come back, plus two compound pairs carried
by the surname alone. The other eighteen register rows miss their
directory entry, because the trade word the two records share sits
inside a longer word on one side.

## Learning a vocabulary

[`find_subwords()`](https://edubruell.github.io/joinery/dev/reference/find_subwords.md)
learns the pieces. Feed it the text as your strategy will see it: the
same cleaning steps that come before the tokenizer in the formula, here
[`normalize_text()`](https://edubruell.github.io/joinery/dev/reference/normalize_text.md).
Fitting on raw text and applying to cleaned text gives worse splits.

``` r

corpus <- normalize_text(c(
  register, plain, others,
  "Holzbau Wagner", "Moebel Brandt", "Tischlerei Kaufmann",
  "Fensterbau Lehmann", "Treppenbau Ritter", "Parkettleger Sommer",
  "Innenausbau Naumann", "Kuechenmoebel Wendt", "Holzhandel Gerlach",
  "Schreinerei Hoffmann", "Dachstuhlbau Kolbe", "Bautischlerei Reuter",
  "Ladenbau Piotrowski", "Gartenmoebel Ebeling", "Massivholzmoebel Stein",
  "Trockenbau Amrein", "Furnierhandel Zorn", "Bootsbau Marquardt",
  "Saegewerk Uhlig", "Restaurierung Thiele",
  "Werkstatt", "Gesellschaft", "Betrieb", "Fabrik", "Handel", "GmbH", "KG"
))

sw <- find_subwords(corpus, vocab_size = 340)
sw
#> <joinery::Subword_Model>
#>   Algorithm:  bpe
#>   Vocabulary: 340 subwords
#>   Boundary markers: stripped
```

The corpus here is the names in the file plus the trade words on their
own and the words that mean company. A vocabulary learns pieces from the
text it sees, so a short word you want compounds cut at is worth feeding
it separately.

One record shows what the model does to a compound:

``` r

subword_tokens(normalize_text("Holzbaugesellschaft Wagner mbH"), sw)
#> [[1]]
#> [1] "HOLZ"            "BAUGESELLSCHAFT" "WAGNER"          "MBH"
subword_tokens(normalize_text("Wagner Holzbau KG"), sw)
#> [[1]]
#> [1] "WAGNER" "HOLZ"   "BAU"    "KG"
```

Both records now carry the piece `HOLZ` next to `WAGNER`. The two
records thus share two tokens instead of one, which is enough for the
pair to pass the threshold.

## What subword tokens find

Put the fitted model in the formula in place of
[`word_tokens()`](https://edubruell.github.io/joinery/dev/reference/word_tokens.md).
Nothing else about the strategy changes. Pieces are ordinary tokens,
each weighted by its rarity, and the threshold means what it meant
before.

``` r

subwords <- search_strategy(
  firm ~ normalize_text() + subword_tokens(sw) + drop_short_tokens(min_nchar = 3),
  threshold = 0.5
)

sub_dups <- detect_duplicates(workshops, id = "id", strategy = subwords)

c(recall = pair_recall(sub_dups), purity = group_purity(sub_dups))
#> recall purity 
#>      1      1
```

All thirty pairs come back, and each duplicate group holds a single
workshop. The ten pairs word tokens already had are still there:

``` r

pair_recall(sub_dups, which = sprintf("%02d", 21:30))
#> [1] 1
```

That formula also drops pieces under three characters. A learned
vocabulary contains one- and two-letter pieces. These turn up in most
records, so they cost compute and blur the score. Dropping them takes
one step in the formula, and in the grid below it lifts recall at 340
pieces from 0.97 to 1.

## Two settings, pulling against each other

`vocab_size` decides how finely text is cut. A small vocabulary has few
pieces to work with, so it cuts words into short fragments that many
workshops share. A large one keeps frequent words whole and cuts only
the rare ones, which brings you back towards word tokens. The minimum
piece length works against the first failure, since it drops the
shortest fragments before they are scored.

``` r

grid <- expand.grid(vocab_size = c(200, 340, 400), min_nchar = c(2, 3))

result <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  model <- find_subwords(corpus, vocab_size = grid$vocab_size[i])
  strat <- search_strategy(
    firm ~ normalize_text() + subword_tokens(model) +
      drop_short_tokens(min_nchar = grid$min_nchar[i]),
    threshold = 0.5
  )
  dups <- detect_duplicates(workshops, id = "id", strategy = strat)
  data.frame(
    vocab_size = grid$vocab_size[i],
    min_nchar  = grid$min_nchar[i],
    recall     = pair_recall(dups),
    purity     = group_purity(dups)
  )
}))

result
#>   vocab_size min_nchar    recall    purity
#> 1        200         2 1.0000000 0.7666667
#> 2        340         2 0.9666667 1.0000000
#> 3        400         2 0.9000000 1.0000000
#> 4        200         3 1.0000000 0.7058824
#> 5        340         3 1.0000000 1.0000000
#> 6        400         3 0.9000000 1.0000000
```

One corner of the grid is clean, 340 pieces with a three-character
minimum. At `vocab_size = 200` the recall holds and the purity falls,
because the fragments get short enough for unrelated workshops to share
them. That is the failure to watch for, because a mixed duplicate group
costs more to undo than a missed pair costs to find. At 400 the
vocabulary keeps the compounds whole again and pairs go missing.

[`rarity_distribution()`](https://edubruell.github.io/joinery/dev/reference/rarity_distribution.md)
is how you judge a fitted vocabulary where you have no ground truth to
score against. It lists the pieces that turn up in the most records.
Here the list mixes whole words such as `WERKSTATT` with fragments such
as `EREI` and `MANN`, which unrelated workshops share. When fragments
crowd out the words at the top, the vocabulary cuts too finely.

``` r

rarity_distribution(workshops, id = "id", strategy = subwords)
#> 
#> ── Rarity_Distribution ─────────────────────────────────────────────────────────
#> rarity method: "inverse_freq"
#> per-column distribution
#> firm: 105 tokens, df_max=14 (GMBH), rarity p50=0.5, suggested min_rarity >~
#> 0.07143
#> top-df offenders (fan-out drivers)
#> firm: 'GMBH' df=14, rarity=0.07143
#> firm: 'WERKSTATT' df=10, rarity=0.1
#> firm: 'EREI' df=8, rarity=0.125
#> firm: 'BAU' df=7, rarity=0.1429
#> firm: 'MANN' df=7, rarity=0.1429
#> firm: 'HOLZ' df=6, rarity=0.1667
#> firm: 'MOEBEL' df=6, rarity=0.1667
#> firm: 'BETRIEB' df=6, rarity=0.1667
#> firm: 'STEIN' df=5, rarity=0.2
#> firm: 'TISCHLEREI' df=4, rarity=0.25
```

On a corpus of millions of rows, fit on a sample.
`find_subwords(x, sample_n = 2e5)` learns an equivalent vocabulary in a
fraction of the time, because the frequent pieces of a large corpus are
already the frequent pieces of a large sample of it.

## Keeping the model

A fitted vocabulary is a plain R object that carries the trained model
as raw bytes, so it saves and reloads like anything else and splits text
the same way every time.

``` r

saveRDS(sw, "workshop_subwords.rds")
sw <- readRDS("workshop_subwords.rds")
```

Save the model and rebuild the formula around it. A strategy formula
carries the environment it was written in, which makes it a much larger
and less predictable thing to put on disk. Rebuilding it around a saved
model costs one line and reproduces the same splits.

## Bringing your own tokenizer

[`subword_tokens()`](https://edubruell.github.io/joinery/dev/reference/subword_tokens.md)
is a named shortcut for a more general step.
[`custom_tokens()`](https://edubruell.github.io/joinery/dev/reference/custom_tokens.md)
takes any tokenizer and puts it in a strategy formula. A rule you have
written yourself then scores like a built-in one.

The tokenizer can be a plain function that takes a character vector and
returns a list of token vectors, one per input record:

``` r

split_runs <- function(text) strsplit(text, "[^a-z]+")

custom_tokens(c("holzbau wagner", "wagner-holzbau"), split_runs)
#> [[1]]
#> [1] "holzbau" "wagner" 
#> 
#> [[2]]
#> [1] "wagner"  "holzbau"
```

It can also be a fitted object with a
[`tokenize()`](https://edubruell.github.io/joinery/dev/reference/tokenize.md)
method. That is how a tokenizer with learned state enters a strategy,
and it is what a `Subword_Model` is. `custom_tokens(text, sw)` and
`subword_tokens(text, sw)` do the same work.

Whichever form you pass, the result needs one element per input record,
each a character vector of that record’s tokens. If the result has the
wrong shape, joinery stops with an error that names the problem, before
any tokens reach the scorer.
