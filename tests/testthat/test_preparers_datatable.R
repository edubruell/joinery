test_that("normalize_text() works as in examples", {
  # Example 1: normalize_text("Café Coñac")
  expect_equal(
    normalize_text("Café Coñac"),
    "CAFE CONAC"
  )
  
  # Example 2: normalize_text(..., transliteration = "Latin-ASCII")
  expect_equal(
    normalize_text("Straße", transliteration = "Latin-ASCII"),
    "STRASSE"
  )
})

test_that("as_metaphone() returns correct Metaphone codes", {
  
  expect_equal(
    as_metaphone("Café"),
    "KF"
  )
  
  expect_equal(
    as_metaphone("Straße"),
    "STRS"
  )
  
})

test_that("as_soundex() works as in examples", {
  expect_equal(
    as_soundex("Café"),
    "C100"
  )
  
  expect_equal(
    as_soundex("Straße"),
    "S362"
  )
})

test_that("as_cologne() works as in examples", {
  expect_equal(
    as_cologne("Café"),
    "43"
  )
  
  expect_equal(
    as_cologne("Straße"),
    "8278"
  )
})

test_that("word_tokens() works as in examples", {
  expect_equal(
    word_tokens("This is an example."),
    list(c("This", "is", "an", "example."))
  )
  
  expect_equal(
    word_tokens("Another, test; string."),
    list(c("Another,", "test;", "string."))
  )
  
  # Check min_length filter
  expect_equal(
    word_tokens("This is an example", min_nchar = 3),
    list(c("This", "example"))
  )
})

test_that("generate_ngrams() works as in examples", {
  # Example: generate_ngrams("hello", 2)
  expect_equal(
    generate_ngrams("hello", 2),
    list(c("he", "el", "ll", "lo"))
  )
  
  # Example: generate_ngrams("an example", 3)
  # "an example" has length 11
  # 3-grams:
  # "an ", "n e", " ex", "exa", "xam", "amp", "mpl", "ple"
  expect_equal(
    generate_ngrams("an example", 3),
    list(c("an ", "n e", " ex", "exa", "xam", "amp", "mpl", "ple"))
  )
  
  # Edge case: too short => empty char vector
  expect_equal(
    generate_ngrams("hi", 3),
    list(character(0))
  )
})

test_that("use_dictionary() works as in examples", {
  dict <- data.table::data.table(
    tokens = c("example", "sample"),
    token_group = c("example/sample", "example/sample")
  )
  
  expect_equal(
    use_dictionary("example", dict),
    list("example/sample")
  )
  
  # nonexistent should return character(0)
  expect_equal(
    use_dictionary("nonexistent", dict),
    list(character(0))
  )
  
  # vectorized lookup
  expect_equal(
    use_dictionary(c("example", "nonexistent"), dict),
    list("example/sample", character(0))
  )
})


test_that("normalize_street() correctly normalizes street names (German + international)", {
  
  # ===========================================================================#
  #                                German (de)                                #
  # ===========================================================================#
  
  expect_equal(normalize_street("Hauptstr. 123",        lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstr 123",         lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstraße 123",      lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstrasse 123",     lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstrasse. 123",    lang="de"), "HAUPTSTRASSE 123")
  
  # Case-insensitivity
  expect_equal(normalize_street("hauptstr. 123",        lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("HAUPTSTR. 123",        lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("HauptSTR 123",         lang="de"), "HAUPTSTRASSE 123")
  
  # Unicode + transliteration
  expect_equal(normalize_street("Königsstraße 55",      lang="de"), "KONIGSSTRASSE 55")
  expect_equal(normalize_street("Königsstr. 55",        lang="de"), "KONIGSSTRASSE 55")
  
  # House numbers
  expect_equal(normalize_street("Hauptstr. 123A",       lang="de"), "HAUPTSTRASSE 123A")
  expect_equal(normalize_street("Hauptstr 12b",         lang="de"), "HAUPTSTRASSE 12B")
  
  # Punctuation handling
  expect_equal(normalize_street("Hauptstr., 123",       lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstr: 123",        lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstr-123",         lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street("Hauptstr. 123.",       lang="de"), "HAUPTSTRASSE 123")
  
  # Space normalization
  expect_equal(normalize_street("Hauptstr.     123",    lang="de"), "HAUPTSTRASSE 123")
  expect_equal(normalize_street(" Hauptstr. 123 ",      lang="de"), "HAUPTSTRASSE 123")
  
  # NA handling
  x <- c("Hauptstr. 123", NA, "Hauptstrasse 123")
  out <- normalize_street(x, lang="de")
  expect_equal(out[[1]], "HAUPTSTRASSE 123")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "HAUPTSTRASSE 123")
  
  # Multi-token street names
  expect_equal(normalize_street("Neue Hauptstr. 123",   lang="de"), "NEUE HAUPTSTRASSE 123")
  expect_equal(normalize_street("12 Hauptstr. 123",     lang="de"), "12 HAUPTSTRASSE 123")
  
  # Avoid over-normalization
  expect_equal(normalize_street("Strandstr. 99",        lang="de"), "STRANDSTRASSE 99")
  
  # Vectorized consistency
  variants <- c(
    "Hauptstr 123",
    "Hauptstraße 123",
    "Hauptstr. 123",
    "Hauptstrasse 123",
    "hauptstr. 123"
  )
  vec_out <- normalize_street(variants, lang="de")
  expect_true(all(vec_out == "HAUPTSTRASSE 123"))
  
  # ===========================================================================#
  #                                   English                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Main St. 20",          lang="en"), "MAIN STREET 20")
  expect_equal(normalize_street("Broadway Rd 5",        lang="en"), "BROADWAY ROAD 5")
  expect_equal(normalize_street("Lincoln Blvd. 10",     lang="en"), "LINCOLN BOULEVARD 10")
  expect_equal(normalize_street("Market Ave 7",         lang="en"), "MARKET AVENUE 7")
  
  # ===========================================================================#
  #                                   French                                  #
  # ===========================================================================#
  
  expect_equal(normalize_street("Rue de Paris 8",       lang="fr"), "RUE DE PARIS 8")
  expect_equal(normalize_street("Av. Victor Hugo 12",   lang="fr"), "AVENUE VICTOR HUGO 12")
  expect_equal(normalize_street("Bd. St Michel 22",     lang="fr"), "BOULEVARD ST MICHEL 22")
  
  # ===========================================================================#
  #                                   Spanish                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Calle Mayor 3",        lang="es"), "CALLE MAYOR 3")
  expect_equal(normalize_street("Avda. del Libertador", lang="es"), "AVENIDA DEL LIBERTADOR")
  expect_equal(normalize_street("Paseo del Prado",      lang="es"), "PASEO DEL PRADO")
  
  # ===========================================================================#
  #                                   Italian                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Via Roma 5",           lang="it"), "VIA ROMA 5")
  expect_equal(normalize_street("P. Garibaldi",         lang="it"), "PIAZZA GARIBALDI")
  expect_equal(normalize_street("Corso Italia 7",       lang="it"), "CORSO ITALIA 7")
  
  # ===========================================================================#
  #                                  Portuguese                               #
  # ===========================================================================#
  
  expect_equal(normalize_street("Rua das Flores 10",    lang="pt"), "RUA DAS FLORES 10")
  expect_equal(normalize_street("Av. Brasil 100",       lang="pt"), "AVENIDA BRASIL 100")
  expect_equal(normalize_street("Praca Central",        lang="pt"), "PRACA CENTRAL")
  
  # ===========================================================================#
  #                                    Polish                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Ul. Kosciuszki 11",    lang="pl"), "ULICA KOSCIUSZKI 11")
  expect_equal(normalize_street("Aleja Krakowska",      lang="pl"), "ALEJA KRAKOWSKA")
  expect_equal(normalize_street("Plac Wolnosci",        lang="pl"), "PLAC WOLNOSCI")
  
  # ===========================================================================#
  #                                     Dutch                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Kerkstraat 9",         lang="nl"), "KERKSTRAAT 9")
  expect_equal(normalize_street("Nieuwe Laan 14",       lang="nl"), "NIEUWE LAAN 14")
  expect_equal(normalize_street("Groot Plein 2",        lang="nl"), "GROOT PLEIN 2")
  
  # ===========================================================================#
  #                                    Turkish                                #
  # ===========================================================================#
  
  expect_equal(normalize_street("İstiklal Cad. 44",     lang="tr"), "ISTIKLAL CADDE 44")
  expect_equal(normalize_street("Ataturk Sokak 3",      lang="tr"), "ATATURK SOKAK 3")
  expect_equal(normalize_street("Konak Blv. 101",       lang="tr"), "KONAK BULVAR 101")
  
  # ===========================================================================#
  #                               Scandinavian                                 #
  # ===========================================================================#
  
  expect_equal(normalize_street("Västra Gatan 9",       lang="sv"), "VASTRA GATA 9")
  expect_equal(normalize_street("Østre Vej 12",         lang="da"), "OSTRE VEJ 12")
  expect_equal(normalize_street("Hovedvägen 44",        lang="sv"), "HOVEDVAGEN 44")
})

test_that("normalize_street() vectorized path matches a scalar reference", {

  # Independent, deliberately naive per-string reference implementation.
  # It mirrors the *contract* (preprocess -> per-token exact-then-suffix
  # replacement -> rejoin) but is written separately from the optimized body,
  # so equality is a genuine parity check rather than a tautology.
  ref_one <- function(s, lang, dict) {
    if (is.na(s)) return(NA_character_)
    sn <- s |>
      stringi::stri_trans_general("Any-Latin") |>
      stringi::stri_trans_general("Latin-ASCII") |>
      stringi::stri_trans_toupper() |>
      stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
      stringi::stri_replace_all_regex("\\s+", " ") |>
      stringi::stri_trim_both()
    d  <- if (!is.null(lang)) dict[dict$lang == lang, ] else dict
    ex <- stats::setNames(d$canonical[d$type == "exact"],  d$variant[d$type == "exact"])
    sf <- stats::setNames(d$canonical[d$type == "suffix"], d$variant[d$type == "suffix"])
    sv <- names(sf)[order(nchar(names(sf)), decreasing = TRUE)]
    toks <- stringi::stri_split_regex(sn, " +")[[1]]
    out  <- vapply(toks, function(tok) {
      tl <- tolower(tok)
      if (tl %in% names(ex)) return(unname(ex[[tl]]))
      if (!is.null(lang)) for (suf in sv) {
        if (stringi::stri_endswith_fixed(tl, suf)) {
          base <- stringi::stri_sub(tok, 1, nchar(tok) - nchar(suf))
          return(paste0(base, sf[[suf]]))
        }
      }
      tok
    }, character(1))
    paste(out, collapse = " ")
  }

  set.seed(42)
  langs <- c("de", "en", "fr", "es", "it", "pt", "pl", "nl", "tr", "sv", "da")
  words <- c("Hauptstr.", "Main", "St.", "Rue", "de", "Paris", "Via", "Roma",
             "Kerkstraat", "Blvd.", "12A", "Königsstraße", "Ul.", "Cad.",
             "Gatan", "Avda.", "100", "", "Neue", "Allee")

  for (lg in c(list(NULL), as.list(langs))) {
    inputs <- vapply(1:60, function(i) {
      k <- sample(1:4, 1)
      paste(sample(words, k, replace = TRUE), collapse = " ")
    }, character(1))
    inputs <- c(inputs, NA_character_, "", "   ")
    expected <- vapply(inputs, ref_one, character(1),
                       lang = lg, dict = joinery::street_types,
                       USE.NAMES = FALSE)
    expect_equal(normalize_street(inputs, lang = lg), expected)
  }
})

test_that("normalize_date() normalizes dates to ISO 8601 format", {
  
  # ===========================================================================#
  #                      Automatic parsing (default)                          #
  # ===========================================================================#
  
  # ISO format (YYYY-MM-DD)
  expect_equal(
    normalize_date("2023-12-31"),
    "2023-12-31"
  )
  
  # European format (DD.MM.YYYY)
  expect_equal(
    normalize_date("31.12.2023"),
    "2023-12-31"
  )
  
  # American format (MM/DD/YYYY)
  expect_equal(
    normalize_date("12/31/2023"),
    "2023-12-31"
  )
  
  # European slash format (DD/MM/YYYY)
  expect_equal(
    normalize_date("31/12/2023", orders = c("dmy", "mdy", "ymd")),
    "2023-12-31"
  )
  
  # ===========================================================================#
  #                       Vectorized behavior                                 #
  # ===========================================================================#
  
  x <- c("2023-01-15", "15.01.2023", "01/15/2023")
  out <- normalize_date(x)
  expect_true(all(out == "2023-01-15"))
  
  # Mixed formats with default orders (ymd, dmy, mdy)
  x <- c("2023-12-31", "31.12.2022", "01/15/2021")
  out <- normalize_date(x)
  expect_equal(out[[1]], "2023-12-31")
  expect_equal(out[[2]], "2022-12-31")
  expect_equal(out[[3]], "2021-01-15")
  
  # ===========================================================================#
  #                     Explicit format specification                         #
  # ===========================================================================#
  
  expect_equal(
    normalize_date("31-12-2023", format = "%d-%m-%Y"),
    "2023-12-31"
  )
  
  expect_equal(
    normalize_date("12-31-2023", format = "%m-%d-%Y"),
    "2023-12-31"
  )
  
  expect_equal(
    normalize_date("2023/12/31", format = "%Y/%m/%d"),
    "2023-12-31"
  )
  
  # ===========================================================================#
  #                            Date objects                                   #
  # ===========================================================================#
  
  d <- as.Date("2023-12-31")
  expect_equal(
    normalize_date(d),
    "2023-12-31"
  )
  
  # Vector of Date objects
  dates <- as.Date(c("2023-01-01", "2023-06-15", "2023-12-31"))
  out <- normalize_date(dates)
  expect_equal(out, c("2023-01-01", "2023-06-15", "2023-12-31"))
  
  # ===========================================================================#
  #                              NA handling                                  #
  # ===========================================================================#
  
  expect_true(is.na(normalize_date(NA_character_)))
  
  x <- c("2023-12-31", NA, "31.12.2023")
  out <- normalize_date(x)
  expect_equal(out[[1]], "2023-12-31")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "2023-12-31")
  
  # Date vector with NA
  dates <- as.Date(c("2023-01-01", NA, "2023-12-31"))
  out <- normalize_date(dates)
  expect_equal(out[[1]], "2023-01-01")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "2023-12-31")
  
  # ===========================================================================#
  #                         Parse failures / warnings                         #
  # ===========================================================================#
  
  # Invalid date with explicit format
  expect_warning(
    normalize_date("not-a-date", format = "%Y-%m-%d"),
    "could not be parsed"
  )
  
  # Invalid date with automatic parsing
  expect_warning(
    normalize_date("invalid"),
    "could not be parsed"
  )
  
  # Mixed valid and invalid
  x <- c("2023-12-31", "invalid", "31.12.2023")
  expect_warning(out <- normalize_date(x))
  expect_equal(out[[1]], "2023-12-31")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "2023-12-31")
  
  # ===========================================================================#
  #                          Custom order specification                       #
  # ===========================================================================#
  
  # Prefer dmy over mdy
  expect_equal(
    normalize_date("01/02/2023", orders = c("dmy", "mdy")),
    "2023-02-01"  # interpreted as 1 Feb 2023
  )
  
  # Prefer mdy over dmy
  expect_equal(
    normalize_date("01/02/2023", orders = c("mdy", "dmy")),
    "2023-01-02"  # interpreted as Jan 2, 2023
  )
  
  # ===========================================================================#
  #                           Edge cases                                      #
  # ===========================================================================#
  
  # Empty string
  expect_warning(out <- normalize_date(""))
  expect_true(is.na(out))
  
  # Whitespace only
  expect_warning(out <- normalize_date("   "))
  expect_true(is.na(out))
  
  # Leap year
  expect_equal(
    normalize_date("29.02.2024"),
    "2024-02-29"
  )
  
  # Year-only or month-year formats should fail gracefully
  expect_warning(out <- normalize_date("2023"))
  expect_true(is.na(out))
})



test_that("date_tokens() extracts date components correctly", {
  
  # ===========================================================================#
  #                         Default: all components                           #
  # ===========================================================================#
  
  expect_equal(
    date_tokens("2023-12-31"),
    list(c("2023", "12", "31"))
  )
  
  expect_equal(
    date_tokens("31.12.2023"),
    list(c("2023", "12", "31"))
  )
  
  expect_equal(
    date_tokens("12/31/2023"),
    list(c("2023", "12", "31"))
  )
  
  # ===========================================================================#
  #                       Specific components only                            #
  # ===========================================================================#
  
  # Year only
  expect_equal(
    date_tokens("2023-12-31", components = "year"),
    list("2023")
  )
  
  # Month only
  expect_equal(
    date_tokens("2023-01-15", components = "month"),
    list("01")
  )
  
  # Day only
  expect_equal(
    date_tokens("2023-12-05", components = "day"),
    list("05")
  )
  
  # Year and month
  expect_equal(
    date_tokens("2023-12-31", components = c("year", "month")),
    list(c("2023", "12"))
  )
  
  # Month and day
  expect_equal(
    date_tokens("2023-06-15", components = c("month", "day")),
    list(c("06", "15"))
  )
  
  # Custom order: day, month, year
  expect_equal(
    date_tokens("2023-12-31", components = c("day", "month", "year")),
    list(c("31", "12", "2023"))
  )
  
  # ===========================================================================#
  #                          Vectorized behavior                              #
  # ===========================================================================#
  
  x <- c("2023-01-15", "15.06.2023", "12/31/2023")
  out <- date_tokens(x)
  
  expect_equal(out[[1]], c("2023", "01", "15"))
  expect_equal(out[[2]], c("2023", "06", "15"))
  expect_equal(out[[3]], c("2023", "12", "31"))
  
  # Vectorized with specific component
  x <- c("2023-01-15", "2024-06-20")
  out <- date_tokens(x, components = "year")
  expect_equal(out[[1]], "2023")
  expect_equal(out[[2]], "2024")
  
  # ===========================================================================#
  #                           Date objects                                    #
  # ===========================================================================#
  
  d <- as.Date("2023-12-31")
  expect_equal(
    date_tokens(d),
    list(c("2023", "12", "31"))
  )
  
  dates <- as.Date(c("2023-01-01", "2023-06-15"))
  out <- date_tokens(dates, components = c("year", "month"))
  expect_equal(out[[1]], c("2023", "01"))
  expect_equal(out[[2]], c("2023", "06"))
  
  # ===========================================================================#
  #                           Explicit format                                 #
  # ===========================================================================#
  
  expect_equal(
    date_tokens("31-12-2023", format = "%d-%m-%Y"),
    list(c("2023", "12", "31"))
  )
  
  expect_equal(
    date_tokens("12-31-2023", format = "%m-%d-%Y", components = "year"),
    list("2023")
  )
  
  # ===========================================================================#
  #                              NA handling                                  #
  # ===========================================================================#
  
  expect_equal(
    date_tokens(NA_character_),
    list(character(0))
  )
  
  x <- c("2023-12-31", NA, "31.12.2023")
  out <- date_tokens(x)
  expect_equal(out[[1]], c("2023", "12", "31"))
  expect_equal(out[[2]], character(0))
  expect_equal(out[[3]], c("2023", "12", "31"))
  
  # Date vector with NA
  dates <- as.Date(c("2023-01-01", NA, "2023-12-31"))
  out <- date_tokens(dates, components = "month")
  expect_equal(out[[1]], "01")
  expect_equal(out[[2]], character(0))
  expect_equal(out[[3]], "12")
  
  # ===========================================================================#
  #                         Parse failures / warnings                         #
  # ===========================================================================#
  
  # Invalid date with explicit format
  expect_warning(
    out <- date_tokens("not-a-date", format = "%Y-%m-%d"),
    "could not be parsed"
  )
  expect_equal(out, list(character(0)))
  
  # Invalid date with automatic parsing
  expect_warning(
    out <- date_tokens("invalid"),
    "could not be parsed"
  )
  expect_equal(out, list(character(0)))
  
  # Mixed valid and invalid
  x <- c("2023-12-31", "invalid", "31.12.2023")
  expect_warning(out <- date_tokens(x))
  expect_equal(out[[1]], c("2023", "12", "31"))
  expect_equal(out[[2]], character(0))
  expect_equal(out[[3]], c("2023", "12", "31"))
  
  # ===========================================================================#
  #                          Custom order specification                       #
  # ===========================================================================#
  
  # Prefer dmy over mdy
  expect_equal(
    date_tokens("01/02/2023", orders = c("dmy", "mdy")),
    list(c("2023", "02", "01"))  # interpreted as 1 Feb 2023
  )
  
  # Prefer mdy over dmy
  expect_equal(
    date_tokens("01/02/2023", orders = c("mdy", "dmy")),
    list(c("2023", "01", "02"))  # interpreted as Jan 2, 2023
  )
  
  # ===========================================================================#
  #                          Zero-padding verification                        #
  # ===========================================================================#
  
  # Single-digit month and day should be zero-padded
  expect_equal(
    date_tokens("2023-01-05"),
    list(c("2023", "01", "05"))
  )
  
  expect_equal(
    date_tokens("2023-9-3", format = "%Y-%m-%d"),
    list(c("2023", "09", "03"))
  )
  
  # ===========================================================================#
  #                              Edge cases                                   #
  # ===========================================================================#
  
  # Empty string
  expect_warning(out <- date_tokens(""))
  expect_equal(out, list(character(0)))
  
  # Leap year
  expect_equal(
    date_tokens("29.02.2024", components = c("month", "day")),
    list(c("02", "29"))
  )
  
  # ===========================================================================#
  #                         Input validation                                  #
  # ===========================================================================#
  
  # Invalid component name
  expect_error(
    date_tokens("2023-12-31", components = "invalid"),
    "components.*invalid"
  )
  
  # Mixed valid and invalid components
  expect_error(
    date_tokens("2023-12-31", components = c("year", "invalid")),
    "components.*invalid"
  )
})

test_that("numeric_tokens() works", {

test_that("approximate_date() rounds dates correctly", {
  
  # ===========================================================================#
  #                              Month rounding                               #
  # ===========================================================================#
  
  expect_equal(
    approximate_date("2023-03-15", unit = "month"),
    "2023-03-01"
  )
  
  expect_equal(
    approximate_date("2023-03-01", unit = "month"),
    "2023-03-01"
  )
  
  expect_equal(
    approximate_date("2023-03-31", unit = "month"),
    "2023-03-01"
  )
  
  # Vectorized
  x <- c("2023-01-15", "2023-06-20", "2023-12-31")
  out <- approximate_date(x, unit = "month")
  expect_equal(out, c("2023-01-01", "2023-06-01", "2023-12-01"))
  
  # ===========================================================================#
  #                             Quarter rounding                              #
  # ===========================================================================#
  
  # Q1 (Jan-Mar)
  expect_equal(
    approximate_date("2023-01-15", unit = "quarter"),
    "2023-01-01"
  )
  
  expect_equal(
    approximate_date("2023-03-31", unit = "quarter"),
    "2023-01-01"
  )
  
  # Q2 (Apr-Jun)
  expect_equal(
    approximate_date("2023-04-01", unit = "quarter"),
    "2023-04-01"
  )
  
  expect_equal(
    approximate_date("2023-05-20", unit = "quarter"),
    "2023-04-01"
  )
  
  expect_equal(
    approximate_date("2023-06-30", unit = "quarter"),
    "2023-04-01"
  )
  
  # Q3 (Jul-Sep)
  expect_equal(
    approximate_date("2023-07-15", unit = "quarter"),
    "2023-07-01"
  )
  
  expect_equal(
    approximate_date("2023-09-30", unit = "quarter"),
    "2023-07-01"
  )
  
  # Q4 (Oct-Dec)
  expect_equal(
    approximate_date("2023-10-01", unit = "quarter"),
    "2023-10-01"
  )
  
  expect_equal(
    approximate_date("2023-12-31", unit = "quarter"),
    "2023-10-01"
  )
  
  # Vectorized across all quarters
  x <- c("2023-02-15", "2023-05-20", "2023-08-10", "2023-11-25")
  out <- approximate_date(x, unit = "quarter")
  expect_equal(out, c("2023-01-01", "2023-04-01", "2023-07-01", "2023-10-01"))
  
  # ===========================================================================#
  #                            Half-year rounding                             #
  # ===========================================================================#
  
  # H1 (Jan-Jun)
  expect_equal(
    approximate_date("2023-01-01", unit = "half"),
    "2023-01-01"
  )
  
  expect_equal(
    approximate_date("2023-03-15", unit = "half"),
    "2023-01-01"
  )
  
  expect_equal(
    approximate_date("2023-06-30", unit = "half"),
    "2023-01-01"
  )
  
  # H2 (Jul-Dec)
  expect_equal(
    approximate_date("2023-07-01", unit = "half"),
    "2023-07-01"
  )
  
  expect_equal(
    approximate_date("2023-08-20", unit = "half"),
    "2023-07-01"
  )
  
  expect_equal(
    approximate_date("2023-12-31", unit = "half"),
    "2023-07-01"
  )
  
  # Vectorized
  x <- c("2023-03-15", "2023-08-20")
  out <- approximate_date(x, unit = "half")
  expect_equal(out, c("2023-01-01", "2023-07-01"))
  
  # ===========================================================================#
  #                              Year rounding                                #
  # ===========================================================================#
  
  expect_equal(
    approximate_date("2023-03-15", unit = "year"),
    "2023-01-01"
  )
  
  expect_equal(
    approximate_date("2023-01-01", unit = "year"),
    "2023-01-01"
  )
  
  expect_equal(
    approximate_date("2023-12-31", unit = "year"),
    "2023-01-01"
  )
  
  # Vectorized across multiple years
  x <- c("2021-06-15", "2022-03-20", "2023-09-10")
  out <- approximate_date(x, unit = "year")
  expect_equal(out, c("2021-01-01", "2022-01-01", "2023-01-01"))
  
  # ===========================================================================#
  #                             Decade rounding                               #
  # ===========================================================================#
  
  expect_equal(
    approximate_date("2023-03-15", unit = "decade"),
    "2020-01-01"
  )
  
  expect_equal(
    approximate_date("2020-01-01", unit = "decade"),
    "2020-01-01"
  )
  
  expect_equal(
    approximate_date("2029-12-31", unit = "decade"),
    "2020-01-01"
  )
  
  expect_equal(
    approximate_date("2030-01-01", unit = "decade"),
    "2030-01-01"
  )
  
  expect_equal(
    approximate_date("1995-06-15", unit = "decade"),
    "1990-01-01"
  )
  
  expect_equal(
    approximate_date("2000-06-15", unit = "decade"),
    "2000-01-01"
  )
  
  # Vectorized across decades
  x <- c("1995-03-15", "2005-08-20", "2023-12-31")
  out <- approximate_date(x, unit = "decade")
  expect_equal(out, c("1990-01-01", "2000-01-01", "2020-01-01"))
  
  # ===========================================================================#
  #                              Date objects                                 #
  # ===========================================================================#
  
  d <- as.Date("2023-03-15")
  expect_equal(
    approximate_date(d, unit = "month"),
    "2023-03-01"
  )
  
  dates <- as.Date(c("2023-01-15", "2023-06-20"))
  out <- approximate_date(dates, unit = "quarter")
  expect_equal(out, c("2023-01-01", "2023-04-01"))
  
  # ===========================================================================#
  #                         Different date formats                            #
  # ===========================================================================#
  
  # European format
  expect_equal(
    approximate_date("31.03.2023", unit = "month"),
    "2023-03-01"
  )
  
  # American format
  expect_equal(
    approximate_date("03/31/2023", unit = "quarter"),
    "2023-01-01"
  )
  
  # Explicit format
  expect_equal(
    approximate_date("31-03-2023", format = "%d-%m-%Y", unit = "half"),
    "2023-01-01"
  )
  
  # ===========================================================================#
  #                              NA handling                                  #
  # ===========================================================================#
  
  expect_true(is.na(approximate_date(NA_character_, unit = "month")))
  
  x <- c("2023-03-15", NA, "2023-06-20")
  out <- approximate_date(x, unit = "quarter")
  expect_equal(out[[1]], "2023-01-01")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "2023-04-01")
  
  # Date vector with NA
  dates <- as.Date(c("2023-01-15", NA, "2023-12-31"))
  out <- approximate_date(dates, unit = "year")
  expect_equal(out[[1]], "2023-01-01")
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "2023-01-01")
  
  # ===========================================================================#
  #                         Parse failures / warnings                         #
  # ===========================================================================#
  
  expect_warning(
    out <- approximate_date("not-a-date", unit = "month"),
    "could not be parsed"
  )
  expect_true(is.na(out))
  
  expect_warning(
    out <- approximate_date("invalid", format = "%Y-%m-%d", unit = "year"),
    "could not be parsed"
  )
  expect_true(is.na(out))
  
  # ===========================================================================#
  #                          Custom order specification                       #
  # ===========================================================================#
  
  # Prefer dmy over mdy
  expect_equal(
    approximate_date("01/02/2023", orders = c("dmy", "mdy"), unit = "month"),
    "2023-02-01"  # interpreted as 1 Feb 2023
  )
  
  # Prefer mdy over dmy
  expect_equal(
    approximate_date("01/02/2023", orders = c("mdy", "dmy"), unit = "month"),
    "2023-01-01"  # interpreted as Jan 2, 2023
  )
  
  # ===========================================================================#
  #                              Edge cases                                   #
  # ===========================================================================#
  
  # Leap year February
  expect_equal(
    approximate_date("2024-02-29", unit = "month"),
    "2024-02-01"
  )
  
  expect_equal(
    approximate_date("2024-02-29", unit = "quarter"),
    "2024-01-01"
  )
  
  # Year 2000 (edge of millennium)
  expect_equal(
    approximate_date("2000-06-15", unit = "decade"),
    "2000-01-01"
  )
  
  # Year 1999
  expect_equal(
    approximate_date("1999-12-31", unit = "decade"),
    "1990-01-01"
  )
})

  
  # --------------------------------------------------------------------------
  # Simple numeric tokens
  # --------------------------------------------------------------------------
  expect_equal(numeric_tokens("12"), list("12"))
  expect_equal(numeric_tokens("003"), list("003"))
  expect_equal(numeric_tokens("12A"), list("12A"))
  expect_equal(numeric_tokens("12B 14C"), list(c("12B", "14C")))
  
  # --------------------------------------------------------------------------
  # Range expansion (only when cleaned form matches '^[0-9]+ [0-9]+$')
  # --------------------------------------------------------------------------
  expect_equal(numeric_tokens("12-14"), list(c("12", "13", "14")))
  expect_equal(numeric_tokens("7–9"), list(c("7", "8", "9")))
  expect_equal(numeric_tokens("20 - 22"), list(c("20", "21", "22")))
  expect_equal(numeric_tokens("3/5"), list(c("3", "4", "5")))
  
  # These *do not* expand into ranges under the current function:
  expect_equal(numeric_tokens("12A 14-16"), list(c("12A", "14", "16")))
  expect_equal(numeric_tokens("12B/14"), list(c("12B", "14")))
  expect_equal(numeric_tokens("7 bis 9"), list(c("7", "9")))

  # --------------------------------------------------------------------------
  # NA / empty / non-numeric inputs
  # --------------------------------------------------------------------------
  expect_equal(numeric_tokens(NA_character_), list(character(0)))
  expect_equal(numeric_tokens(""), list(character(0)))
  expect_equal(numeric_tokens(" "), list(character(0)))
  expect_equal(numeric_tokens("A B C"), list(character(0)))
  
  # --------------------------------------------------------------------------
  # Vectorized behavior
  # --------------------------------------------------------------------------
  x <- c("12-14", "5A", "30/32", "X", NA)
  out <- numeric_tokens(x)
  
  expect_equal(out[[1]], c("12", "13", "14"))
  expect_equal(out[[2]], "5A")
  expect_equal(out[[3]], c("30", "31", "32"))
  expect_equal(out[[4]], character(0))
  expect_equal(out[[5]], character(0))
  
  # --------------------------------------------------------------------------
  # Destructive mode: digits only
  # --------------------------------------------------------------------------
  expect_equal(
    numeric_tokens("12A", destructive = TRUE),
    list("12")
  )
  
  expect_equal(
    numeric_tokens("12A 14B", destructive = TRUE),
    list(c("12","13","14"))   # because "12 14" is a valid range
  )
  
  expect_equal(
    numeric_tokens("X Y Z", destructive = TRUE),
    list(character(0))
  )
  
  expect_equal(
    numeric_tokens("10A/12B", destructive = TRUE),
    list(c("10","11","12"))
  )
  
  # --------------------------------------------------------------------------
  # Edge cases
  # --------------------------------------------------------------------------
  expect_equal(numeric_tokens("14-12"), list(c("14","12")))  # reversed, no range
  expect_equal(numeric_tokens("10..12"), list(c("10","11","12")))
  expect_equal(numeric_tokens("4—6"), list(c("4","5","6")))
})

test_that("drop_numeric_tokens() removes house-number tokens", {

  # keep_letters = TRUE (default): only pure-digit tokens dropped
  expect_equal(
    drop_numeric_tokens(list(c("MAIN", "12", "ST"))),
    list(c("MAIN", "ST"))
  )
  expect_equal(
    drop_numeric_tokens(list(c("MAIN", "12A"))),
    list(c("MAIN", "12A"))           # number-letter token retained
  )

  # keep_letters = FALSE: any token containing a digit dropped
  expect_equal(
    drop_numeric_tokens(list(c("MAIN", "12A")), keep_letters = FALSE),
    list("MAIN")
  )

  # vectorized over the list; empty + all-numeric vectors handled
  out <- drop_numeric_tokens(list(c("A", "1"), character(0), c("7", "9")))
  expect_equal(out[[1]], "A")
  expect_equal(out[[2]], character(0))
  expect_equal(out[[3]], character(0))

  # symmetric inverse of numeric_tokens(): the two partition a token set
  toks <- c("HAUPTSTRASSE", "12", "B")
  kept <- drop_numeric_tokens(list(toks))[[1]]
  expect_false(any(grepl("^[0-9]+$", kept)))

  # validation: list input required
  expect_error(drop_numeric_tokens(c("a", "1")), "must be a list")
})

test_that("drop_numeric_tokens() works inside a search_strategy pipeline", {
  dt <- data.table::data.table(
    id     = c("a", "b"),
    street = c("Hauptstrasse 12", "Hauptstrasse 99")
  )
  strat <- search_strategy(
    street ~ normalize_text() + word_tokens() + drop_numeric_tokens(),
    threshold = 0.5
  )
  dups <- detect_duplicates(dt, id = "id", strategy = strat)
  # house numbers stripped -> the two share only the street name and match
  expect_true(nrow(dups) >= 1L)
})

test_that("filter_stopwords() removes stopwords correctly", {
  
  # Basic removal
  tokens <- list(c("This", "is", "a", "test"))
  stop <- c("is", "a")
  expect_equal(
    filter_stopwords(tokens, stop),
    list(c("This", "test"))
  )
  
  # Case-insensitivity
  tokens <- list(c("Hello", "WORLD"))
  stop <- c("world")
  expect_equal(
    filter_stopwords(tokens, stop),
    list("Hello")
  )
  
  # No stopwords removed
  tokens <- list(c("apple", "banana"))
  stop <- c("pear")
  expect_equal(
    filter_stopwords(tokens, stop),
    list(c("apple", "banana"))
  )
  
  # Empty tokens
  tokens <- list(character(0))
  stop <- c("is")
  expect_equal(
    filter_stopwords(tokens, stop),
    list(character(0))
  )
  
  # Multiple elements
  tokens <- list(
    c("one", "two", "three"),
    c("red", "blue")
  )
  stop <- c("two", "blue")
  out <- filter_stopwords(tokens, stop)
  expect_equal(out[[1]], c("one", "three"))
  expect_equal(out[[2]], "red")
  
  # Input validation
  expect_error(filter_stopwords("not_a_list", c("a")))
  expect_error(filter_stopwords(list("ok"), 123))
})


test_that("token_shapes() computes correct shapes", {
  
  # Simple letter, digit, hybrid
  tokens <- list(c("MULLER", "A12B", "99X"))
  expect_equal(
    token_shapes(tokens),
    list(c("AAAAAA", "ANNA", "NNA"))
  )
  
  # Mixed lowercase — should still be treated as letters
  tokens <- list(c("abc", "123", "a1b2"))
  expect_equal(
    token_shapes(tokens),
    list(c("XXX", "NNN", "XNXN"))
  )
  
  # Non-alphanumeric generate "X"
  tokens <- list(c("A-B", "C_D"))
  expect_equal(
    token_shapes(tokens),
    list(c("AXA", "AXA"))
  )
  
  # Empty list element
  tokens <- list(character(0))
  expect_equal(
    token_shapes(tokens),
    list(character(0))
  )
  
  # Input validation
  expect_error(token_shapes("not_a_list"))
})


test_that("extract_initials() extracts first letters", {
  
  tokens <- list(c("Anna", "BERTA", "C3PO"))
  expect_equal(
    extract_initials(tokens),
    list(c("A", "B", "C"))
  )
  
  # One-element list
  expect_equal(
    extract_initials(list("HELLO")),
    list("H")
  )
  
  # Empty list element
  expect_equal(
    extract_initials(list(character(0))),
    list(character(0))
  )
  
  # Multi-token vector
  tokens <- list(c("X1", "Y2", "Z3"))
  expect_equal(
    extract_initials(tokens),
    list(c("X", "Y", "Z"))
  )
  
  # Input validation
  expect_error(extract_initials("not_a_list"))
})

test_that("fuzzy_tokens() performs fuzzy clustering correctly", {
  # ---------------------------------------------------------------------------
  # Basic German surname corruption
  # ---------------------------------------------------------------------------
  x <- c("Neumann", "Neumaxn", "Neuman")
  out <- fuzzy_tokens(x, max_dist = 2, method = "osa")
  
  expect_equal(out[[1]], "NEUMANN")
  expect_equal(out[[2]], "NEUMANN")
  expect_equal(out[[3]], "NEUMANN")
  
  
  # ---------------------------------------------------------------------------
  # City name typos / vowel changes
  # ---------------------------------------------------------------------------
  x <- c("Friedberg", "Frielberg")
  out <- fuzzy_tokens(x, max_dist = 2)
  
  expect_equal(out[[1]], "FRIEDBERG")
  expect_equal(out[[2]], "FRIEDBERG")
  
  
  # ---------------------------------------------------------------------------
  # Street names: Unicode, ß → SS, spelling variants
  # ---------------------------------------------------------------------------
  x <- c("Dorfstraße", "Dorfstrasse", "Dorfstrase")
  out <- fuzzy_tokens(x, max_dist = 2, method = "lv")
  
  expect_equal(out[[1]], "DORFSTRASSE")
  expect_equal(out[[2]], "DORFSTRASSE")
  expect_equal(out[[3]], "DORFSTRASSE")
  
  
  # ---------------------------------------------------------------------------
  # Mixed: very different tokens must stay separate
  # ---------------------------------------------------------------------------
  x <- c("Mueller", "Schmidt")
  out <- fuzzy_tokens(x, max_dist = 1)
  
  expect_equal(out[[1]], "MUELLER")
  expect_equal(out[[2]], "SCHMIDT")
  
  
  
  # ---------------------------------------------------------------------------
  # JW method for long strings with vowel differences
  # ---------------------------------------------------------------------------
  x <- c("Katharina", "Katarina")
  out <- fuzzy_tokens(x, max_dist = 0.15, method = "jw")
  
  # JW returns a similarity-based distance, canonical selection preserved
  # Both should map to first token
  expect_equal(out[[1]], "KATHARINA")
  expect_equal(out[[2]], "KATHARINA")
  
  
  # ---------------------------------------------------------------------------
  # min_nchar should remove short tokens BEFORE fuzzy clustering
  # ---------------------------------------------------------------------------
  x <- c("A B C", "A BCX")
  out <- fuzzy_tokens(x, min_nchar = 2)
  
  expect_equal(out[[1]], character(0))   # all tokens too short
  expect_equal(out[[2]], "BCX")          # only BCX survives
  
  
  # ---------------------------------------------------------------------------
  # NA handling
  # ---------------------------------------------------------------------------
  x <- c("Neumann", NA, "Neuamn")
  out <- fuzzy_tokens(x)
  
  expect_equal(out[[1]], "NEUMANN")
  expect_true(is.na(out[[2]]))   # NA stays NA
  expect_equal(out[[3]], "NEUMANN") 
  
  
  # ---------------------------------------------------------------------------
  # Vectorization: per-row clustering independent
  # ---------------------------------------------------------------------------
  x <- c("Neumann", "Mueller", "Neumaxn mueler")
  out <- fuzzy_tokens(x, max_dist = 2)
  
  expect_equal(out[[1]], "NEUMANN")
  expect_equal(out[[2]], "MUELLER")
  
  # row 3 has two fuzzy groups
  expect_true(
    all(out[[3]] %in% c("NEUMANN", "MUELER", "MUELLER"))
  )
})


test_that("strip_vowels() removes vowels correctly", {
  
  # Basic German example
  expect_equal(
    strip_vowels("Müller"),
    "MLLR"
  )
  
  # Accents removed before stripping vowels
  expect_equal(
    strip_vowels("Café Noir"),
    "CF NR"
  )
  
  # Multilingual examples
  expect_equal(
    strip_vowels("José María"),
    "JS MR"
  )
  
  expect_equal(
    strip_vowels("Ångström"),
    "NGSTRM"
  )
  
  # Mixed-case handling
  expect_equal(
    strip_vowels("AnNa"),
    "NN"
  )
  
  # Multiple words / spacing normalization
  expect_equal(
    strip_vowels("  A   E   IOU  "),
    ""      # all vowels removed → empty string
  )
  
  # Empty or whitespace-only
  expect_equal(
    strip_vowels(""),
    ""
  )
  
  expect_equal(
    strip_vowels("   "),
    ""
  )
  
  # Vectorized behaviour
  x <- c("Müller", "Café", "Anna Marie")
  out <- strip_vowels(x)
  
  expect_equal(out[[1]], "MLLR")
  expect_equal(out[[2]], "CF")
  expect_equal(out[[3]], "NN MR")
  
  # NA handling
  expect_true(is.na(strip_vowels(NA_character_)))
  
  # Non-letter characters remain
  expect_equal(
    strip_vowels("A1E2I3O4U5"),
    "12345"
  )
  
  # Consonant-only input unchanged
  expect_equal(
    strip_vowels("BCDFG"),
    "BCDFG"
  )
})


test_that("prepare_search_data tolerates a non-unique id (warns, no cartesian crash)", {
  # Regression: a non-unique id once exploded the block-attach merge into a
  # cartesian (data.table allow.cartesian guard). block_dt must be unique by id,
  # and the duplication must surface as a warning rather than a crash.
  dt <- data.table::data.table(
    id     = c("a", "a", "a", "b", "c"),       # "a" repeated
    name   = c("mueller", "mueller", "mueller", "schmidt", "mueller"),
    street = c("hauptstr", "hauptstr", "hauptstr", "ringstr", "hauptstr"),
    blk    = c("1", "1", "1", "1", "1")
  )
  strat <- search_strategy(
    name   ~ normalize_text + word_tokens(min_nchar = 3),
    street ~ normalize_text + word_tokens(min_nchar = 3),
    weights = c(name = 0.6, street = 0.4), block_by = "blk", threshold = 0.9
  )
  expect_warning(
    tok <- prepare_search_data(dt, "id", strat),
    "not unique"
  )
  # Block column attached exactly once per (id, token) — no row multiplication.
  expect_true(all(tok$blk == "1"))
  expect_equal(data.table::uniqueN(tok$id), 3L)
  # A unique-id frame must not warn.
  dt2 <- dt[!duplicated(id)]
  expect_no_warning(prepare_search_data(dt2, "id", strat))
})
