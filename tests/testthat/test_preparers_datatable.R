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

test_that("numeric_tokens() works", {
  
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

