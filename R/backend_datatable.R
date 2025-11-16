
DT_tbl <- new_S3_class("data.table")
method(prepare_search_data,
       list(DT_tbl, Search_Strategy)) <- function(data, strategy) {
         browser()
         
         # Make a safe working copy
         dt <- data.table::copy(data)
         
         preparers <- strategy@preparers   # named list: column → preparer function
         
         # For each preparer, apply its function to the column
         out <- data.table::rbindlist(
           lapply(names(preparers), function(col) {
             
             prep_fun <- preparers[[col]]   # R function produced by parse_formula()
             
             if (!col %in% names(dt)) {
               stop(sprintf("Column '%s' not found in data", col), call. = FALSE)
             }
             
             # Apply the preparer to the column value
             processed <- prep_fun(dt[[col]])
             
             # Output a standard long format table
             data.table::data.table(
               column = col,
               value  = processed
             )
           }),
           use.names = TRUE,
           fill = TRUE
         )
         
         out[]
       }


#' Normalize text string
#'
#' This function converts a text string to upper case, transliterates it based on the specified
#' transliteration scheme, retains only alphanumeric characters and spaces, and removes extra spaces.
#'
#' @param .text A character string or vector to be normalized.
#' @param .transliteration A character string specifying the transliteration scheme to be used,
#'        defaulting to "De-ASCII".
#'
#' @return Returns a normalized, upper-case version of the input text, with non-alphanumeric characters
#'         and extra spaces removed.
#'
#' @examples
#' normalize_text("Café Coñac")
#' normalize_text("Straße", .transliteration = "Latin-ASCII")
#' @export
#' @import stringi
normalize_text <- function(.text, .transliteration = "De-ASCII") {
  #Validate inputes to the generate_ngrams function
  c("Input .text must be a string" = is.character(.text),
    "Input .transliteration must be a string" = is.character(.transliteration)
  ) |>
    validate_inputs()
  
  # String to upper case characters
  .text <- stri_trans_toupper(.text)
  # Transliterate other language specific characters
  .text <- stri_trans_general(.text, .transliteration)
  # Keep only alphanumeric characters and spaces
  .text <- stri_replace_all_regex(.text, "[^A-Za-z0-9 ]", "")
  # Remove additional spaces if they exist
  .text <- stri_trim_both(.text)
  .text <- stri_replace_all_regex(.text, "\\s+", " ")
  return(.text)
}

#' Convert Text to Metaphone Encoding
#'
#' This function converts a text string to its Metaphone encoding. The Metaphone algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param text A character string or vector to be converted to Metaphone encoding.
#'
#' @return Returns the Metaphone encoded version of the input text.
#' @examples
#' as_metaphone("Café")
#' as_metaphone("Straße")
#' @export
#' @import phonics
as_metaphone <- function(text) {
  # Normalize accents: Café → Cafe
  x_norm <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  
  # Handle ß separately (ASCII//TRANSLIT turns it into "ss" on some locales, but not reliably)
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)
  
  phonics::metaphone(x_norm,clean = FALSE)
}


#' Convert Text to Soundex Encoding
#'
#' This function converts a text string to its Soundex encoding. The Soundex algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param text A character string or vector to be converted to Soundex encoding.
#'
#' @return Returns the Soundex encoded version of the input text.
#' @examples
#' as_soundex("Café")
#' as_soundex("Straße")
#' @export
#' @import phonics
as_soundex <- function(.text){
  # Validate inputs to the function
  c("Input text must be a string" = is.character(.text)
  ) |>
    validate_inputs()
  
  # Normalize accents: Café → Cafe
  x_norm <- iconv(.text, from = "", to = "ASCII//TRANSLIT")
  
  # Handle ß separately (ASCII//TRANSLIT turns it into "ss" on some locales, but not reliably)
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)
  
  phonics::soundex(x_norm,clean = FALSE)
}


#' Convert Text to Cologne Phonetic Encoding
#'
#' This function converts a text string to its Cologne Phonetic encoding. The Cologne Phonetic algorithm
#' is used to encode words phonetically by reducing them to a simplified representation based on their
#' pronunciation, particularly suited for German language.
#'
#' @param .text A character string or vector to be converted to Cologne Phonetic encoding.
#'
#' @return Returns the Cologne Phonetic encoded version of the input text.
#' @examples
#' as_cologne("Café")
#' as_cologne("Straße")
#' @export
#' @import phonics
as_cologne <- function(.text){
  c("Input text must be a string" = is.character(.text)) |>
    validate_inputs()
  
  x_norm <- iconv(.text, from = "", to = "ASCII//TRANSLIT")
  x_norm <- gsub("ß", "ss", x_norm, ignore.case = TRUE)
  
  phonics::cologne(x_norm, clean = FALSE)
}


#' Return a list of word tokens for the .text separated by spaces.
#'
#' This function splits the input text into words based on spaces. It returns a vector of the words
#' found in the text. This function is useful for natural language processing tasks where word-level
#' manipulation of text is required.
#'
#' @param text A character string from which words will be extracted.
#' @param min_length An integer specifying the minimum length of words to keep. Defaults to 0.
#'
#' @return Returns a vector of words extracted from the input text.
#'
#' @examples
#' word_tokens("This is an example.")
#' word_tokens("Another, test; string.")
#' @export
word_tokens <- function(text,min_length=0){
  # Validate inputs to the function
  c("Input text must be a string" = is.character(text),
    "Input min_length must be a integer valued numeric" = min_length == as.integer(min_length)
  ) |>
    validate_inputs()
  
  # Split the text into words based on spaces
  words <- strsplit(text, "\\s+")
  # Remove empty elements if any (this can happen with multiple spaces)
  words <- words[nzchar(words)]
  
  # Filter out words shorter than .min_length
  if (min_length > 0) {
    words <- map(words,function(x){
      filter <- nchar(x)>=min_length
      x[filter]
    })
  }
  return(words)
}


#' Generate n-grams from text
#'
#' This function generates n-grams from a given text string. An n-gram is a contiguous sequence of n items
#' from a given sample of text or speech. This function will return a list of all possible n-grams of length n.
#'
#' @param text A character string or vector from which to generate n-grams.
#' @param n An integer specifying the length of each n-gram.
#'
#' @return Returns a list of n-grams generated from the input text. If the text length is less than n, returns
#'         an empty character vector.
#'
#' @examples
#' generate_ngrams("hello", 2)
#' generate_ngrams("an example", 3)
#' @export
#' @import data.table
#' @import stringi
generate_ngrams <- function(text, n) {
  # Validate inputs
  c(
    "Input text must be a string" = is.character(text),
    "Input n must be an integer valued numeric" = n == as.integer(n)
  ) |>
    validate_inputs()
  
  int_df <- data.table::data.table(text = text)
  int_df[, len_text := stri_length(text)]
  
  # Generate n-grams for one string using purrr-shim syntax
  generate_ngrams_single <- function(s) {
    len_s <- stri_length(s)
    if (len_s < n) return(character(0))
    
    map_chr(
      seq_len(len_s - n + 1),
      ~ stri_sub(s, .x, .x + n - 1)
    )
  }
  
  # Apply with purrr-shim map()
  int_df[, ngrams := map(text, generate_ngrams_single)]
  
  int_df$ngrams
}

#' Use similarity dictionary to group similar tokens together
#'
#' This function looks up a token in the similarity dictionary and returns the corresponding token group for a token.
#'
#' @param text A character string or vector representing the token to be looked up.
#' @param dict A data table containing the similarity dictionary with tokens and their respective groups.
#'
#' @return Returns the token group corresponding to the input token.
#'
#' @examples
#' dict <- data.table(tokens = c("example", "sample"), token_group = c("example/sample", "example/sample"))
#' use_dictionary("example", dict)
#' use_dictionary("nonexistent", dict)
#' @export
use_dictionary <- function(text, dict) {
  # Validate inputs to the function
  c("Input dict must be a data.table" = data.table::is.data.table(dict)) |>
    validate_inputs()
  
  lookup_row <- function(r){
    dict[tokens %in% r]$token_group
  }
  
  map(text,lookup_row)
}
