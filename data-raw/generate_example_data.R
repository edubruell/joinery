# data-raw/make-linkage-example.R
library(tibble)
library(dplyr)
library(purrr)
library(stringi)

set.seed(42)

# ==============================================================================
# 1. Noise helpers
# ==============================================================================

typo <- function(x) {
  if (runif(1) < 0.85) return(x)  # 15% get typo
  if (nchar(x) < 4) return(x)
  pos <- sample.int(nchar(x), 1)
  substr(x, pos, pos) <- sample(c(letters, "ä","ö","ü"), 1)
  x
}

drop_vowel <- function(x) {
  if (runif(1) < 0.9) return(x)
  sub("[aeiouAEIOUäöüÄÖÜ]", "", x)
}

swap_tokens <- function(x) {
  tokens <- strsplit(x, " ")[[1]]
  if (length(tokens) < 2 || runif(1) < 0.9) return(x)
  paste(sample(tokens), collapse = " ")
}

distort <- function(x) x |> typo() |> drop_vowel() |> swap_tokens()


# ==============================================================================
# 2. Geography (unchanged)
# ==============================================================================


geo_map <- tribble(
  ~Ort, ~Kreis,
  # Städteregion Aachen
  "Aachen", "Städteregion Aachen",
  "Herzogenrath", "Städteregion Aachen",
  "Würselen", "Städteregion Aachen",
  "Alsdorf", "Städteregion Aachen",
  "Stolberg", "Städteregion Aachen",
  "Eschweiler", "Städteregion Aachen",
  "Baesweiler", "Städteregion Aachen",
  "Simmerath", "Städteregion Aachen",
  "Monschau", "Städteregion Aachen",
  "Roetgen", "Städteregion Aachen",
  # Köln - Stadt
  "Köln", "Stadt Köln",
  "Frechen", "Rhein-Erft-Kreis",
  "Hürth", "Rhein-Erft-Kreis",
  "Pulheim", "Rhein-Erft-Kreis",
  "Bergheim", "Rhein-Erft-Kreis",
  # München - Stadt
  "München", "Stadt München",
  "Garching", "Landkreis München",
  "Unterschleißheim", "Landkreis München",
  "Ismaning", "Landkreis München",
  "Taufkirchen", "Landkreis München",
  # Frankfurt - Stadt
  "Frankfurt am Main", "Stadt Frankfurt am Main",
  "Offenbach", "Stadt Offenbach am Main",
  "Neu-Isenburg", "Kreis Offenbach",
  "Bad Vilbel", "Wetteraukreis",
  "Maintal", "Main-Kinzig-Kreis",
  # Stuttgart - Stadt
  "Stuttgart", "Stadt Stuttgart",
  "Esslingen", "Landkreis Esslingen",
  "Böblingen", "Landkreis Böblingen",
  "Filderstadt", "Landkreis Esslingen",
  "Sindelfingen", "Landkreis Böblingen",
  # Hannover - Region
  "Hannover", "Region Hannover",
  "Garbsen", "Region Hannover",
  "Laatzen", "Region Hannover",
  "Langenhagen", "Region Hannover",
  "Seelze", "Region Hannover",
  "Ronnenberg", "Region Hannover",
  # Leipzig - Stadt
  "Leipzig", "Stadt Leipzig",
  "Markkleeberg", "Landkreis Leipzig",
  "Schkeuditz", "Landkreis Nordsachsen",
  "Taucha", "Landkreis Nordsachsen",
  # Dresden - Stadt
  "Dresden", "Stadt Dresden",
  "Freital", "Landkreis Sächsische Schweiz-Osterzgebirge",
  "Radebeul", "Landkreis Meißen",
  "Coswig", "Landkreis Meißen",
  # Nürnberg - Stadt
  "Nürnberg", "Stadt Nürnberg",
  "Fürth", "Stadt Fürth",
  "Erlangen", "Stadt Erlangen",
  "Schwabach", "Stadt Schwabach",
  # Bremen - Stadt
  "Bremen", "Stadt Bremen",
  "Bremerhaven", "Stadt Bremerhaven",
  # Regensburg - Stadt
  "Regensburg", "Stadt Regensburg",
  "Lappersdorf", "Landkreis Regensburg",
  "Pentling", "Landkreis Regensburg",
  "Wenzenbach", "Landkreis Regensburg",
  # Augsburg - Stadt
  "Augsburg", "Stadt Augsburg",
  "Königsbrunn", "Landkreis Augsburg",
  "Neusäß", "Landkreis Augsburg",
  "Friedberg", "Landkreis Aichach-Friedberg",
  # Ruhrgebiet
  "Dortmund", "Stadt Dortmund",
  "Essen", "Stadt Essen",
  "Bochum", "Stadt Bochum",
  "Herne", "Stadt Herne",
  "Gelsenkirchen", "Stadt Gelsenkirchen"
)

# ==============================================================================
# 3. Streets
# ==============================================================================
streets <- c(
  # Monopoly streets
  "Badstraße", "Turmstraße", "Chausseestraße", "Elisenstraße",
  "Poststraße", "Seestraße", "Hafenstraße", "Neue Straße",
  "Münchener Straße", "Wiener Straße", "Berliner Straße",
  "Theaterstraße", "Museumstraße", "Opernplatz", "Lessingstraße",
  "Schillerstraße", "Goethestraße", "Rathausplatz", "Hauptstraße",
  "Bahnhofstraße", "Parkstraße", "Schlossallee",
  # Common streets
  "Dorfstraße", "Lindenweg", "Bergstraße", "Kirchplatz",
  "Marktplatz", "Paulstraße", "Königsallee", "Gartenstraße",
  "Friedrichstraße", "Mozartstraße", "Beethovenstraße", "Bismarckstraße",
  "Wilhelmstraße", "Kaiserstraße", "Schulstraße", "Waldstraße",
  "Ringstraße", "Feldweg",
  "Mühlweg", "Tannenweg", "Rosenweg", "Buchenweg",
  "Am Markt", "Am Bahnhof", "An der Kirche", "Unter den Linden",
  "Alte Dorfstraße", "Kurze Straße", "Lange Straße",
  "Sandweg", "Steinweg", "Wiesenweg", "Heideweg",
  "Sonnenstraße", "Talstraße", "Hohestraße",
  # Adenauer & Brandt
  "Konrad-Adenauer-Straße", "Willy-Brandt-Straße",
  # Cardinal directions & regional
  "Nordstraße", "Südstraße", "Oststraße", "Weststraße",
  # Nature & trees
  "Ahornweg", "Eichenweg", "Fichtenweg", "Birkenweg",
  # Historical figures
  "Heinrich-Heine-Straße", "Albert-Einstein-Straße", "Clara-Zetkin-Straße",
  # Trade & craft
  "Mühlenstraße", "Schmiedestraße", "Bäckerstraße", "Gerberstraße",
  # Geographic features
  "Flussstraße", "Brückenstraße", "Auweg", "Hangweg"
)


# ==============================================================================
# 4. Names (NOW WITH WEIGHTS)
# ==============================================================================

# Approximate real distributions
german_first <- c("Anna","Marie","Sophie","Laura","Julia","Lena","Emma","Hannah","Lea","Sarah",
                  "Mia","Emilia","Clara","Charlotte","Johanna","Amelie","Franziska","Isabella",
                  "Peter","Michael","Thomas","Karl","Heinz","Andreas","Christian","Martin",
                  "Uwe","Jürgen","Stefan","Lukas","Tim","Klaus","Hans","Werner","Helmut",
                  "Wolfgang","Matthias","Sebastian","Daniel","Alexander","Markus","Florian")

german_first_w <- c(0.055,0.050,0.045,0.040,0.038,0.035,0.033,0.030,0.028,0.027,
                    0.025,0.024,0.023,0.022,0.021,0.020,0.019,0.018,
                    0.048,0.045,0.042,0.028,0.025,0.024,0.023,0.022,
                    0.018,0.017,0.016,0.020,0.017,0.016,0.015,0.014,0.013,
                    0.012,0.019,0.018,0.021,0.020,0.019,0.017)

german_last <- c("Schmidt","Müller","Weber","Schneider","Fischer","Wagner","Becker",
                 "Hoffmann","Koch","Bauer","Schulz","Meyer","Richter","Klein","Wolf",
                 "Schröder","Neumann","Braun","Zimmermann","Krüger","Schmitt","Lange",
                 "Hofmann","Krause","Meier","Lehmann","Huber","Mayer","Herrmann","König")

german_last_w <- c(0.070,0.065,0.060,0.055,0.050,0.058,0.052,0.048,0.045,0.042,
                   0.040,0.038,0.036,0.034,0.032,0.038,0.036,0.034,0.032,0.030,
                   0.031,0.029,0.028,0.027,0.026,0.025,0.024,0.023,0.022,0.021)

# Turkish names (approx 3-4% of German population)
turkish_first <- c("Mehmet","Ali","Fatma","Hassan","Aylin","Ahmet","Emine","Hasan","Zeynep","Mustafa")
turkish_first_w <- c(0.15,0.12,0.11,0.10,0.09,0.10,0.09,0.08,0.08,0.08)

turkish_last <- c("Yilmaz","Demir","Öztürk","Kaya","Arslan","Çelik","Koç","Şahin","Özkan","Yildiz")
turkish_last_w <- c(0.13,0.11,0.10,0.10,0.09,0.10,0.09,0.09,0.10,0.09)

# Polish names (approx 2-3% of German population)
polish_first <- c("Jan","Anna","Piotr","Katarzyna","Andrzej","Maria","Tomasz","Magdalena","Krzysztof","Joanna")
polish_first_w <- c(0.12,0.11,0.10,0.10,0.09,0.10,0.09,0.09,0.10,0.10)

polish_last <- c("Nowak","Kowalski","Wiśniewski","Wójcik","Kowalczyk","Kamiński","Lewandowski","Zieliński","Szymański","Woźniak")
polish_last_w <- c(0.11,0.11,0.10,0.10,0.10,0.10,0.10,0.09,0.10,0.09)


# ==============================================================================
# 5. Create BASE dataset (REALISTIC NAME COMBINATIONS)
# ==============================================================================

n <- 3000

base <- tibble(
  id_base = sprintf("B%04d", 1:n),
  
  ethnic = sample(c("german","turkish","polish"), n, TRUE, c(0.93, 0.04, 0.03)),
  
  Vorname = case_when(
    ethnic == "turkish" ~ sample(turkish_first, n, TRUE, turkish_first_w),
    ethnic == "polish" ~ sample(polish_first, n, TRUE, polish_first_w),
    TRUE ~ sample(german_first, n, TRUE, german_first_w)
  ),
  
  Nachname = case_when(
    ethnic == "turkish" ~ sample(turkish_last, n, TRUE, turkish_last_w),
    ethnic == "polish" ~ sample(polish_last, n, TRUE, polish_last_w),
    TRUE ~ sample(german_last, n, TRUE, german_last_w)
  ),
  
  Strasse = sample(streets, n, TRUE),
  Hausnummer = as.character(sample(1:150, n, TRUE)),
  Ort = sample(geo_map$Ort, n, TRUE)
) |>
  left_join(geo_map, by = "Ort") |>
  select(id_base, Vorname, Nachname, Strasse, Hausnummer, Ort, Kreis)



# ==============================================================================
# 6. Distortion helpers for TARGET dataset
# ==============================================================================

initialize_first <- function(x) paste0(substr(x,1,1), ".")

add_or_remove_middle <- function(x, ethnic) {
  if (runif(1) > 0.15) return(x)
  
  mids <- switch(ethnic,
                 german = c("Maria", "Marie", "Sophie", "Elisabeth", "Peter", "Josef", "Wilhelm"),
                 turkish = c("Ali", "Mehmet", "Hassan", "Fatma", "Ayşe"),
                 polish = c("Maria", "Anna", "Jan", "Stanisław", "Józef"),
                 c("Maria", "Jean", "Lee")
  )
  
  if (runif(1) < 0.5) paste(x, sample(mids, 1)) else strsplit(x, " ")[[1]][1]
}

initial_middle <- function(x) {
  parts <- strsplit(x, " ")[[1]]
  if (length(parts) < 2) return(x)
  paste0(substr(parts[1],1,1), ". ", paste(parts[-1], collapse=" "))
}

street_distort <- function(x) {
  x <- if (runif(1) < 0.2) gsub("straße","strasse", x, TRUE) else x
  x <- if (runif(1) < 0.2) gsub("strasse","str.", x, TRUE) else x
  typo(x)
}

house_distort <- function(hn) {
  if (runif(1) < 0.15) return(paste0(hn, sample(c("A","B","C"),1)))
  if (runif(1) < 0.10) return(paste0(hn, sample(letters,1)))
  hn
}


add_title <- function(name, ethnic = "german") {
  if (runif(1) < 0.92) return(name)
  
  titles <- switch(ethnic,
                   german = c("Dr.", "Prof.", "Prof. Dr."),
                   turkish = c("Dr.", "Prof.", "Prof. Dr."),
                   polish = c("Dr.", "Prof.", "Prof. Dr."),
                   c("Dr.", "Prof.", "Prof. Dr.")
  )
  
  probs <- c(0.75, 0.15, 0.10)
  
  paste(sample(titles, 1, prob = probs), name)
}

remove_title <- function(name) {
  gsub("^(Dr\\.|Prof\\.|Prof\\. Dr\\.) ", "", name)
}

vary_title <- function(name) {
  if (runif(1) < 0.5) {
    add_title(name)
  } else {
    remove_title(name)
  }
}

# ==============================================================================
# 7. TARGET dataset
# ==============================================================================

match_idx <- sample(n, round(0.80*n))
base_matched <- base[match_idx, ]


target_matches <- base_matched |>
  mutate(
    ethnic = case_when(
      Nachname %in% turkish_last ~ "turkish",
      Nachname %in% polish_last ~ "polish",
      TRUE ~ "german"
    ),
    id_target = sprintf("T%04d", match_idx),
    Vorname = map2_chr(Vorname, ethnic, ~ {
      .x |> 
        add_title(.y) |>
        add_or_remove_middle(.y) %>%
        { if (runif(1) < 0.25) initialize_first(.) else . } %>%
        { if (runif(1) < 0.10) initial_middle(.) else . } %>% 
        typo()
    }),
    Nachname = map_chr(Nachname, ~ if (runif(1) < 0.15) swap_tokens(.x) else typo(.x)),
    Strasse = map_chr(Strasse, street_distort),
    Hausnummer = map_chr(Hausnummer, house_distort),
    Ort = map_chr(Ort, typo)
  ) |>
  select(-ethnic)


new_n <- round(0.20*n)
new_idx <- (n+1):(n+new_n)

target_new <- tibble(
  id_target = sprintf("T%04d", new_idx),
  
  ethnic = sample(c("german","turkish","polish"), new_n, TRUE, c(0.93, 0.04, 0.03)),
  
  Vorname = case_when(
    ethnic == "turkish" ~ sample(turkish_first, new_n, TRUE, turkish_first_w),
    ethnic == "polish" ~ sample(polish_first, new_n, TRUE, polish_first_w),
    TRUE ~ sample(german_first, new_n, TRUE, german_first_w)
  ),
  
  Nachname = case_when(
    ethnic == "turkish" ~ sample(turkish_last, new_n, TRUE, turkish_last_w),
    ethnic == "polish" ~ sample(polish_last, new_n, TRUE, polish_last_w),
    TRUE ~ sample(german_last, new_n, TRUE, german_last_w)
  ),
  
  Strasse = sample(streets, new_n, TRUE),
  Hausnummer = as.character(sample(1:150, new_n, TRUE)),
  Ort = sample(geo_map$Ort, new_n, TRUE)
) |>
  left_join(geo_map, by = "Ort") |>
  select(id_target, Vorname, Nachname, Strasse, Hausnummer, Ort, Kreis)



# ==============================================================================
# 8. Combine TARGET
# ==============================================================================

target_example <- bind_rows(target_matches, target_new) |>
  rename(actual_link = id_base)


# ==============================================================================
# 9. Duplicate 5% of the rows of the base table with small errors
# ==============================================================================


dup_idx <- sample(n, round(0.10 * n))

base_dups <- base[dup_idx, ] |>
  mutate(
    ethnic = case_when(
      Nachname %in% turkish_last ~ "turkish",
      Nachname %in% polish_last ~ "polish",
      TRUE ~ "german"
    ),
    id_base = sprintf("B%04d", (n + 1):(n + length(dup_idx))),
    Vorname = map2_chr(Vorname, ethnic, ~ {
      .x |> 
        vary_title() |>
        add_or_remove_middle(.y) %>%
        { if (runif(1) < 0.25) initialize_first(.) else . } %>%
        { if (runif(1) < 0.10) initial_middle(.) else . }
    }),
    ,
    Hausnummer = map_chr(Hausnummer, house_distort)
  ) |>
  select(-ethnic)

base_example <- bind_rows(base, base_dups)

# ==============================================================================
# 10. Save as Package Datasets
# ==============================================================================

usethis::use_data(base_example, overwrite = TRUE)
usethis::use_data(target_example, overwrite = TRUE)





