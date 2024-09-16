
# Gen. Util. Functions
# Remove diacritics, convert to lowercase, convert to single ws, trim extra ws, remove punct
normalize_string <- function(input_string) {
     normalized_string <- stri_trans_general(input_string, "Latin-ASCII")
     normalized_string <- stri_trans_tolower(normalized_string)
     normalized_string <- gsub("\\s+", " ", normalized_string)
     normalized_string <- trimws(normalized_string)
     normalized_string <- gsub("[[:punct:]]", "", normalized_string)
     return(normalized_string)
}

# Remove NA columns
remove_na_columns <- function(df) {
     na_percentages <- df %>% summarise(across(everything(), ~sum(is.na(.)) / n() * 100))
     df <- df %>% select(where(~sum(is.na(.)) < nrow(df)))
     return(df)
}

# Add year suffix
add_year_suffix_remove_year_col <- function(df) {
     
     suffix <- paste0("_", unique(df$year))
     df <- df %>%
          rename_with(~paste0(.x, suffix), -c(key, year)) %>%
          select(-year)
     return(df)
}
