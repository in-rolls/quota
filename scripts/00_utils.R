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

# Proportion significant p-values in a list of models via tidy
proportion_significant_pvalues <- function(models) {

count_significant_pvalues <- function(model) {
     tidy_model <- tidy(model)
     tidy_model %>%
          filter(term != "(Intercept)" & p.value < 0.05) %>%
          nrow()
}

count_coefficients <- function(model) {
     tidy_model <- tidy(model)
     tidy_model %>%
          filter(term != "(Intercept)") %>%
          nrow()
}

total_significant_pvalues <- models %>%
     map_dbl(count_significant_pvalues) %>%
     sum()

total_coefficients <- models %>%
     map_dbl(count_coefficients) %>%
     sum()

proportion_significant <- total_significant_pvalues / total_coefficients
proportion_significant
}

## Custom Stargazer

library(stargazer)

# Custom stargazer function with common options
custom_stargazer <- function(models, ...) {
     stargazer(
          models,
          header = TRUE,
          type = "latex", 
          model.names = FALSE,
          
          omit.stat = c("rsq", "ser", "f"),
          digits = 2,
          column.sep.width = "0pt",
          
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          
          star.cutoffs = NULL,
          report = "vcs",

          no.space = TRUE,
          single.row = FALSE,
          notes.align = "l",
          font.size = "scriptsize",
          ...
     )
}
