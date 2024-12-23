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

custom_stargazer <- function(models, notes, ..., out = NULL) {
     # Capture stargazer output without table environments
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE, # Prevent stargazer from wrapping its own table environment
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
               font.size = "scriptsize",
               ...
          )
     )
     
     # Remove any stargazer table environment manually
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}", stargazer_output)
     ]
     
     # Wrap the content in a proper LaTeX table environment
     wrapped_output <- paste0(
          "\\begin{table}[!htbp]\n",
          "\\centering\n",
          "\\begin{threeparttable}\n",
          paste(stargazer_output, collapse = "\n"),
          "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
          paste0("\\item ", notes, collapse = "\n"),
          "\n\\end{tablenotes}\n",
          "\\end{threeparttable}\n",
          "\\end{table}"
     )
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}

custom_stargazer <- function(models, notes, float_env = "table", ..., out = NULL) {
     # Capture stargazer output without table environments
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE, # Prevent stargazer from wrapping its own table environment
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
               font.size = "scriptsize",
               ...
          )
     )
     
     # Remove any LaTeX table environment from stargazer output
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\begin\\{sidewaystable\\}|\\\\end\\{sidewaystable\\}", stargazer_output)
     ]
     
     # Wrap the content in the specified float environment (default: table)
     wrapped_output <- paste0(
          "\\begin{", float_env, "}[!htbp]\n",
          "\\centering\n",
          "\\begin{threeparttable}\n",
          paste(stargazer_output, collapse = "\n"),
          "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
          paste0("\\item ", notes, collapse = "\n"),
          "\n\\end{tablenotes}\n",
          "\\end{threeparttable}\n",
          "\\end{", float_env, "}"
     )
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}

custom_stargazer <- function(models, notes, ..., out = NULL) {
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE, # Prevent stargazer from wrapping its own table environment
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
               font.size = "scriptsize",
               ...
          )
     )
     
     # Remove any LaTeX table environment from stargazer output
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\begin\\{sidewaystable\\}|\\\\end\\{sidewaystable\\}", stargazer_output)
     ]
     
     # Wrap the content in a proper LaTeX sidewaystable environment
     wrapped_output <- paste0(
          "\\begin{sidewaystable}[!htbp]\n",
          "\\centering\n",
          "\\begin{threeparttable}\n",
          paste(stargazer_output, collapse = "\n"),
          "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
          paste0("\\item ", notes, collapse = "\n"), # Add multiline notes
          "\n\\end{tablenotes}\n",
          "\\end{threeparttable}\n",
          "\\end{sidewaystable}"
     )
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}

custom_stargazer <- function(models, notes, float.env = "table", ..., out = NULL) {
     # Capture stargazer output without table environments
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE, # Prevent stargazer from wrapping its own table environment
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
               font.size = "scriptsize",
               ...
          )
     )
     
     # Remove any LaTeX table environment from stargazer output
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\begin\\{sidewaystable\\}|\\\\end\\{sidewaystable\\}", stargazer_output)
     ]
     
     # Wrap the content in the specified float environment
     wrapped_output <- paste0(
          "\\begin{", float_env, "}[!htbp]\n", # Dynamically set the float environment
          "\\centering\n",
          "\\begin{threeparttable}\n",
          paste(stargazer_output, collapse = "\n"),
          "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
          paste0("\\item ", notes, collapse = "\n"), # Add multiline notes
          "\n\\end{tablenotes}\n",
          "\\end{threeparttable}\n",
          "\\end{", float_env, "}"
     )
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}

custom_stargazer <- function(models, notes, float.env = "table", ..., out = NULL) {
     stargazer_output <- capture.output(
          stargazer(
               models,
               header = FALSE,
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
               font.size = "scriptsize",
               notes.append = FALSE,
               notes = "",
               notes.align = "l",
               ...
          )
     )
     
     stargazer_output <- gsub(
          pattern = "Constant\\s*&\\s*([-+0-9.]+)(\\**)?\\s*&",
          replacement = "Constant & \\2 &",
          x = stargazer_output
     )
     
     # Remove any existing table or sidewaystable environments
     stargazer_output <- stargazer_output[
          !grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\begin\\{sidewaystable\\}|\\\\end\\{sidewaystable\\}", stargazer_output)
     ]
     
     # Generate the output for `table`
     if (float.env == "table") {
          wrapped_output <- paste0(
               "\\begin{table}[!htbp]\n",
               "\\centering\n",
               "\\begin{threeparttable}\n",
               paste(stargazer_output, collapse = "\n"),
               "\n\\begin{tablenotes}[flushleft]\n\\scriptsize\n",
               paste0("\\item ", notes, collapse = "\n"),
               "\n\\end{tablenotes}\n",
               "\\end{threeparttable}\n",
               "\\end{table}"
          )
     }
     # Generate the output for `sidewaystable`
     else if (float.env == "sidewaystable") {
          wrapped_output <- paste0(
               "\\begin{sidewaystable}[!htbp]\n",
               "\\centering\n",
               "\\begin{threeparttable}\n",
               paste(stargazer_output, collapse = "\n"),
               "\n\\begin{tablenotes}[flushleft]\n\\setlength{\\itemindent}{0em}\n\\scriptsize\n",
               paste0("\\item ", notes, collapse = "\n"), # Add custom multiline notes
               "\n\\end{tablenotes}\n",
               "\\end{threeparttable}\n",
               "\\end{sidewaystable}"
          )
     } else {
          stop("Invalid float_env. Use 'table' or 'sidewaystable'.")
     }
     
     # Write to file or print to console
     if (!is.null(out)) {
          writeLines(wrapped_output, con = out)
     } else {
          cat(wrapped_output, sep = "\n")
     }
}
