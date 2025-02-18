# Load libs
library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stargazer)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(pbapply)
library(janitor)
library(stringi)

# Source utils
source(here("scripts/00_utils.R"))

# Read dat
mnrega_elex_raj_05_20 <- read_parquet(here("data/raj/mnrega_elex_raj_05_20.parquet")) %>% 
     dplyr::rename(match_name = match_name.x)

# Social Audit Read and transform
mnrega_raj_sa <- read_csv(here("data/mnrega/final_audit_results_translit.csv"))

mnrega_raj_sa <- mnrega_raj_sa %>%
     filter(state == 'RAJASTHAN') %>%
     clean_names() %>%
     filter(!is.na(panchayat)) %>%
     mutate(
          diff_works = (total_works - works_verified == 0),
          diff_households = (total_households - households_verified == 0),
          job_cards_with_people_numeric = case_when(
               job_cards_with_people == "Less than 25%"      ~ 0,
               job_cards_with_people == "Between 25% and 50%"  ~ .33,
               job_cards_with_people == "Between 50% and 75%"  ~ .66,
               job_cards_with_people == "Greater than 75%"     ~ 1,
               TRUE                                          ~ NA_real_
          ),
          adequate_manpower = case_when(
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Less than 25%"      ~ 0,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Between 25% and 50%"  ~ .33,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Between 50% and 75%"  ~ .66,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Greater than 75%"     ~ 1,
               TRUE                                          ~ NA_real_
          ),
          rozgar_diwas = case_when(
               gpc_mgnregs_administration_is_rozgar_diwas_was_held_every_month == "Yes" ~ 1,
               gpc_mgnregs_administration_is_rozgar_diwas_was_held_every_month == "No" ~ 0,
               TRUE ~ NA_real_
          ),
          match_name = normalize_string(gsub(" ", "", paste0(district, block, plain_transliteration)))
     ) %>%
     group_by(match_name) %>%
     summarise(
          diff_works                   = mean(diff_works, na.rm = TRUE),
          diff_households              = mean(diff_households, na.rm = TRUE),
          job_cards_with_people_numeric = mean(job_cards_with_people_numeric, na.rm = TRUE),
          adequate_manpower            = mean(adequate_manpower, na.rm = TRUE),
          rozgar_diwas                 = mean(rozgar_diwas, na.rm = TRUE),
          .groups                           = "drop"
     )

# Add Main. Cols to MNREGA
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))

mnrega_elex_raj_05_20 <- mnrega_elex_raj_05_20 %>%
     mutate(
          # Create columns for the 2011 to 2014 range
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern_11_14 <- paste0(.x, "_201[1-4]$")
                       new_col_11_14 <- rowSums(select(mnrega_elex_raj_05_20, matches(col_pattern_11_14)), na.rm = TRUE)
                       return(new_col_11_14)
                  })
     )

# Join the two
mnrega_raj_sa_elex <- mnrega_elex_raj_05_20 %>% 
     select(female_res_2005, paste0(column_groups, "_tot_11_14"), female_res_2010, female_res_2015, female_res_2020, match_name) %>%
     inner_join(mnrega_raj_sa, by = c("match_name" = "match_name"))

# Run mods
dependent_vars <- c("diff_works", "diff_households", "job_cards_with_people_numeric",
                    "adequate_manpower", "rozgar_diwas")

models <- map(dependent_vars, function(dv) {
     lm(as.formula(paste0(dv, " ~ female_res_2005 + female_res_2010 + female_res_2015 + female_res_2020")),
           data = mnrega_raj_sa_elex)
})

# Covar. label mapper
label_mapping <- c(
     "female_res_2005" = "2005",
     "female_res_2010" = "2010",
     "female_res_2015" = "2015",
     "female_res_2020" = "2020",
     "(Intercept)" = "Constant"
     
)

coef_names <- names(coef(models[[1]]))
cov_labels <- label_mapping[coef_names]

# Write
modelsummary(
     models,
     coef_map = label_mapping,
     title = "Effects of Reservations on the Audit Performance.",
     label = "main_mnrega_audit",
     gof_omit  = "AIC|BIC|RMSE|^R2$",
     notes = paste(cons_term, "The outcomes are from MNREGA social audits from 2020--2024.
               All outcomes have been coded to lie between 0 and 1 with 1 reflecting greater 
               adherence to procedures or lower discrepancy between the audit and reported amounts.
               Works: Are validated number of works the same as reported?;
               Households: Are validated number of households the same as reported?;
               Job Cards: What percentage of jobs cards are with people?;
               Adq. Manpower: Is there adequate manpower to implement MNREGA?;
               Rozgar Diwas: Is Rozgar Diwas organized every month?"
     ),
     notes_append  = TRUE,
     notes_align   = "l",
     keepxp = FALSE,
     stars = FALSE,
     fmt = 2,
     output = here("tabs/mnrega_raj_05_20_audit.tex")
)

custom_stargazer(models,
                 title = "Effects of Reservations on the Audit Performance.",
                 covariate.labels = c("2005", "2010", "2015", "2020", "Constant"),
                 column.labels = c("Works", "Households", "Job Cards", "Adq. Manpower", "Rozgar Diwas"),
                 label = "main_mnrega_audit_res",
                 notes = paste(cons_term, "The outcomes are from MNREGA social audits from 2020--2024.
                                        All outcomes have been coded to lie between 0 and 1 with 1 reflecting greater 
                                        adherence to procedures or lower discrepancy between the audit and reported amounts.
                                        Works: Are validated number of works the same as reported?;
                                        Households: Are validated number of households the same as reported?;
                                        Job Cards: What percentage of jobs cards are with people?;
                                        Adq. Manpower: Is there adequate manpower to implement MNREGA?;
                                        Rozgar Diwas: Is Rozgar Diwas organized every month?"
                              ),
                 out = here("tabs/mnrega_raj_05_20_audit.tex"))

## Subsetting to passed audit GPs

bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

# Actual 
main_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), 
              data = mnrega_raj_sa_elex[mnrega_raj_sa_elex$diff_works > .31, ]))

# Print
map(main_models, ~ tidy(.x) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
map(main_models, glance)
proportion_significant_pvalues(main_models)

# Number of completed projects
n_comp_proj <- paste0(
     c("total", "connectivity", "childcare", "sanitation", "water_conserve", "water_trad"), 
     "_comp_project_tot_11_14"
)

n_comp_main_models <- main_models[names(main_models) %in% n_comp_proj]

# Main
main_outcome_caption <- "They are: 
(i) All: The total number of completed projects, including areas not listed here like Fisheries, Drought Proofing, etc.;
(ii) Rural Roads: The number of projects to improve connectivity and roads;
(iii) Sanitation: The number of projects to improve sanitation facilities;
(iv) Water Conservation: The number of projects to improve water conservation;
(v) Trad. Water: The number of projects to maintain traditional water bodies."

custom_stargazer(n_comp_main_models,
                 title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011--2014 Among Those GPs With Above Average Performance on Audit",
                 covariate.labels = c("2005", "2010", "Constant"),
                 column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Trad. Water"),
                 add.lines = list(c("Covariates", rep("No", 5))),
                 label = "main_mnrega_audit",
                 notes = paste(cons_term, 
                               "The outcomes are from MNREGA administrative data for years 2011--2014.", 
                               main_outcome_caption),
                 out = here("tabs/mnrega_audit_raj_05_10_main.tex"))

