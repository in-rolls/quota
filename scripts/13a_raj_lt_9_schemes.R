# Load libs
library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stargazer)

# Source utils
source(here("scripts/00_utils.R"))

# Load dat
raj_elex_shrug <- read_parquet(here("data/raj/shrug_lgd_raj_elex_05_10.parquet"))
ay <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))

raj_elex_ay <- raj_elex_shrug %>% 
     inner_join(ay, by = "shrid2")

## Antyodya
## From Antyodya, they use data on 9 schemes: (Narasimhan/Weaver) -- seems like 8
# (i) BPL ration cards, which entitle the holders to purchase subsidized grains from government ration shops; 
# (ii) publicly provided health insurance through the Pradhan Mantri Jan Arogya Yojana; 
# (iii) pensions for the elderly, widows, and the disabled under the National Social Assistance Programme; 
# (iv) household electricity connections through the Saubhagya scheme (Pradhan Mantri Sahaj Bijli Har Ghar Yojana); 
# (v) receipt of a liquified petroleum gas (LPG) connection through the Pradhan Mantri Ujjwala scheme; 
# (vi) receipt of a Rs5,000 cash transfer for pregnant women and mothers through the Pradhan Mantri Matru Vandana Yojana (PMMVY) scheme; 
# (vii) receiving housing subsidies (or being on the waiting list for subsidies) through the Pradhan Mantri Awaas Yojana or state-specific schemes; 
# and (viii) having a ­ zero-balance bank account (Pradhan Mantri Jan Dhan initiative).

children_var <- list(
     "total_hhd_having_bpl_cards" = "BPL",
     "total_hhd_registered_under_pmjay" = "PMJAY",
     "total_hhd_availing_pension_under" = "NSAP",
     "total_hhd_availing_pmuy_benefits" = "PMUY",
     "total_hhd_having_pmsbhgy_benefit" = "Saubhagaya",
     "total_no_of_eligible_beneficiari" = "PMMVY",
     "total_hhd_have_got_pmay_house" = "PMAY",
     "total_hhd_availing_pmjdy_bank_ac" = "PMJD"
)

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),  # Keep shrid2-level variable
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(children_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
     )

# Model Names
model_names <- paste0("lm_", names(children_var))

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(names(children_var), names(children_var)) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Long-term Outcomes Concerning 8 Key Gov. Schemes",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(children_var)),
          add.lines = list(c("Covariates", rep("No", length(children_var)))),
          label = "raj_shrug_children_05_10",
          notes = c("BPL: HH with BPL Cards",
                    "PMJAY: HH registered under PM Jan Arogya Yojana",
                    "NSAP: HH with pensions under National Social Assistance Programme",
                    "PMUY: HH with LPG connection under PM Ujjwala Yojana",
                    "Saubhagaya: HH with electricity connection under Saubhayaga.",
                    "PMMVY: Number of beneficiaries under Rs 5,000 for pregnant women and mothers under PM Matru Vandana Yojana",
                    "PMAY: HH receiving subsidies under PM Awaas Yojana",
                    "PMJD: HH with zero-balance bank account under PM Jan Dhan"),
          out = here("tabs/shrug_raj_05_10_schemes.tex"))
