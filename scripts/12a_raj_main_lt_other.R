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
ay   <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))
secc <- read_csv(here("data/shrug/shrug-secc-mord-rural-csv/secc_rural_shrid.csv"))

raj_elex_ay <- raj_elex_shrug %>% 
     inner_join(ay, by = "shrid2") %>%
     inner_join(secc, by = "shrid2")

# s.e. and beta are weird--- we don't see any sig. effects but for now don't feel comfortable adding these
# #"two_crop_acre_sum" = "Irrigation" #"Irrigation: Total land with assured irrigation for two crops" 

# interactions

other_var <- list(
     "total_female_child_age_bw_0_6" = "Female Children",
     "total_hhd_having_bpl_cards" = "BPL Cards",
     "total_no_of_eligible_beneficiari" = "Maternity Benefit",
     "wall_mat_solid_share" = "Solid Wall",
     "roof_mat_solid_share" = "Solid Roof"
     )

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(other_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
          )

# Model Names
model_names <- paste0("lm_", names(other_var))
models <- set_names(names(other_var), names(other_var)) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Other Short- and Long-term Outcomes",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(other_var)),
          add.lines = list(c("Covariates", rep("No", length(other_var)))),
          label = "raj_shrug_other_05_10",
          notes = paste(cons_term, "The outcomes are from the Mission Antyodya Survey from 2019. 
                   The village level outcomes have been aggregated to a GP level. The outcomes are: 
                     (i) Female Children: The number of female children (0-6 years);
                     (ii) BPL Cards: The number of households with a BPL ration card;
                     (iii) Maternity Benefit: The number of eligible beneficiaries under Pradhan Mantri Matru Vandana Yojana (PMMVY)
                     (iv) Solid Wall: Share of households with solid wall - wood/stone/sheets/burnt brick/concrete;
                     (v) Solid Roof: Share of households with solid roof - stone/slate sheets/concrete burned brick."),
          out = "tabs/shrug_raj_05_10_other.tex")
