# Load libs.
library(readr)
library(arrow)
library(stargazer)
library(tidyverse)
library(stringi)
library(kableExtra)
library(here)

# Load utils
source(here("scripts/00_utils.R"))

# Let's use the data joined to MNREGA
mnrega_up_05_10 <- read_parquet(here("data/up/mnrega_elex_up_05_10.parquet"))

# Load inner
up_05_10 <- read_parquet("data/up/up_05_10_inner.parquet")
up_all <- read_parquet("data/up/up_all_inner.parquet")

# Load fuzzy
up_05_10_ff <- read_parquet("data/up/up_05_10_fuzzy.parquet")
up_10_15_ff <- read_parquet("data/up/up_10_15_fuzzy.parquet")

# District

up_dist_wise_transition <- up_05_10_ff %>%
     group_by(district_name_eng_2010) %>%
     summarise(
          prop_1 = mean(female_res_2005 == 1),
          prop_11 = mean(female_res_2005 == 1 & female_res_2010 == 1),
          prop_01 = mean(female_res_2005 == 0 & female_res_2010 == 1),
          prop_10 = mean(female_res_2005 == 1 & female_res_2010 == 0),
          prop_00 = mean(female_res_2005 == 0 & female_res_2010 == 0),
          size = n()
     ) %>%
     arrange(desc(prop_10))
print(up_dist_wise_transition, n = 100)
write_csv(up_dist_wise_transition, file = here("data/up/up_dist_wise_05_10transition.csv"))

# Random or not
#------------------
with(up_05_10,    summary(lm(female_res_2010 ~ female_res_2005)))
with(up_05_10[up_05_10$phase_1_2005 == 1, ],    summary(lm(female_res_2010 ~ female_res_2005)))
with(up_05_10[up_05_10$phase_2_2005 == 1, ],    summary(lm(female_res_2010 ~ female_res_2005)))

with(up_05_10_ff, summary(lm(female_res_2010 ~ female_res_2005)))
# ~ same ans.
#with(up_05_10_ff, summary(lm(female_res_2010 ~ female_res_2005 + 
#                                  I(paste0(district_name_2005,  block_code_2005)))))

with(up_all, summary(lm(female_res_2010 ~ female_res_2005)))
with(up_all, summary(lm(female_res_2015 ~ female_res_2010)))

#with(up_10_15, summary(lm(female_res_2015 ~ female_res_2010)))
#with(up_10_15_ff, summary(lm(female_res_2015 ~ female_res_2010)))
#with(up_15_21, summary(lm(female_res_2021 ~ female_res_2015)))

## Run within district regressions and calculate sig. p-values

get_p_value <- function(df) {
     model <- lm(female_res_2010 ~ female_res_2005, data = df)
     p_value <- summary(model)$coefficients["female_res_2005TRUE", "Pr(>|t|)"]
     return(p_value)
}

p_values <- mnrega_up_05_10 %>%
     group_by(district_name_eng_2010) %>%
     summarize(p_value = get_p_value(pick(everything()))) %>%
     ungroup()

count_below_threshold <- p_values %>%
     summarize(count = sum(p_value < 0.05))

count_below_threshold/nrow(p_values)

# Output Table
rand_or_not    <- with(mnrega_up_05_10, lm(female_res_2010 ~ female_res_2005))
rand_or_not_fe <- with(mnrega_up_05_10, lm(female_res_2010 ~ female_res_2005 + as.factor(district_name_eng_2010)))
rand_or_not_p1 <- with(mnrega_up_05_10[mnrega_up_05_10$phase_1_2005 == 1 | mnrega_up_05_10$phase_2_2005 == 1, ], lm(female_res_2010 ~ female_res_2005))

source(here("scripts/00_utils.R"))

custom_stargazer(list(rand_or_not, rand_or_not_fe, rand_or_not_p1),
          title = "Predicting 2010 GP Reservation Status Using the 2005 Reservation Status in UP.",
          covariate.labels = c("2005", "Constant"),
          column.labels = c("All", "With District FE", "Phase 1 or 2"),
          label = "rand_or_no_up",
          digits = 2,
          notes = "Statistical significance symbols for the constant terms are suppressed.
                   Phase 1 and 2 limits regression to districts covered in the first two phases of MNREGA.
                   Within district regressions yield 12 significant coefficients, considerably more than what we would expect by chance.",
          omit = "as.factor",
          out = "tabs/rand_or_not_2010_on_2005_up.tex")
