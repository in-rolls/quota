# Load libs
library(here)
library(arrow)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(estimatr)
library(kableExtra)
library(nnet)

# Robustness
# Pre-treatment covariates can be added


# Balance for 2020 data: 
# GPs that were reserved throughout and the that were never reserved
# Using the same 2001 Census covariates listed above to predict whether a GP is classified as always-reserved or never-reserved yields a non-significant 
# F-statistic (XXX), which is what one would expect given random assignment.


# Load dat
up_elex_shrug <- read_parquet(here("data/up/shrug_lgd_up_elex_05_10.parquet"))
vd_01 <- read_csv(here("data/shrug/shrug-vd01-csv/pc01_vd_clean_shrid.csv"))

up_elex_vd_01 <- up_elex_shrug %>% 
     inner_join(vd_01, by = "shrid2")

# Notes
# Literacy and children < 6 are from the census abstract
#"pc01_pca_p_06" = "Total children < 6 (Female)",
#"pc01_pca_m_lit" = "Total literate (Male)",
#"pc01_pca_f_lit" = "Total literate (Female)",

balance_vars <- list(
     "pc01_vd_t_p" = "Total population",
     "pc01_vd_t_f" = "Total population (Female)",
     "pc01_vd_sc_f" = "Total SC (Female)",
     "pc01_vd_st_f" = "Total ST (Female)",
     "pc01_vd_medi_fac" = "Number of medical facilities",
     "pc01_vd_m_home" = "Number of maternity homes",
     "pc01_vd_mcw_cntr" = "Number Of Family Welfare Centres",
     "pc01_vd_p_sch" = "Number of Primary Schools",
     "pc01_vd_m_sch" = "Number of Middle Schools",
     "pc01_vd_handpump" = "Handpumps",
     "pc01_vd_tap" = "Tap water",
     "pc01_vd_well" = "Wells",
     "pc01_vd_bank_fac" = "Banking facility",
     "pc01_vd_power_supl" = "Power supply",
     "pc01_vd_app_mr" = "Approach - mud road"
     #"pc01_vd_app_pr" = "Approach - paved road"
)

up_elex_vd_01 <- up_elex_vd_01 %>%
     mutate(treat = paste(female_res_2005, female_res_2010, sep = "-"),
            treat = recode(treat,
                           `0-0` = "NF-NF",
                           `0-1` = "NF-F",
                           `1-0` = "F-NF",
                           `1-1` = "F-F"),
            cluster_id = paste(district_name_eng_2005, block_name_eng_2005, gp_name_eng_2005, sep = "-"))  

# GP Level
up_elex_vd_01_gp <- up_elex_vd_01 %>%
     group_by(cluster_id, treat) %>%
     summarise(across(all_of(names(balance_vars)), ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
     ungroup()

calculate_f_stat_feols <- function(var, df) {
     model <- lm_robust(as.formula(paste(var, "~ treat")), 
                        data = df)
     f_stat <- summary(model)$fstatistic["value"]
     numdf <- summary(model)$fstatistic["numdf"]
     dendf <- summary(model)$fstatistic["dendf"]
     
     p_value <- pf(f_stat, df1 = numdf, df2 = dendf, lower.tail = FALSE)
     return(p_value)
}

calculate_ri_p_value <- function(var, df, n_permutations = 1000) {
     original_model <- lm_robust(as.formula(paste(var, "~ treat")), data = df)
     observed_f_stat <- summary(original_model)$fstatistic["value"]
     
     permuted_f_stats <- numeric(n_permutations)
     
     for (i in 1:n_permutations) {
          df$shuffled_treat <- sample(df$treat)
          
          permuted_model <- lm_robust(as.formula(paste(var, "~ shuffled_treat")), data = df)
          
          permuted_f_stats[i] <- summary(permuted_model)$fstatistic["value"]
     }
     
     p_value <- mean(permuted_f_stats >= observed_f_stat, na.rm = TRUE)
     
     return(p_value)
}

f_stats <- map_dbl(names(balance_vars), ~ calculate_ri_p_value(.x, up_elex_vd_01_gp))

balance_wide <- up_elex_vd_01_gp %>%
     group_by(treat) %>%
     summarise(across(all_of(names(balance_vars)), ~ round(mean(., na.rm = TRUE), 2)),
               N = n())

balance_long <- balance_wide %>%
     pivot_longer(cols = -treat, names_to = "Variable", values_to = "Value") %>%
     pivot_wider(names_from = treat, values_from = Value) %>%
     mutate(f_stat = c(round(f_stats, 2), "")) %>%
     mutate(Variable = recode(Variable, !!!balance_vars))

# Omnibus F-stat
formula <- as.formula(paste("treat ~", paste(names(balance_vars), collapse = " + ")))
full_model <- multinom(formula, data = up_elex_vd_01_gp)
null_model <- multinom(treat ~ 1, data = up_elex_vd_01_gp)
dev_diff <- null_model$deviance - full_model$deviance
df_diff <- full_model$edf - null_model$edf
p_value <- 1 - pchisq(dev_diff, df_diff)

bal_table <- balance_long %>%
     mutate(across(where(is.numeric), ~ ifelse(Variable == "N", formatC(., format = "f", digits = 0), .))) %>%
     kable("latex", 
           booktabs = TRUE, 
           col.names = c("Variable", "T-T", "T-C", "C-T", "C-C", "\\textit{p}(F-stat.)"), 
           caption = "Summary statistics and p-value of the F-statistics for variables.", 
           label = "balance_table_up",
           escape = FALSE,
           align = c("l", "r", "r", "r", "r", "r")) %>%
     kable_styling(latex_options = c("hold_position", "scale_down")) %>%
     add_header_above(c(" " = 1, "Means by Group" = 4, " " = 1)) %>%
     row_spec(0, bold = TRUE) %>%
     footnote(general = sprintf("T denotes GPs that were reserved for women and C denotes other GPs. All the covariates were taken from the 2001 Census Village Directory.
            N indicates the number of Gram Panchayats. The p-value of the F-statistic is derived from regressions using randomization inference. 
            We also fit a null model and a multinomial model that used all the above covariates to predict the assigned reservation sequence, e.g., T-T, T-C, etc. 
            The p-value of the Likelihood Ratio test is %.2f, which suggests that, consistent with random assignment, the complete multinomial model, fits
            no better than a null model with no predictors.", p_value),
              escape = FALSE,
            threeparttable = TRUE)

save_kable(bal_table, file = here("tabs/balance_table_up.tex"))
