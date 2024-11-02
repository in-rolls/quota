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
mnrega_elex_raj_05_10 <- read_parquet(here("data/raj/mnrega_elex_raj_05_10.parquet"))

# Areas
# "childcare", "infrastructure", 
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))

mnrega_elex_raj_05_10 <- mnrega_elex_raj_05_10 %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_raj_05_10, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  })
     )

# Main
# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

# Actual 
main_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005")), data = mnrega_elex_raj_05_10))

# Define the number of rows to sample and the number of iterations
sample_size <- nrow(mnrega_elex_raj_05_10)
num_iterations <- 1000

# Initialize an empty data frame to store results
results <- data.frame(
     beta_y1 = numeric(),
     tstat_y1 = numeric(),
     beta_y2 = numeric(),
     tstat_y2 = numeric()
)

for (i in 1:num_iterations) {
     stratified_sample <- mnrega_elex_raj_05_10 %>%
          group_by(female_res_2005) %>%
          sample_n(min(ifelse(female_res_2005 == 1, round(sample_size * 1/3), round(sample_size * 2/3)), n()))
     
     # Male
     model_y1 <- lm(connectivity_comp_project_tot_11_14 ~ female_res_2005, data = stratified_sample)
     beta_y1 <- coef(summary(model_y1))[2, 1]
     tstat_y1 <- coef(summary(model_y1))[2, 3]
     
     # Female
     model_y2 <- lm(sanitation_comp_project_tot_11_14 ~ female_res_2005, data = stratified_sample)
     beta_y2 <- coef(summary(model_y2))[2, 1]
     tstat_y2 <- coef(summary(model_y2))[2, 3]
     
     results <- rbind(results, data.frame(
          iteration = i,
          beta_y1 = beta_y1,
          tstat_y1 = tstat_y1,
          beta_y2 = beta_y2,
          tstat_y2 = tstat_y2
     ))
}

print(results)
results$diff_beta <- results$beta_y1 - results$beta_y2
results$diff_t <- results$tstat_y1 - results$tstat_y2

p1 <- ggplot(results, aes(x = beta_y1)) + 
     geom_density(fill = "blue", alpha = 0.5) +
     geom_density(aes(x = beta_y2), fill = "red", alpha = 0.5) +
     ggtitle("Density of Beta1 and Beta2") +
     theme_minimal()

p2 <- ggplot(results, aes(x = diff_t)) +
     geom_density(fill = "purple", alpha = 0.5) +
     ggtitle("Density of the Difference of Betas") +
     theme_minimal()

# Print the plots
print(p1)
print(p2)
sd(results$diff_t)

# Standard deviation of the purple
# results$diff_t
# Minimal detect effect: 2.49*1.44
# MDE for t-stat for male preferred outcome/women 
     ## 2011--2014 ~ 2010
