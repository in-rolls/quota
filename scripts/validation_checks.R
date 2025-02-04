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
## Will use raj_elex_ay for the rest of the analysis and renaming for ease - 

df = raj_elex_ay

df$category_2010 = ifelse(df$category_2010 == "GENW", "GEN",df$category_2010 )
df$category_2010 = ifelse(df$category_2010 == "OBCW", "OBC",df$category_2010 )
df$category_2010 = ifelse(df$category_2010 == "SCW", "SC",df$category_2010 )
df$category_2010 = ifelse(df$category_2010 == "STW", "ST",df$category_2010 )

df$category_label = paste(df$category_2005, df$category_2010, sep = "_")
table(df$category_label)

hist(df$piped_water_fully_covered)
df = df[df$category_label !="OBC_NA",]

lm1 = lm(piped_water_fully_covered~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                  (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(piped_water_fully_covered~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                  (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)

################################################################################################################################

###############################################################################################
## Lets create a composite index of poverty - ## using variables that have total number of HH

# First standardize all variables to 0-1 scale
# Note: Some variables need to be reversed as higher values indicate worse conditions


safe_ratio <- function(numerator, denominator) {
          ifelse(denominator == 0 | is.na(denominator) | is.na(numerator), 
                 0,  
                 pmin(numerator/denominator, 1)) } # cap the ratio at 1}
     
# Helper function to replace Inf values
     clean_infinite <- function(x) {
          x[is.infinite(x)] <- 0
          x[is.na(x)] <- 0
          pmin(pmax(x, 0), 1)  # constrain values between 0 and 1
     }
     
# Create standardized positive indicators (higher is better)
     df$std_clean_energy <- clean_infinite(safe_ratio(df$total_hhd_with_clean_energy, df$total_hhd))
     df$std_water <- clean_infinite(safe_ratio(df$total_hhd_having_piped_water_con, df$total_hhd))
     df$std_bank <- clean_infinite(safe_ratio(df$total_hhd_availing_pmjdy_bank_ac, df$total_hhd))
     df$std_housing <- clean_infinite(safe_ratio(df$total_hhd_have_got_pmay_house, df$total_hhd))
    
     df$std_health <- clean_infinite(safe_ratio(df$total_hhd_registered_under_pmjay, df$total_hhd))
     df$std_pension <- clean_infinite(safe_ratio(df$total_hhd_availing_pension_under, df$total_hhd))
    
     df$food_grain <- clean_infinite(safe_ratio(df$gp_total_hhd_receiving_food_grai, df$total_hhd))
     df$food_grain_nfsa <- clean_infinite(safe_ratio(df$gp_total_hhd_eligible_under_nfsa, df$total_hhd))
     df$farm_activities <- clean_infinite(safe_ratio(df$total_hhd_engaged_in_farm_activi, df$total_hhd))
     df$forest_prod <- clean_infinite(safe_ratio(df$total_hhd_source_of_minor_forest, df$total_hhd))
     
# Create standardized negative indicators (higher is worse, so subtract from 1)
     df$std_bpl <- clean_infinite(1 - safe_ratio(df$total_hhd_having_bpl_cards, df$total_hhd))
     df$std_kuccha <- clean_infinite(1 - safe_ratio(df$total_hhd_with_kuccha_wall_kucch, df$total_hhd))
     df$std_sanitation <- clean_infinite(1 - safe_ratio(df$total_hhd_not_having_sanitary_la, df$total_hhd))
     
# Create matrix of indicators and check for any remaining issues
     indicator_matrix <- cbind(
          df$std_clean_energy,
          df$std_water,
          df$std_bank,
          df$std_housing,
          df$std_health,
          df$std_pension,
          df$food_grain,
          df$food_grain_nfsa,
          df$farm_activities,
          df$forest_prod,
          df$std_bpl,
          df$std_kuccha,
          df$std_sanitation
     )
     
# Final cleanup of the matrix
     indicator_matrix[is.infinite(indicator_matrix)] <- 0
     indicator_matrix[is.na(indicator_matrix)] <- 0
     indicator_matrix <- pmin(pmax(indicator_matrix, 0), 1)
     
# Calculate final index
     df$wellbeing_index <- rowMeans(indicator_matrix, na.rm = TRUE)
     


# Use the function
# Check results
summary(df$wellbeing_score)
hist(df$wellbeing_score, 
     main="Distribution of Wellbeing Index",
     xlab="Index Score",
     breaks=20)

df$composite_index = index(df)
lm1 = lm(composite_index~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                       (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)

components <- c(
     "std_clean_energy" = "LPG/BioGasAccess",
     "std_water" = "Piped Water Access",
     "std_bank" = "Dhan Jan Bank Account",
     "std_housing" = "Housing Assist Received ",
     "std_kuccha" = "Housing Quality",
     "std_health" = "Health Coverage",
     "std_pension" = "Get Pension Assist",
     "std_sanitary" = "No Sanitary Facility",
     "food_grain" = "Get Food Assist",
     "food_grain_nfsa" = "NFSA Eligibility",
     "farm_activities" = "Only Farm Activities",
     "forest_prod" = "Forest Subsist",
     "std_bpl" = "Below Poverty Card"
)

# Initialize results storage
results <- data.frame(
     Component = character(),
     Coefficient = numeric(),
     StdError = numeric(),
     PValue = numeric(),
     stringsAsFactors = FALSE
)

# Run individual regressions for each component
for(var in names(components)) {
     # Create formula
     formula <- as.formula(paste(var, "~category_label"))
     
     # Run regression
     lm_result <- lm(formula,  data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                    (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
     
     # Extract coefficients (we want the 4th coeff because I am interested in STST comparison, taking second coefficient)
     coef_data <- summary(lm_result)$coefficients
     if(nrow(coef_data) > 1) {  # Check if we have category coefficient
          results <- rbind(results, data.frame(
               Component = components[var],
               Coefficient = coef_data[4, 1],  # Coefficient
               StdError = coef_data[2, 2],     # Standard Error
               PValue = coef_data[2, 4]        # P-value
          ))
     }
}

# Add significance stars
results$Significance <- ifelse(results$PValue < 0.001, "***",
                               ifelse(results$PValue < 0.01, "**",
                                      ifelse(results$PValue < 0.05, "*", "")))

# Sort by absolute value of coefficient
results$AbsCoef <- abs(results$Coefficient)
results <- results[order(results$AbsCoef, decreasing = TRUE), ]

# Create visualization
library(ggplot2)

ggplot(results, aes(x = reorder(Component, Coefficient), y = Coefficient)) +
     geom_bar(stat = "identity", 
              fill = ifelse(results$Coefficient > 0, "steelblue", "indianred")) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
     coord_flip() +
     theme_minimal() +
     labs(
          title = "Component-wise Effects of Category on Household Outcomes",
          subtitle = "Regression Coefficients with Standard Errors",
          x = "",
          y = "Coefficient Estimate"
     ) +
     geom_errorbar(aes(ymin = Coefficient - StdError, 
                       ymax = Coefficient + StdError),
                   width = 0.2) +
     geom_text(aes(label = Significance, 
                   y = ifelse(Coefficient > 0, 
                              Coefficient + StdError, 
                              Coefficient - StdError)),
               vjust = ifelse(results$Coefficient > 0, -0.5, 1.5)) +
     theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10)
     )

##############################################################################################
##############################################################################################
## Now focusiing on outcomes related to wellbeing of women and children 
# 1. Child Health Indicators
# Immunization rate
df$health_1 <- clean_infinite(safe_ratio(df$total_childs_aged_0_to_3_years_i,
                                         df$total_childs_aged_0_to_3_years))

df$total_children_0_6 <- df$total_male_child_age_bw_0_6 + df$total_female_child_age_bw_0_6

# Non-underweight rate
df$health_2 <- clean_infinite(1 - safe_ratio(df$total_underweight_child_age_unde,
                                             df$total_children_0_6))

# Non-stunting rate
df$health_3 <- clean_infinite(safe_ratio(df$total_childs_categorized_non_stu,
                                         df$total_children_0_6))

# Non-anemic rate
df$health_4 <- clean_infinite(1 - safe_ratio(df$total_no_of_young_anemic_childre,
                                             df$total_children_0_6))

# 2. Education Access Indicators
# School attendance rate
df$education_1 <- clean_infinite(1 - safe_ratio(df$no_of_children_not_attending_sch,
                                                df$total_population))

# Scholarship coverage
df$education_2 <- clean_infinite(safe_ratio(df$total_minority_children_getting_,
                                            df$total_population))

# 3. Early Childhood Care
# Anganwadi enrollment rates
df$care_1 <- clean_infinite(safe_ratio(df$total_childs_aged_0_to_3_years_r,
                                       df$total_childs_aged_0_to_3_years))

df$care_2 <- clean_infinite(safe_ratio(df$total_childs_aged_3_to_6_years_r,
                                       pmax(df$total_children_0_6 - df$total_childs_aged_0_to_3_years, 1)))
# First, let's create individual regressions for each component
# Filter the data as in your original regression
filtered_df <- df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                       (df$category_2010 == "GEN"|df$category_2010 == "ST"), ]

# Run regressions for each component
lm_health1 <- lm(health_1 ~ category_label, data = filtered_df)  # Registered
lm_health2 <- lm(health_2 ~ category_label, data = filtered_df)  # Non-underweight
lm_health3 <- lm(health_3 ~ category_label, data = filtered_df)  # Non-stunting
lm_health4 <- lm(health_4 ~ category_label, data = filtered_df)  # Non-anemic

lm_edu1 <- lm(education_1 ~ category_label, data = filtered_df)  # School attendance
lm_edu2 <- lm(education_2 ~ category_label, data = filtered_df)  # Scholarship

lm_care1 <- lm(care_1 ~ category_label, data = filtered_df)  # Anganwadi 0-3
lm_care2 <- lm(care_2 ~ category_label, data = filtered_df)  # Anganwadi 3-6

# Create a summary of all coefficients
results <- data.frame(
     Component = c("Immunization Rate", "Non-underweight Rate", "Non-stunting Rate", 
                   "Non-anemic Rate", "School Attendance", "Scholarship SC/ST",
                   "Anganwadi (0-3)", "Anganwadi (3-6)"),
     Coefficient = c(coef(lm_health1)[2], coef(lm_health2)[2], coef(lm_health3)[2],
                     coef(lm_health4)[2], coef(lm_edu1)[2], coef(lm_edu2)[2],
                     coef(lm_care1)[2], coef(lm_care2)[2]),
     StdError = c(summary(lm_health1)$coefficients[2,2],
                  summary(lm_health2)$coefficients[2,2],
                  summary(lm_health3)$coefficients[2,2],
                  summary(lm_health4)$coefficients[2,2],
                  summary(lm_edu1)$coefficients[2,2],
                  summary(lm_edu2)$coefficients[2,2],
                  summary(lm_care1)$coefficients[2,2],
                  summary(lm_care2)$coefficients[2,2]),
     PValue = c(summary(lm_health1)$coefficients[2,4],
                summary(lm_health2)$coefficients[2,4],
                summary(lm_health3)$coefficients[2,4],
                summary(lm_health4)$coefficients[2,4],
                summary(lm_edu1)$coefficients[2,4],
                summary(lm_edu2)$coefficients[2,4],
                summary(lm_care1)$coefficients[2,4],
                summary(lm_care2)$coefficients[2,4])
)

# Add significance stars
results$Significance <- ifelse(results$PValue < 0.001, "***",
                               ifelse(results$PValue < 0.01, "**",
                                      ifelse(results$PValue < 0.05, "*", "")))

# Sort by absolute value of coefficient to see biggest contributors
results$AbsCoef <- abs(results$Coefficient)
results <- results[order(results$AbsCoef, decreasing = TRUE), ]


# Create the plot
ggplot(results, aes(x = reorder(Component, Coefficient), y = Coefficient)) +
     geom_bar(stat = "identity", fill = ifelse(results$Coefficient > 0, "steelblue", "indianred")) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
     coord_flip() + # This flips the coordinates to make horizontal bars
     theme_minimal() +
     labs(
          title = "Component-wise Effects of Category on Children's Outcomes",
          x = "",  # Remove x axis label since it's self-explanatory
          y = "Coefficient Estimate"
     ) +
     # Add error bars
     geom_errorbar(aes(ymin = Coefficient - StdError, 
                       ymax = Coefficient + StdError),
                   width = 0.2) +
     # Add significance stars
     geom_text(aes(label = Significance, 
                   y = ifelse(Coefficient > 0, 
                              Coefficient + StdError, 
                              Coefficient - StdError)),
               vjust = ifelse(results$Coefficient > 0, -0.5, 1.5)) +
     theme(
          plot.title = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10)
     )

##############################################################################################
## Some raw regressions
##############################################################################################

lm1 = lm(availability_of_primary_school~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                  (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(availability_of_primary_school~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                  (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)

################################################################################################################################

lm1 = lm(total_female_child_age_bw_0_6~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                       (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_female_child_age_bw_0_6~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                       (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)


################################################################################################################################

lm1 = lm(total_area_covered_under_irrigat~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                       (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_area_covered_under_irrigat~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                       (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################

lm1 = lm(is_bank_available~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                             (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(is_bank_available~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                          (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################
################################################################################################################################

lm1 = lm(is_village_connected_to_all_weat~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                          (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(is_village_connected_to_all_weat~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                          (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################
################################################################################################################################

lm1 = lm(total_no_of_registered_children_~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_no_of_registered_children_~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################################################################################################################################################

lm1 = lm(female_population~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(female_population~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################################################################################################################################################

lm1 = lm(phc~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(phc~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)



################################################################################################################################################################################################################################################################

lm1 = lm(availability_of_mother_child_hea~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(availability_of_mother_child_hea~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)

###################################################################################################################################

lm1 = lm(is_community_waste_disposal_syst~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(is_community_waste_disposal_syst~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)

################################################################################################################################
###################################################################################################################################

lm1 = lm(total_hhd_having_bpl_cards~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_hhd_having_bpl_cards~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)


###################################################################################################################################

lm1 = lm(total_hhd_having_piped_water_con~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                   (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_hhd_having_piped_water_con~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                   (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)
################################################################################################################################

###################################################################################################################################

lm1 = lm(total_hhd_not_having_sanitary_la~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "SC") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "SC") ,])
summary(lm1)

lm1 = lm(total_hhd_not_having_sanitary_la~category_label, data = df[(df$category_2005 == "GEN"|df$category_2005 == "ST") & 
                                                                         (df$category_2010 == "GEN"|df$category_2010 == "ST") ,])
summary(lm1)
################################################################################################################################

