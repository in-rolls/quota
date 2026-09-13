library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

source(here("scripts/00_utils.R"))

cdm_data <- read_parquet(here("data/cdm/up_gp_mnrega_2015.parquet"))
stopifnot(!anyNA(cdm_data$eleid), !anyDuplicated(cdm_data$eleid))

# Assume absent categories have zero spending when total spending is reported.
# Keep the recorded-only alternative to show sensitivity to that assumption.
annual_recorded <- cdm_data |>
  select(eleid, tot_new, matches("exp.*Lakhs[0-9]{4}$")) |>
  pivot_longer(
    -c(eleid, tot_new),
    names_to = c("component", "year"),
    names_pattern = "(.*)([0-9]{4})$", names_transform = list(year = as.integer)
  ) |>
  pivot_wider(names_from = component, values_from = value) |>
  transmute(
    eleid, year,
    population = tot_new,
    total_reported = !is.na(Labour_exp_disbursed_Lakhs) &
      !is.na(Material_exp_disbursed_Lakhs),
    sanit_labour = sanitation_Labour_exp_Lakhs,
    sanit_material = sanit_Material_exp_Lakhs,
    water_labour = water_Labour_exp_Lakhs,
    water_material = water_Material_exp_Lakhs,
    total_labour = Labour_exp_disbursed_Lakhs,
    total_material = Material_exp_disbursed_Lakhs
  ) |>
  pivot_longer(
    matches("^(sanit|water|total)_(labour|material)$"),
    names_to = c("outcome", ".value"), names_sep = "_"
  ) |>
  mutate(
    outcome = paste0("nrega_", outcome),
    spending = 100 * (labour + material) / population,
    recorded = !is.na(spending),
    absent_category = outcome != "nrega_total" & total_reported &
      is.na(labour) & is.na(material)
  )
stopifnot(all(cdm_data$tot_new > 0))

annual_spending <- bind_rows(
  annual_recorded |>
    mutate(
      assumption = "absent_category_zero",
      assumed_zero = absent_category,
      spending = if_else(assumed_zero, 0, spending)
    ),
  annual_recorded |>
    mutate(assumption = "recorded_only", assumed_zero = FALSE)
) |>
  select(assumption, eleid, year, outcome, spending, recorded, assumed_zero)
stopifnot(
  all(!annual_spending$assumed_zero | !annual_spending$recorded),
  all(annual_spending$spending[annual_spending$assumed_zero] == 0)
)

period_spending <- annual_spending |>
  filter(year >= 2012) |>
  mutate(period = if_else(year <= 2015, "pre", "post")) |>
  summarise(
    years_observed = sum(recorded),
    years_assumed_zero = sum(assumed_zero),
    years_available = sum(!is.na(spending)),
    spending = if (any(!is.na(spending))) {
      sum(spending, na.rm = TRUE)
    } else {
      NA_real_
    },
    .by = c(assumption, eleid, outcome, period)
  )
stopifnot(all(
  is.na(period_spending$spending) == (period_spending$years_available == 0)
))

spending_wide <- bind_rows(
  annual_spending |> mutate(outcome = paste(outcome, year, sep = "_")),
  period_spending |> mutate(outcome = paste(outcome, period, sep = "_"))
) |>
  select(assumption, eleid, outcome, spending) |>
  pivot_wider(names_from = outcome, values_from = spending)
analysis_data <- cdm_data |>
  select(eleid, femalereservation, runningvar2_norm_std, district) |>
  left_join(spending_wide, by = "eleid", relationship = "one-to-many")

outcomes_post <- c(
  "nrega_sanit_post", "nrega_water_post", "nrega_total_post",
  "nrega_sanit_2016", "nrega_sanit_2017", "nrega_sanit_2018"
)
labels_post <- c(
  "Sanit. Post", "Water Post", "Total Post",
  "Sanit. 2016", "Sanit. 2017", "Sanit. 2018"
)
outcomes_all <- c(
  "nrega_sanit_post", "nrega_water_post", "nrega_total_post",
  "nrega_sanit_pre", "nrega_water_pre", "nrega_total_pre"
)
labels_all <- c(
  "Sanit. Post", "Water Post", "Total Post",
  "Sanit. Pre", "Water Pre", "Total Pre"
)
common_notes <- c(
  "Outcomes: MNREGA spending in thousands of rupees per person.",
  "Pre = FY 2012--13 through 2015--16; Post = FY 2016--17 through 2018--19.",
  "FY 2015--16 spans the October 2015 election.",
  "Absent categories are set to zero when both total spending components",
  "are reported for that GP-year; other missing amounts remain missing.",
  "Period totals sum available years; entirely missing periods are excluded.",
  "These associations do not implement the source paper's fuzzy RD.",
  "Data: Chaturvedi, Das \\& Mahajan (2023)."
)

local_results <- list()
ols_results <- list()
for (coding_assumption in c("absent_category_zero", "recorded_only")) {
  assumption_data <- analysis_data |>
    filter(assumption == coding_assumption)
  for (bandwidth in c(0.1, 0.075, 0.05)) {
    sample_data <- assumption_data |>
      filter(abs(runningvar2_norm_std) < bandwidth) |>
      mutate(weight = 1 - abs(runningvar2_norm_std) / bandwidth)
    models <- map(outcomes_post, \(outcome) {
      lm(reformulate(c("femalereservation", "runningvar2_norm_std"), outcome),
        data = sample_data, weights = weight
      )
    })
    local_results[[paste(coding_assumption, bandwidth)]] <- map2_dfr(
      models, outcomes_post, \(model, outcome) {
        tidy(model, conf.int = TRUE) |>
          mutate(
            assumption = coding_assumption, bandwidth, outcome,
            n = nobs(model), .before = 1
          )
      }
    )
    if (coding_assumption == "recorded_only") next
    suffixes <- c("0.1" = "10", "0.075" = "075", "0.05" = "05")
    suffix <- suffixes[[as.character(bandwidth)]]
    custom_stargazer(
      models,
      digits = 2, float.env = "sidewaystable",
      title = paste0(
        "Female Reservation and MNREGA Spending (UP 2015, BW=", bandwidth, ")"
      ),
      covariate.labels = c("Female Reservation", "Running Variable"),
      column.labels = labels_post,
      label = paste0("tab:mnrega_up_cdm_bw", suffix),
      notes = c(
        paste0(
          "Bandwidth = ", bandwidth,
          "; triangular-weighted OLS with a common slope."
        ),
        common_notes
      ),
      add.lines = list(c(
        "Bandwidth", rep(as.character(bandwidth), length(models))
      )),
      out = here("tabs", paste0("mnrega_up_cdm_bw", suffix, ".tex"))
    )
  }
  for (specification in c("ols", "ols_fe")) {
    predictors <- c(
      "femalereservation",
      if (specification == "ols_fe") "factor(district)"
    )
    models <- map(outcomes_all, \(outcome) {
      lm(reformulate(predictors, outcome), data = assumption_data)
    })
    ols_results[[paste(coding_assumption, specification)]] <- map2_dfr(
      models, outcomes_all, \(model, outcome) {
        tidy(model, conf.int = TRUE) |>
          filter(term == "femalereservation") |>
          mutate(
            assumption = coding_assumption, specification, outcome,
            n = nobs(model), .before = 1
          )
      }
    )
    if (coding_assumption == "recorded_only") next
    custom_stargazer(
      models,
      column.labels = labels_all,
      covariate.labels = c("Female Reservation", "Constant"), omit = "^factor",
      title = paste0(
        "Female Reservation and MNREGA Spending (UP 2015, ",
        if_else(specification == "ols_fe", "District FE", "OLS"), ")"
      ),
      label = paste0("tab:mnrega_up_cdm_", specification),
      notes = c(
        if (specification == "ols_fe") {
          "OLS with district fixed effects."
        } else {
          "Unadjusted OLS."
        },
        common_notes
      ),
      out = here("tabs", paste0("mnrega_up_cdm_", specification, ".tex"))
    )
  }
}
write_csv(bind_rows(local_results), here("output/cdm_mnrega_local_ols.csv"))
write_csv(bind_rows(ols_results), here("output/cdm_mnrega_ols.csv"))

period_spending |>
  semi_join(filter(cdm_data, !is.na(femalereservation)), by = "eleid") |>
  summarise(
    panchayats = n(), observed = sum(years_observed > 0),
    available = sum(years_available > 0),
    missing = sum(years_available == 0),
    with_assumed_zero = sum(years_assumed_zero > 0),
    complete_period = sum(years_available == if_else(period == "pre", 4L, 3L)),
    mean = mean(spending, na.rm = TRUE), sd = sd(spending, na.rm = TRUE),
    .by = c(assumption, outcome, period)
  ) |>
  write_csv(here("output/cdm_mnrega_summary.csv"))
