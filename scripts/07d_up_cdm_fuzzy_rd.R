# Chaturvedi, Das and Mahajan (2023), Table C15: GP-level NREGS spending.
library(arrow)
library(broom)
library(dplyr)
library(estimatr)
library(here)
library(kableExtra)
library(purrr)
library(readr)
library(tidyr)

cdm_data <- read_parquet(here("data/cdm/up_gp_mnrega_2015.parquet")) |>
  transmute(
    gp_id = eleid,
    spending = 100 * (
      Labour_exp_disbursed_Lakhs2016 + Material_exp_disbursed_Lakhs2016
    ) / tot_new,
    population = tot_new,
    reservation = femalereservation,
    assignment = femaleinstrument,
    running_variable = running_variable_cdm,
    muslim_share
  )
stopifnot(
  !anyNA(cdm_data$gp_id), !anyDuplicated(cdm_data$gp_id),
  all(cdm_data$population > 0),
  all(na.omit(cdm_data$reservation) %in% 0:1),
  all(na.omit(cdm_data$assignment) %in% 0:1),
  all(cdm_data$assignment == (cdm_data$running_variable >= 0), na.rm = TRUE)
)

specifications <- list(
  Overall = spending ~ reservation + running_variable +
    assignment:running_variable |
    assignment + running_variable + assignment:running_variable,
  Heterogeneity = spending ~ reservation * muslim_share + running_variable +
    assignment:running_variable + running_variable:muslim_share +
    assignment:running_variable:muslim_share |
    assignment * muslim_share + running_variable +
      assignment:running_variable + running_variable:muslim_share +
      assignment:running_variable:muslim_share
)
model_grid <- expand_grid(
  specification = names(specifications), bandwidth = c(0.1, 0.075, 0.05)
) |>
  mutate(column = row_number())
models <- vector("list", nrow(model_grid))
samples <- vector("list", nrow(model_grid))

for (i in seq_len(nrow(model_grid))) {
  bandwidth <- model_grid$bandwidth[[i]]
  formula <- specifications[[model_grid$specification[[i]]]]
  samples[[i]] <- cdm_data |>
    filter(abs(running_variable) < .env$bandwidth, is.finite(spending)) |>
    drop_na(all_of(all.vars(formula))) |>
    mutate(weight = 1 - abs(running_variable) / .env$bandwidth)
  models[[i]] <- iv_robust(
    formula,
    data = samples[[i]], weights = weight,
    se_type = "HC0", diagnostics = TRUE
  )
  stopifnot(models[[i]]$rank == models[[i]]$k)
}

# HC0 reproduces the source's probability-weighted ivregress standard errors.
results <- map2_dfr(models, seq_along(models), \(model, i) {
  tidy(model) |>
    mutate(
      specification = model_grid$specification[[i]],
      bandwidth = model_grid$bandwidth[[i]], column = i,
      n = nobs(model),
      control_mean = mean(
        samples[[i]]$spending[samples[[i]]$running_variable <= 0]
      ),
      .before = 1
    )
})
write_csv(results, here("output/cdm_mnrega_fuzzy_rd.csv"))

coefficient_labels <- c(
  reservation = "Female reservation",
  "reservation:muslim_share" = "Reservation $\\times$ Muslim share",
  muslim_share = "Muslim share"
)
coefficient_rows <- results |>
  filter(term %in% names(coefficient_labels)) |>
  mutate(term = factor(term, levels = names(coefficient_labels))) |>
  select(column, term, estimate, std.error) |>
  pivot_longer(c(estimate, std.error), names_to = "statistic") |>
  mutate(
    statistic = factor(statistic, levels = c("estimate", "std.error")),
    value = if_else(
      statistic == "estimate", sprintf("%.4f", value),
      sprintf("(%.4f)", value)
    )
  ) |>
  arrange(term, statistic) |>
  pivot_wider(names_from = column, values_from = value, values_fill = "") |>
  mutate(label = if_else(
    statistic == "estimate", unname(coefficient_labels[as.character(term)]), ""
  )) |>
  select(label, all_of(as.character(seq_along(models))))
summary_rows <- results |>
  distinct(column, bandwidth, n, control_mean) |>
  transmute(
    column,
    Bandwidth = sprintf("%.3f", bandwidth),
    `Control mean` = sprintf("%.4f", control_mean),
    Panchayats = format(n, big.mark = ",", trim = TRUE)
  ) |>
  pivot_longer(-column, names_to = "label") |>
  pivot_wider(names_from = column, values_from = value)

bind_rows(coefficient_rows, summary_rows) |>
  knitr::kable(
    format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    col.names = c("", paste0("(", seq_along(models), ")")),
    align = c("l", rep("r", length(models))),
    caption = paste(
      "Female reservation and NREGS spending:",
      "replication of CDM Table C15"
    ),
    label = "mnrega-up-cdm-fuzzy-rd"
  ) |>
  add_header_above(c(" " = 1, "Overall" = 3, "By Muslim share" = 3)) |>
  kable_styling(font_size = 9, latex_options = "hold_position") |>
  footnote(
    general = paste(
      "Outcome: total NREGS expenditure per person in FY 2016--17,",
      "in thousands of rupees. Local linear 2SLS with triangular weights",
      "and separate slopes across the assignment cutoff. Assignment",
      "instruments reservation; in columns (4)--(6), assignment interacted",
      "with Muslim share also instruments reservation interacted with that",
      "share. These columns include Muslim share and its interactions with",
      "both slopes; the reservation coefficient is the effect at zero",
      "Muslim share. The running variable follows the source analysis.",
      "Missing outcomes are excluded. Control means are",
      "unweighted means below or at the cutoff in each estimation sample.",
      "Heteroskedasticity-robust (HC0) standard errors in parentheses;",
      "one observation per panchayat. Source: Chaturvedi, Das and Mahajan",
      "(2023), Table C15, and their replication files."
    ),
    general_title = "", threeparttable = TRUE, escape = FALSE
  ) |>
  save_kable(here("tabs/mnrega_up_cdm_fuzzy_rd.tex"))

print(results |> filter(term %in% names(coefficient_labels)))
