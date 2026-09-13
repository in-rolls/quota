library(dplyr)
library(here)
library(testthat)
source(here("scripts/00_linkage.R"))

test_that("exact links beat competing fuzzy links without duplicate outcomes", {
  e <- tibble(
    election_id = c("jethana", "jhadwasa"),
    election_name = c("ajmerpeesanganjethana", "ajmerpeesanganjhadwasa")
  )
  g <- tibble(local_body_code = "1", lgd_name = "ajmerpeesanganjethana")
  x <- match_panchayats(e, g)
  expect_equal(x$match_status[x$election_id == "jethana"], "exact")
  expect_equal(
    x$match_status[x$election_id == "jhadwasa"], "closer_election_exists"
  )
})

test_that("ties between distinct panchayats are left unresolved", {
  e <- tibble(election_id = c("a", "b"), election_name = c("same", "same"))
  g <- tibble(local_body_code = "1", lgd_name = "same")
  expect_true(all(match_panchayats(e, g)$match_status == "ambiguous_gp_match"))
  e <- slice(e, 1)
  g <- tibble(local_body_code = c("1", "2"), lgd_name = c("same", "same"))
  expect_true(all(
    match_panchayats(e, g)$match_status == "ambiguous_election_match"
  ))
})

test_that("catalog aliases for one ID do not create artificial ties", {
  e <- tibble(election_id = "a", election_name = "same")
  g <- tibble(local_body_code = c("1", "1"), lgd_name = c("same", "same"))
  matches <- match_panchayats(e, g)
  expect_equal(matches$match_status, "exact")
  expect_true(is.na(matches$election_margin))
  expect_true(is.na(matches$gp_margin))
})

test_that("matching is invariant to input order", {
  e <- tibble(
    election_id = c("a", "b", "c"),
    election_name = c("abcd", "abcd", "other")
  )
  g <- tibble(
    local_body_code = c("1", "2", "3"),
    lgd_name = c("abcd", "other", "others")
  )
  x <- match_panchayats(e, g) |> arrange(election_id, local_body_code)
  y <- match_panchayats(e[3:1, ], g[3:1, ]) |>
    arrange(election_id, local_body_code)
  expect_equal(x, y)
  expect_true(all(
    match_panchayats(e, g[FALSE, ])$match_status == "no_district_candidates"
  ))
})

test_that("runner-up margins retain near-ties before thresholding", {
  e <- tibble(election_id = c("a", "b"), election_name = c("abcd", "abce"))
  g <- tibble(local_body_code = c("1", "2"), lgd_name = c("abcd", "abcx"))
  matches <- match_panchayats(e, g)
  a <- filter(matches, election_id == "a")
  # The runner-up lies outside the .15 acceptance cutoff.
  expect_equal(a$election_margin, 1 / 6)
  expect_equal(a$gp_margin, 1 / 6)
  e$election_name <- "abcd"
  expect_true(all(match_panchayats(e, g)$gp_margin == 0))
  g$lgd_name <- "abcd"
  expect_true(all(match_panchayats(e, g)$election_margin == 0))
})

test_that("different numbered panchayats cannot match", {
  for (pair in list(c("GP 45", "GP 44"), c("GP ४५", "GP ४४"),
                    c("ganganagarpadampur35bb", "ganganagarpadampur23bb"))) {
    e <- tibble(election_id = "a", election_name = pair[1])
    g <- tibble(local_body_code = "1", lgd_name = pair[2])
    expect_false(any(
      match_panchayats(e, g)$match_status %in% c("exact", "fuzzy")
    ))
  }
})

for (state in c("raj", "up")) {
  d <- arrow::read_parquet(here(
    "data", state, paste0("shrug_lgd_", state, "_elex_05_10.parquet")
  ))
  geography <- arrow::read_parquet(here(
    "data", state, "shrug_lgd_geography.parquet"
  ))
  links <- arrow::read_parquet(here(
    "data", state, "lgd_election_links.parquet"
  ))
  accepted_links <- filter(links, match_status %in% c("exact", "fuzzy"))
  margin_summary <- accepted_links |>
    summarise(
      links = n(),
      near_ties = sum(election_margin < .01 | gp_margin < .01, na.rm = TRUE),
      .by = match_status
    )
  message(state, ": margins below .01 (diagnostic only)")
  print(margin_summary)
  test_that(paste(state, "outcomes and histories are unique"), {
    expect_false(anyNA(d$local_body_code))
    expect_equal(anyDuplicated(d$shrid2), 0L)
    histories <- distinct(
      d, local_body_code, election_id, female_res_2005, female_res_2010
    )
    expect_equal(anyDuplicated(histories$local_body_code), 0L)
    expect_equal(anyDuplicated(histories$election_id), 0L)
    bad <- geography |> filter(geography_status != "resolved" | gp_withheld)
    expect_false(any(d$local_body_code %in% bad$local_body_code))
    accepted <- filter(links, match_status %in% c("exact", "fuzzy"))
    expect_setequal(d$local_body_code, accepted$local_body_code)
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(d, path)
    expect_equal(d, arrow::read_parquet(path))
    unlink(path)
  })
}

# Reproduce the historical table and isolate the linkage change. Definitions,
# equal-GP weighting and conventional OLS inference are held fixed.
baseline_ref <- commandArgs(trailingOnly = TRUE)
if (length(baseline_ref) != 1L) {
  stop(
    "Supply the baseline Git reference: ",
    "Rscript --vanilla scripts/validate_linkage.R a8f5cca"
  )
}
outcomes <- c(
  scholarship_recipients = "total_minority_children_getting_",
  primary_school_availability = "availability_of_primary_school",
  midday_meal_availability = "availability_of_mid_day_meal_sch",
  immunized_children = "total_childs_aged_0_to_3_years_i"
)
library(stringi)
source(here("scripts/00_utils.R"))
ay <- readr::read_csv(
  shrug_path("shrug-antyodaya-csv/antyodaya_shrid.csv.zip"),
  col_select = all_of(c("shrid2", unname(outcomes))), show_col_types = FALSE
)
results <- list()
for (state in c("raj", "up")) {
  relative_path <- paste0(
    "data/", state, "/shrug_lgd_", state, "_elex_05_10.parquet"
  )
  old_path <- tempfile(fileext = ".parquet")
  status <- system2("git", c(
    "-C", shQuote(here()), "show",
    shQuote(paste0(baseline_ref, ":", relative_path))
  ), stdout = old_path)
  stopifnot(status == 0L)
  historical <- arrow::read_parquet(old_path)
  unlink(old_path)
  current <- arrow::read_parquet(here(relative_path))
  scenarios <- list(
    historical = historical,
    historical_reversed = historical[rev(seq_len(nrow(historical))), ],
    repaired = current,
    exact_names = filter(current, match_status == "exact")
  )
  for (scenario in names(scenarios)) {
    d <- scenarios[[scenario]] |>
      inner_join(ay, by = "shrid2", relationship = "many-to-one")
    if (scenario %in% c("historical", "historical_reversed")) {
      # Preserve the old bug only in this historical reproduction.
      g <- d |>
        group_by(key) |>
        summarise(
          female_res_2005 = first(as.numeric(female_res_2005)),
          female_res_2010 = first(as.numeric(female_res_2010)),
          across(all_of(unname(outcomes)), sum), .groups = "drop"
        )
    } else {
      g <- d |>
        mutate(across(c(female_res_2005, female_res_2010), as.numeric)) |>
        group_by(local_body_code, female_res_2005, female_res_2010) |>
        summarise(across(all_of(unname(outcomes)), sum), .groups = "drop")
      stopifnot(!anyDuplicated(g$local_body_code))
    }
    for (outcome in names(outcomes)) {
      formula <- reformulate(
        c("female_res_2005", "female_res_2010"), outcomes[[outcome]]
      )
      model <- lm(formula, g)
      if (!scenario %in% c("historical", "historical_reversed")) {
        reversed <- lm(formula(model), g[rev(seq_len(nrow(g))), ])
        expect_equal(
          unname(coef(model)), unname(coef(reversed)),
          tolerance = 1e-10
        )
      }
      results[[paste(state, scenario, outcome)]] <-
        broom::tidy(model, conf.int = TRUE) |>
        mutate(
          state = state, scenario = scenario, outcome = outcome,
          n = nobs(model),
          outcome_mean = mean(model.response(model.frame(model))),
          baseline_ref = baseline_ref, .before = 1
        )
    }
  }
}
readr::write_csv(bind_rows(results), here("tabs/linkage_comparison.csv"))
print(bind_rows(results) |> filter(
  term == "female_res_2010",
  outcome == "scholarship_recipients"
), width = Inf)
