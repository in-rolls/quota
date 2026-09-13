library(arrow)
library(dplyr)
library(here)
library(purrr)
library(stringi)
library(stringr)

source(here("scripts/00_utils.R"))

# Load dat
mnrega_r1 <- read_parquet(here("data/mnrega/mnrega_r1.parquet"))
mnrega_r3 <- read_parquet(here("data/mnrega/mnrega_r3.parquet"))
mnrega_r5 <- read_parquet(here("data/mnrega/mnrega_r5.parquet"))
mnrega_r6 <- read_parquet(here("data/mnrega/mnrega_r6.parquet"))
elex_up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))
elex_up_05_21 <- read_parquet(here("data/up/up_05_21.parquet"))

# Join MNREGA Reports
mnrega_up_dupes <- mnrega_r6 %>%
  inner_join(mnrega_r1, by = "state_key") %>%
  inner_join(mnrega_r3, by = "state_key") %>%
  inner_join(mnrega_r5, by = "state_key") %>%
  filter(state.x == "UTTAR PRADESH")

mnrega_up <- mnrega_up_dupes %>%
  select(-contains(".y"), -contains(".x.x"), -contains(".y.y")) %>%
  rename_with(~ str_remove(., "\\..*$"))

mnrega_up <- mnrega_up %>%
  mutate(
    district = tolower(district),
    match_name = gsub(" ", "", normalize_string(paste(district, block, panchayat)))
  )

elex_up_05_10 <- elex_up_05_10 %>%
  mutate(match_name = gsub(" ", "", normalize_string(paste(
    district_name_eng_2010,
    block_name_eng_2010,
    gp_name_eng_2010
  ))))

elex_up_05_21 <- elex_up_05_21 %>%
  mutate(match_name = gsub(" ", "", normalize_string(paste(
    district_name_eng_2010,
    block_name_eng_2010,
    gp_name_eng_2010
  ))))

# Calculate the same nearest-name distances in district batches.
match_mnrega <- function(elections, outcomes) {
  elections <- elections |> mutate(election_row = row_number())
  outcomes <- outcomes |> mutate(outcome_row = row_number())
  districts <- intersect(tolower(elections$district_name_eng_2010), tolower(outcomes$district))
  matches <- map_dfr(districts, function(district_name) {
    a <- elections |> filter(tolower(district_name_eng_2010) == district_name)
    b <- outcomes |> filter(tolower(district) == district_name)
    distance <- stringdist::stringdistmatrix(
      tolower(a$match_name), tolower(b$match_name),
      method = "jw", p = 0, nthread = 1
    )
    nearest <- which(distance == apply(distance, 1, min), arr.ind = TRUE)
    tibble(
      election_row = a$election_row[nearest[, 1]],
      outcome_row = b$outcome_row[nearest[, 2]],
      dist_mnrega_match = distance[nearest]
    )
  })
  matches |>
    left_join(elections, by = "election_row", relationship = "many-to-one") |>
    left_join(outcomes, by = "outcome_row", relationship = "many-to-one") |>
    arrange(election_row, outcome_row) |>
    select(-election_row, -outcome_row)
}

mnrega_elex_up_05_10 <- match_mnrega(elex_up_05_10, mnrega_up)
mnrega_elex_up_05_21 <- match_mnrega(elex_up_05_21, mnrega_up)

# Remove duplicates and filter down to where  dist_mnrega_match < .1
mnrega_elex_up_05_10_dedupe <- mnrega_elex_up_05_10 %>%
  filter(dist_mnrega_match < 0.1) %>%
  group_by(match_name.x) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  group_by(match_name.y) %>%
  filter(n() == 1) %>%
  ungroup()

mnrega_elex_up_05_21_dedupe <- mnrega_elex_up_05_21 %>%
  filter(dist_mnrega_match < 0.1) %>%
  group_by(match_name.x) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  group_by(match_name.y) %>%
  filter(n() == 1) %>%
  ungroup()

write_parquet(mnrega_elex_up_05_10_dedupe, sink = here("data/up/mnrega_elex_up_05_10.parquet"))
write_parquet(mnrega_elex_up_05_21_dedupe, sink = here("data/up/mnrega_elex_up_05_21.parquet"))
