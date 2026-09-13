# Match distinct election panchayats to LGD panchayats within one district.
# Multiple catalog names may refer to the same numeric LGD identifier.
match_panchayats <- function(elections, catalog, max_distance = 0.15) {
  stopifnot(
    !anyDuplicated(elections$election_id),
    !anyNA(elections$election_id), !anyNA(elections$election_name),
    !anyNA(catalog$local_body_code), !anyNA(catalog$lgd_name)
  )
  if (nrow(catalog) == 0L) {
    return(dplyr::tibble(
      election_id = elections$election_id,
      local_body_code = NA_character_,
      dist_elex_lgd_match = NA_real_, election_ties = 0L, gp_ties = 0L,
      election_margin = NA_real_, gp_margin = NA_real_,
      match_status = "no_district_candidates"
    ))
  }
  distances <- stringdist::stringdistmatrix(
    elections$election_name, catalog$lgd_name,
    method = "jw", p = 0
  )
  # Exact string equality is independent of floating-point distance rounding.
  distances[outer(elections$election_name, catalog$lgd_name, "==")] <- 0
  election_numbers <- stringi::stri_extract_all_regex(
    chartr("०१२३४५६७८९", "0123456789", elections$election_name), "\\p{N}+",
    omit_no_match = TRUE
  ) |>
    vapply(paste, character(1), collapse = "|")
  gp_numbers <- stringi::stri_extract_all_regex(
    chartr("०१२३४५६७८९", "0123456789", catalog$lgd_name), "\\p{N}+",
    omit_no_match = TRUE
  ) |>
    vapply(paste, character(1), collapse = "|")
  numbered <- outer(election_numbers != "", gp_numbers != "", "&")
  distances[numbered & outer(election_numbers, gp_numbers, "!=")] <- Inf
  aliases <- split(seq_len(nrow(catalog)), catalog$local_body_code)
  gp_distances <- vapply(aliases, function(columns) {
    apply(distances[, columns, drop = FALSE], 1, min)
  }, numeric(nrow(elections)))
  gp_distances <- matrix(
    gp_distances,
    nrow = nrow(elections), dimnames = list(NULL, names(aliases))
  )
  election_min <- apply(gp_distances, 1, min)
  gp_min <- apply(gp_distances, 2, min)
  # Runner-up gaps across distinct IDs, before the distance cutoff.
  election_second <- apply(gp_distances, 1, function(x) {
    if (length(x) > 1L) sort(x, partial = 2)[2] else NA_real_
  })
  gp_second <- apply(gp_distances, 2, function(x) {
    if (length(x) > 1L) sort(x, partial = 2)[2] else NA_real_
  })
  election_best <- gp_distances == election_min
  gp_best <- sweep(gp_distances, 2, gp_min, "==")
  candidates <- which(election_best, arr.ind = TRUE)
  dplyr::tibble(
    election_id = elections$election_id[candidates[, 1]],
    local_body_code = colnames(gp_distances)[candidates[, 2]],
    dist_elex_lgd_match = gp_distances[candidates],
    election_ties = rowSums(election_best)[candidates[, 1]],
    gp_ties = colSums(gp_best)[candidates[, 2]],
    election_margin = (election_second - election_min)[candidates[, 1]],
    gp_margin = unname((gp_second - gp_min)[candidates[, 2]]),
    match_status = dplyr::case_when(
      dist_elex_lgd_match >= max_distance ~ "outside_distance_cutoff",
      election_ties > 1 ~ "ambiguous_election_match",
      !gp_best[candidates] ~ "closer_election_exists",
      gp_ties > 1 ~ "ambiguous_gp_match",
      dist_elex_lgd_match == 0 ~ "exact",
      TRUE ~ "fuzzy"
    )
  )
}
