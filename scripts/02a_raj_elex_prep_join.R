# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(here)

# Load util
source(here("scripts/00_utils.R"))

# Load dat

## Excluding Baran because they did not follow any reservation recommendations -
## Have added a footnote in the paper

#MGNREGA came to force on 2006 in Rajasthan and implemented in a phased manner.
#In Rajasthan, out of total 33 districts, 
# 6 districts namely Banswara, Dungarpur, Jhalawar, Karauli, Sirohi, Udaipur were covered in Phase I 
# of MGNREGA (2006-2007); another 6 districts
#comprising of Barmer, Chittorgarh, Jaisalmer, Jalore, Sawai Madhopur and Tonk were covered
#in Phase II of MGNREGA (2007-2008) and from April 1, 2008, remaining 21 districts, including
#Jaipur, Dausa and Sikar, were covered in Phase III of MGNREGA.
# https://www.ijamtes.org/gallery/156.aug%20ijmte%20%20-%20879.pdf
# https://www.inspirajournals.com/uploads/Issues/136293092.pdf

elex_raj_05_10 <- read_csv(here("data/raj/sp_2005_2010 - sp_2005_2010.csv")) %>%
     mutate(match_name = normalize_string(paste(dist_name_new_2010, samiti_name_new_2010, gp_new_2010))) %>%
     filter(is.na(nuke)) %>%
     group_by(match_name) %>%
     filter(n() == 1) %>%
     ungroup() %>%
     mutate(dist_name_new_2010 = tolower(dist_name_new_2010)) %>%
     filter(!(dist_name_new_2010 == 'baran')) %>%
     mutate(female_res_2005 = ifelse( grepl("W",  reservation_2005) == TRUE, 1, 0),
            female_res_2010 = ifelse( grepl("W",  reservation_2010) == TRUE, 1, 0),
            case = paste0(female_res_2005, female_res_2010, sep = "_"),
            phase_1 = dist_name_new_2010 %in% c("banswara", "dungarpur", "jhalawar", "karauli", "sirohi", "udaipur"),
            phase_2 = dist_name_new_2010 %in% c("barmer", "chittorgarh", "jaisalmer", "jalore", "sawaimadhopur", "tonk")
     )

write_parquet(elex_raj_05_10, sink = here("data/raj/elex_raj_05_10.parquet"))

# 05_10_15_20
elex_raj_05_20 <- read_csv(here("data/raj/sp_05_10_15_20_best_manual.csv")) %>%
     mutate(match_name = normalize_string(paste(dist_name_new_2010, samiti_name_new_2010, gp_new_2010))) %>%
     group_by(match_name) %>%
     filter(n() == 1) %>%
     ungroup() %>%
     filter(!(dist_name_new_2010 == 'BARAN')) %>%
     mutate(dist_name_new_2010 = tolower(dist_name_new_2010),
            female_res_2005 = ifelse( grepl("W",  reservation_2005) == TRUE, 1, 0),
            female_res_2010 = ifelse( grepl("W",  reservation_2010) == TRUE, 1, 0),
            female_res_2015 = ifelse( grepl("W",  reservation_2015) == TRUE, 1, 0),
            female_res_2020 = ifelse( grepl("W",  reservation_2020) == TRUE, 1, 0),
            case = paste(female_res_2005, female_res_2010, female_res_2015, female_res_2020, sep = "_"),
            phase_1 = dist_name_new_2010 %in% c("banswara", "dungarpur", "jhalawar", "karauli", "sirohi", "udaipur"),
            phase_2 = dist_name_new_2010 %in% c("barmer", "chittorgarh", "jaisalmer", "jalore", "sawaimadhopur", "tonk")
     )

write_parquet(elex_raj_05_20, sink = here("data/raj/elex_raj_05_20.parquet"))

