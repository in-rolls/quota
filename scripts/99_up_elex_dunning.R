# Load libs
library(readr)
library(foreign)
library(here)
library(janitor)
library(haven)
library(dplyr)
library(fixest)

# Load dat
dta_files <- list.files(path = here("data/up/dunning/by_district/"), pattern = "\\.dta$", full.names = TRUE)
data_list <- lapply(dta_files, read_dta)
up_05_10_d <- bind_rows(data_list) %>% clean_names()


#a <- read_dta("data/up/dunning/Uttar Pradesh_panchayats.dta")

# The nine districts in our dataset are Barabanki, Chitrakoot, Fatehpur, Hardoi, Kheri (NREGA Phase I districts)
# and Ambedkar Nagar, Basti, Etah, Jhansi (NREGA Phase II districts)

# Phase I Districts:  Azamgarh, Banda, Barabanki, Chandauli, Chitrakoot, Fatehpur, Gorakhpur, Hamirpur, Hardoi, Jalaun, Jaunpur, Kaushambi, Kushinagar, Lakimpur Kheri, Lalitpur, Mahoba, Mirzapur, Pratapgarh, Rae Bareli, Sitapur, Sonbhadra, Unnao  
# Phase II Districts: Ambedkar Nagar, Bahraich, Ballia, Balrampur, Basti, Budaun, Etah, Farrukhabad, Gonda, Jhansi, Maharajganj, Mau, Ramabai Nagar (Kanpur Dehat), Sant Kabir Nagar,  Shrawasti, Siddharth Nagar, Sultanpur
# Phase III Districts: Bijnor, Moradabad, Rampur, Saharanpur, Muzaffarnagar, Meerut, Ghaziabad, Bulandshahar, Aligarh, Mathura, Agra, Firozabad, Mainpuri, Bareilly, Pilibhit, Shahjahanpur, Lucknow, Kanpur Nagar, Allahabad, Faizabad, Deoria, Etawah, Ghazipur, Varanasi, Gautam Buddha Nagar, Baghpat, Hathras, Kannauj, Jyotiba Phule Nagar, Auraiya, Sant Ravidas Nagar, Kanshiram Nagar

up_05_10_d <- up_05_10_d %>% mutate(
     key = tolower(paste(districtname, block_name, village_panchayatname)),
     phase_1 = tolower(districtname) %in% c("barabanki", "chitrakoot", "fatehpur", "hardoi", "kheri"),
     phase_2 = tolower(districtname) %in% c("ambedkar nagar", "basti", "etah", "jhansi"),
     phase_1c = tolower(districtname) %in% c("barabanki", "chitrakoot", "fatehpur", "hardoi", "kheri", "pratapgarh", "rae bareli", "sitapur", "unnao"),
     phase_2c = tolower(districtname) %in% c("ambedkar nagar", "ballia", "basti", "etah", "jhansi", "mau", "siddharth nagar"),
     female_reserved_05 = na_if(female_reserved_05, 99),
     female_reserved_10 = na_if(female_reserved_10, 99),
     districtname = tolower(districtname),
     block_name = tolower(block_name),
     village_panchayatname = tolower(village_panchayatname)
)

# Validation
validation <- up_05_10 %>% 
     inner_join(up_05_10_d, by = c("eng_key_2005" = "key"))
# 73 of 1052 mismatch
print(validation[validation$female_res_2005 != validation$female_reserved_05, 
                     c("female_res_2005", "female_reserved_05", "page_2005", "sr_no_2005", 
                       "gp_name_eng_2005", "gp_res_status_fin_eng_2005")], n = 50)
# confirmed with pdf for a few: errors with Dunning
# dunning et al. just picked districts where mean reserved in 2005 is substantially higher: 
# mean(up_05_10[tolower(up_05_10$district_name_eng_2005) %in% unique(up_05_10_d$districtname),]$female_res_2005)

# Basics
mean(up_05_10_d$female_reserved_05, na.rm = T)
mean(up_05_10_d$female_reserved_10, na.rm = T)
nrow(up_05_10_d[up_05_10_d$phase_1 == 1,])
nrow(up_05_10_d[up_05_10_d$phase_2 == 1,])

# Random or not
with(up_05_10_d[up_05_10_d$phase_1 == 1, ], summary(lm(female_reserved_10 ~ female_reserved_05)))
with(up_05_10_d[up_05_10_d$phase_2 == 1, ], summary(lm(female_reserved_10 ~ female_reserved_05)))

# Paper/MNREGA merge guidance
# Our employment data are from FY 2010–11 to FY 2012–13. 
# The website does not provide data on expenditure or works for FY 2010–11, 
# and so we use data from FY 2011–12 to FY 2013–14.

# Join long data to MNREGA
up_05_10_mnrega_r6 <- up_05_10_d %>%
     inner_join(mnrega_r6_wide, by = "key")

nrow(up_05_10_mnrega_r6[up_05_10_mnrega_r6$phase_1 == 1, ])
nrow(up_05_10_mnrega_r6[up_05_10_mnrega_r6$phase_2 == 1, ])

with(up_05_10_mnrega_r6[up_05_10_mnrega_r6$phase_1 == 1, ], summary(lm(female_reserved_10 ~ female_reserved_05)))
with(up_05_10_mnrega_r6[up_05_10_mnrega_r6$phase_2 == 1, ], summary(lm(female_reserved_10 ~ female_reserved_05)))

## Replicate Table 1
up_05_10_mnrega_r6 %>% 
     filter(phase_1 == 1) %>% 
     summarize(meanf_reserved_05 = mean(female_reserved_05), 
               meanf_reserved_10 = mean(female_reserved_10, na.rm = T), 
               share_female = mean(tot_population_female/total_population, na.rm = T), 
               share_sc = mean(scheduled_caste_population/total_population, na.rm = T),
               share_scf = mean(scheduled_caste_population_femal/total_population, na.rm = T),
               n = n())

up_05_10_mnrega_r6 %>% 
     filter(phase_2 == 1) %>% 
     summarize(meanf_reserved_05 = mean(female_reserved_05, na.rm = T), 
               meanf_reserved_10 = mean(female_reserved_10, na.rm = T), 
               share_female = mean(tot_population_female/total_population, na.rm = T), 
               share_sc = mean(scheduled_caste_population/total_population, na.rm = T),
               share_scf = mean(scheduled_caste_population_femal/total_population, na.rm = T),
               n = n())

# with(up_05_10_mnrega_r6, range(scheduled_caste_population_femal/total_population, na.rm = T))
# [1] 0 1
# with(up_05_10_mnrega_r6, sum(scheduled_caste_population_femal/total_population == 1, na.rm = T))
# [1] 60
# with(up_05_10_mnrega_r6, range(total_population, na.rm = T))
# [1]    11 29212

up_05_10_mnrega_r1 <- up_05_10_d %>%
     inner_join(mnrega_r1, by = "key")

up_05_10_mnrega_r1 %>%
     filter(phase_1 == 1) %>%
     group_by(female_reserved_05) %>%
     summarize(job_cards = mean(tot_job_cards_issued_11_14),
               se = sqrt(var(tot_job_cards_issued_11_14))/sqrt(n()),
               total_women = mean(tot_reg_women_11_14))


feols(tot_drought_comp_project_11_14 ~ female_reserved_05 + 
                                        total_population + 
                                        I(paste0(districtname, block_name)),
                                        data = up_05_10_mnrega_r6[up_05_10_mnrega_r6$phase_1 == 1, ])

#I(tot_population_female/total_population) + 
#     ,
