library(haven)
library(tidyverse)

# NOTES regarding messiness in REDS 2014/2015 data:  

# 1. In Household data, Gram Panchayat has a number, whilst in village data, GP has names.
#     -> furthermore, in HH data all location info (state/district etc) have numbers,
#         but in village data they all have names.
#     -> THIS MEANS village id is the only common identifier between village and HH data.
# 2. In HH data phase 1, GP number is fully missing. In phase 2, GP number = village number.
# 3. In village data, phase 1,
#    -> 31 observations are coded as missing village ID (9999)
#    -> village IDs 1 and 2 have been given to two different villages
# 4. In village data, phase 2, village ID 162 has been given to two different villages.
# 5. In village data, village IDs repeat, but are used for different villages.
#     -> i.e., village 34 in phase 1 is a given village,
#         but in phase 2 it is is a totally different village.
#      -> HH data does not seem to have this issue

#_______________________________________________________________________________________#

#Prepping data on reservation in villages.

setwd("C:/Users/indir/Dropbox/India Reservations/data/reds")

vdata1 = read_dta("./reds06/Sepri1/Village/SECTION_11_D.dta") %>% #phase 1 villages
     mutate(phase=1) 

vdata2 = read_dta("./reds06/Sepri2/Village/SECTION_11_D.dta") %>% #phase 2 villages
     mutate(phase=2)

#To check village id counts: 
check1 = vdata1 %>% count(village_sr_no)
check2 = vdata2 %>% count(village_sr_no)

vdata = bind_rows(vdata1, vdata2) %>% 
  rename(village_id = village_sr_no) %>%
  filter(village_id != 9999) %>%  #removing cases with missing village ID
     filter(village_id != 1) %>%   #removing villages incorrectly assigned
     filter(village_id != 2) %>%
     filter(village_id != 162) %>% 
  select(village_id:s11_d1_p35_q07) %>%
  select(!s11_d1_p35_q05:s11_d1_p35_q06) 

vdata= vdata %>%
  rename(panchayat_term = s11_d1_p35_q01) %>%
  rename(year_of_election = s11_d1_p35_q03) %>%
  rename(month_of_election = s11_d1_p35_q04) %>%
  rename(reservation_women = s11_d1_p35_q07) 

vdata %>% summarize(n_distinct(village_id))

#rm(vdata1, vdata2, check1, check2)

vdata = pivot_wider(data=vdata, 
                    names_from=panchayat_term, 
                    values_from =c(year_of_election, 
                                   month_of_election, reservation_women)) %>%
  rename(reserved_now = reservation_women_1) %>%
  rename(reserved_before = reservation_women_2) 
 
vdata %>% summarize(n_distinct(village_id))


#_________________________________________________________________________________#

#Prepping data on households and NREGS. 

#Phase 1

hh107 = read_dta("./reds06/Sepri1/HH/SECTION07.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q7_1)

hh109 = read_dta("./reds06/Sepri1/HH/SECTION09.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q9_2)

hh111 = read_dta("./reds06/Sepri1/HH/SECTION11.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q11_2) 


#merging the three sections together 

hh_1 = hh107 %>% 
  inner_join(hh109, by = c("village_id", "HHID", "Member_ID")) %>%
  inner_join(hh111, by = c("village_id", "HHID", "Member_ID"))

#Phase 2 

hh207 = read_dta("./reds06/Sepri2/HH/SECTION07.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q7_1)

hh209 = read_dta("./reds06/Sepri2/HH/SECTION09.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q9_2)

hh211 = read_dta("./reds06/Sepri2/HH/SECTION11.dta") %>% rename(village_id=village) %>% rename(HHID = q1_1) %>%
  rename(Member_ID = q11_2) 

hh_2 = hh207 %>% 
  inner_join(hh209, by = c("village_id", "HHID", "Member_ID")) %>%
  inner_join(hh211, by = c("village_id", "HHID", "Member_ID"))

#Appending HH phase 1 and 2 

hhdata = bind_rows(hh_1, hh_2)

#___________________________________________________________________________________#

#Merging HH and village data 

final_data = vdata %>% inner_join(hhdata, by="village_id")

final_data %>% count(HHID)


#To check village id counts in HH data: 
check1 = hh107 %>% count(village_id)
print(check1, n=Inf)
check2 = hh207 %>% count(village_id)
print(check2, n=Inf)


