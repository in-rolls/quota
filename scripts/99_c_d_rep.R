## Read in the data

library(foreign)

## Bengal
## n = 166
partb <- read.dta("data/dataverse_files/womenpolicymakers_partb.dta")
# Given n of 166 as above, this is likely Bengal
parta <- read.dta("data/dataverse_files/womenpolicymakers_parta.dta") 
# Village level dataset for Bengal
partd <- read.dta("data/dataverse_files/womenpolicymakers_partd.dta")
# Given same number of observations as partd + Village ID/GP ID, 
# it is likely Bengal
partc <- read.dta("data/dataverse_files/womenpolicymakers_partc.dta")
# Two re?-surveys of Bengal 
re_a <- read.dta("data/dataverse_files/womenpolicymakers_resurveya.dta")
re_d <- read.dta("data/dataverse_files/womenpolicymakers_resurveyd.dta")
