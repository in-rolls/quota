library(haven)
library(tidyverse)

setwd("C:/Users/indir/Dropbox/India Reservations")

village1 = read_dta("./data/reds/reds06/Sepri1/Village/VILLAGE_DETAIL.dta")

village2 = read_dta("./data/reds/reds06/Sepri2/Village/VILLAGE_DETAIL.dta")

unique(village1$gram_panchayat)

unique(village2$gram_panchayat)

unique(village1$q1_1h)

unique(village2$q1_1h)

num1 = village1 %>% count(village_sr_no)

num2 = village2 %>% count(village_sr_no)

unique(village2$village_sr_no)

check1 = village1 %>% count(q1_1h)

check2 = village2 %>% count(q1_1h)

hhph1 = read_dta("./reds06/Sepri1/HH/SECTION01.dta")

hhph2 = read_dta("./reds06/Sepri2/HH/SECTION01.dta")

hhph1 %>% count(panchayat)

hhph2 %>% count(panchayat)

hh1 = hhph1 %>% count(panchayat)

hh2 = hhph2 %>% count(panchayat)

range(hh1$n)
mean(hh1$n)
sd(hh1$n)

range(hh2$n)
mean(hh2$n)
sd(hh2$n)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Exploring data on meetings question 

hhq11 = read_dta("./reds06/Sepri1/HH/SECTION11.dta")

#PROBLEM ! GENDER IS NOT THERE HERE; WILL NEED TO MERGE IN USING HH and MEMBER ID FROM Q7 

hhq11 %>% count(q11_3) 
unique(hhq11$q11_3)

hhq11 %>% count(q11_4) 
hhq11 %>% count(q11_5) 




