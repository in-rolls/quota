library(haven)
library(dplyr)
library(rmarkdown)
library(knitr)
library(kableExtra)

# setwd("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06")
# ###############################################################################################################
 df1 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/HH/SECTION11.dta")
df1 = df1[df1$state==6,]
 
 df2 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/HH/Village_HH_Match_Code.dta")
# 
 df3 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\HH\\SECTION01.dta")
 df3 = df3[df3$state==6,]
# ####################################################################################################
# df4 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri2/HH/SECTION06A.dta")
# 
# df5 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri2/HH/SECTION06A.dta")
# 
# df6 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri2/Village/VILLAGE_DETAIL.dta")
# ##################################################################################################################
# # df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_02_NOW.dta")
# df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\VILLAGE_DETAIL_UP.dta")
# 
# df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_17.dta")
# 
# df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_6.dta")
# 
# df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_7.dta")
# 
# df7 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_9.dta")

df11c <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_C.dta")
df11d <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_D.dta")
df11d2 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_D_2.dta")
df11d3 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_D_3NOW.dta")
df11_6 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\SECTION_11_6.dta")
df_id = read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\Village\\VILLAGE_DETAIL.dta")

df = full_join(df_id, df11c )
df = full_join(df, df11d)
df = full_join(df,  df11d2)
df = full_join(df,  df11d3)
df = full_join(df,  df11_6)

final = df[df$q1_1h == "RAJASTHAN",]
table(final$s11_d1_p35_q03)
table(final$s11_d1_p35_q07)
table(is.na(final$s11_d1_p35_q07))
library(purrr)
n <- ncol(final)
labels_list <- map(1:n, function(x) attr(final[[x]], "label") )
variable_name <- names(final)
labels_vector <- map_chr(1:n, function(x) attr(final[[x]], "label") )
desc_final=data.frame(variable_name, description = labels_vector) 


sub_df1 = final %>% select(	village_name,tehsil_taluka,q1_1a, gram_panchayat, cd_block, district, 	
                            s11_d1_p35_q03,s11_d1_p35_q07, 	
                            s11_d2_p36_q07, s11_d2_p36_q11, s11_6_p39_q01,s11_6_p39_q03)
sub_df1 = unique(sub_df1)

write.csv(sub_df1, file = "clean_reds.csv", row.names = FALSE)

sub_df1 = read.csv("clean_reds.csv")

sub_df1$s11_6_p39_q01 = as.factor(as.character(sub_df1$s11_6_p39_q01))
str(sub_df1)
sub_df1$priority <- factor(sub_df1$s11_6_p39_q01,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                          labels = c("Water", "Sanitation", "Roads",
                                     "Irrigation", "Transport", "Electric",
                                     "Street_lig", "Subsidies", "Communication",
                                     "School", "Health", "Resource",
                                     "Unemployment", "Employ_scheme"))
sub_df1$s11_d1_p35_q07 = as.factor(as.character(sub_df1$s11_d1_p35_q07))
sub_df1$reservation = ifelse(sub_df1$s11_d1_p35_q07 == "1", "Woman", "Man")
sub_df1$rank_value = 1/sub_df1$s11_6_p39_q03 

output = sub_df1 %>%group_by(reservation, priority) %>% summarise(value_priority = mean(s11_6_p39_q03))%>% spread(reservation, value_priority)

temp = sub_df1[,1:8]
temp = unique(temp) 
sub_df1.05 = sub_df1[sub_df1$s11_d1_p35_q03 == "2005",]
sub_df1.10 = sub_df1[sub_df1$s11_d1_p35_q03 == "2010",]

sub_df1 = final %>% select(	village_name,tehsil_taluka,q1_1a, gram_panchayat, cd_block, district, 	
                            s11_d2_p36_q04, s11_d2_p36_q13, s11_d2_p36_q14, s11_d1_p35_q03,s11_d1_p35_q07)
sub_df1 = unique(sub_df1)
# df7 = df7[df7$state == "RAJASTHAN",]
# SECTION_11_D_2
# SECTION_11_9
# SECTION_11_C
# SECTION_11_D
# SECTION_11_D_3NOW
df8 <- read_dta("C:\\Users\\manus\\Dropbox (Personal)\\India Reservations\\data\\reds\\reds06\\Sepri1\\HH\\SECTION11.dta")
df8 = df8[df8$state==6,]

b = as.data.frame(table(df7$district))
table(is.na(df8$q11_8))

length(table(df8$village))
#24
name = as.data.frame(table(df8$head2006))
a = as.data.frame(colnames(df8))


######################################################################################
## Making tabulation of women's attendance
######################################################################################

setwd("C:/Users/indir/Dropbox/India Reservations/data/reds/reds06/Sepri1")

#df1 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/HH/SECTION11.dta")
df1 = read_dta("./HH/SECTION11.dta")

df1 = df1[df1$state==6,]
colnames(df1)[9] = "name_respondent"

#df2 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/HH/SECTION07.dta")
df2 = read_dta("./HH/SECTION07.dta")

df2 = df2[df2$state==6,]
colnames(df2)[9] = "name_respondent"

#df3 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/Village/SECTION_11_D.dta")
df3 = read_dta("./Village/SECTION_11_D.dta")

df3 = as.data.frame(df3)
colnames(df3)[1] = "villagecode"
head(df3)
df3 = df3[df3$s11_d1_p35_q01 == 1,]
df3 = na.omit(df3)

# df4 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/Village/VILLAGE_DETAIL.dta")

#df5 <- read_dta("C:/Users/manus/Dropbox (Personal)/India Reservations/data/reds/reds06/Sepri1/HH/Village_HH_Match_Code.dta")
df5 = read_dta("./HH/Village_HH_Match_Code.dta")


total = left_join(df1, df2, by = c("q1_1", "village", "name_respondent"))
colnames(total)[6] = "villagecode"
total = left_join(total, df5)
total = as.data.frame(total)
colnames(total)[76] = "village_name"
total = left_join(total, df3, c( "village_name"))
head(total)

## Making summary statistics 

#note: q11_3 is: how many GP meetings did you attend in the past 12 months
table(total$q11_3, useNA ="ifany")

total$attend_meeting = ifelse(total$q11_3 == 0, 0, 1)
total$attend_meeting_na = ifelse(is.na(total$q11_3), 0, 1)


total = total %>% rename(reservation = s11_d1_p35_q07) %>%
     rename(gender = q7_3) 

total = total %>% mutate(q11_4 = as.character(q11_4)) %>%
     mutate(q11_4 = recode(q11_4, 
            `1` = "not know ab meeting in time",
            `2` = "issue not imp. to me",
            `3` = "no influence anyway",
            `4` = "don't understand issue", 
            `5` = "feel embarrassed/shy",
            `6` = "bad experience in past",
            `7` = "sickness",
            `8` = "old age",
            `9` = "lack privacy for women",
            `10`= "incovenient meeting place",
            `11` = "have to sit w men",
            `12` = "have to sit w women",
            `13` = "have to sit w other castes/religions",
            `14` = "dislike current pradhan",
            `15` = "dislike current GP members",
            `16` = "not allowed by head of HH",
            `17` = "pradhan is diff caste",
            `18` = "no GP meetings held"
            ))


final_tab1 = total %>% group_by(reservation ,gender ) %>% summarize(sum_vals = sum(attend_meeting, na.rm = TRUE))
final_tab1

# final_tab = total %>% group_by(s11_d1_p35_q07 ,q7_3 ) %>% summarize( sum_vals = sum(attend_meeting_na, na.rm = TRUE))
# final_tab
# 
# table(total$q11_3)


final_tab1 = na.omit(final_tab1)
final_tab1

327/(327+1385) #=~19%

619/(619+2503) #=~19%

#note: q11_4 == reasons for not attending the meeting 

final_tab2 = total %>% group_by(reservation,gender, q11_4 ) %>% summarize(n())
final_tab2 

table1 = as.data.frame(table(total$q11_4))

table2 = as.data.frame(table(total$attend_meeting_na))

final_tab3 = na.omit(total %>% group_by(reservation ,gender ) %>% summarize(sum_vals = sum(attend_meeting_na, na.rm = TRUE)))

final_tab4 = cbind(final_tab3, final_tab1)


#Creating table for paper 

final_table = na.omit(total %>% group_by(reservation, gender) %>%
                           summarize(Count_attended=sum(attend_meeting, na.rm=TRUE),
                                     Total = n())
                           )


final_table = final_table %>% 
     mutate(reservation = recode(reservation, 
                                     `1` = "Reserved",
                                     `2`= "Not reserved")) %>%
     mutate(gender = as.character(gender)) %>%
     mutate(gender = recode(gender, 
                                `1` = "Male",
                                `2` = "Female")) %>%
     rename(Status = reservation) %>%
     rename(Attendee_Gender = gender) 



#Generate tex file for the table 

setwd("C:/Users/indir/Dropbox/India Reservations/scripts/Indira_REDS/Meeting attendance table_REDS")

sink("meeting_attendance.tex")

kable(final_table, format = "latex", booktabs = TRUE, caption = "Insert caption here") %>%
     cat()

sink()






     
     
     
     
     
#notes: attend_meeting_na = 1 if respondent has answered, whether that answer is zero or more
#attend_meeting = 1 if respondent has answered >1 and 0 otherwise. 
     
     
