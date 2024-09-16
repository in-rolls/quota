library(haven)
library(dplyr) 

setwd("C:/Users/indir/Dropbox/India Reservations")

df1 <- read_dta("./data/reds/reds06/Sepri1/HH/SECTION01.dta")
df2 <- read_dta("./data/reds/reds06/Sepri2/HH/SECTION01.dta")

combined_df <- bind_rows(df1, df2)

combined_df <- combined_df %>%
  rename(hhid = q1_2,
         hhid14 = q1_1)

combined_df <- combined_df %>%
  distinct(hhid14, .keep_all = TRUE)

combined_df <- combined_df %>%
  rename(stateid = state,
         districtid = district,
         villageid = village,
         hhid99 = q1_3)

combined_df <- combined_df %>%
  arrange(hhid) %>%
  mutate(hhid = ifelse(hhid < 10000,
                       stateid * 10000000000 + districtid * 10000000 + villageid * 10000 + hhid,
                       hhid))

combined_df <- combined_df %>%
  mutate(hhid99 = ifelse(hhid99 < 1000,
                         stateid * 10000000000 + districtid * 10000000 + villageid * 10000 + hhid99,
                         hhid99))
            

summary_hhid99 <- combined_df %>%
  filter(is.na(hhid)) %>%
  summarize(mean_hhid99 = mean(hhid99, na.rm = TRUE),
            sd_hhid99 = sd(hhid99, na.rm = TRUE),
            min_hhid99 = min(hhid99, na.rm = TRUE),
            max_hhid99 = max(hhid99, na.rm = TRUE))

print(summary_hhid99)

duplicates_hhid14 <- combined_df %>%
  arrange(hhid14) %>%
  group_by(hhid14) %>%
  filter(n() > 1) %>%
  ungroup()

print(duplicates_hhid14)



filtered_rows_60451100068 <- combined_df %>%
  filter(hhid14 == 60451100068)

filtered_rows_60411000068 <- combined_df %>%
  filter(hhid14 == 60411000068)

print(filtered_rows_60451100068)
print(filtered_rows_60411000068)

combined_df <- combined_df %>%
  mutate(hhid14 = ifelse(hhid14 == 60451100068 & districtid == 41, 
                         60411000068, 
                         hhid14))

unique_identifier_check <- combined_df %>%
  group_by(hhid14) %>%
  filter(n() > 1)

if (nrow(unique_identifier_check) == 0) {
  print("hhid14 is a unique identifier")
} else {
  print("hhid14 is not a unique identifier")
}

combined_df <- combined_df %>%
  arrange(hhid)

#village wise 

# Recoding hhid values for villageid 71
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50310700021 ~ 50310710021,
    hhid == 50310700046 ~ 50310710046,
    hhid == 50370710164 ~ 50310710164,
    villageid == 71 & is.na(hhid) & head2006 == "BHURA RAWAT" ~ 50310710191,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 72
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 5031072 ~ 50310720164,
    hhid == 50370720009 ~ 50310720009,
    is.na(hhid) & villageid == 72 & head2006 == "ASHOK JAISWAL" ~ 50310720009,
    is.na(hhid) & villageid == 72 & head2006 == "JALIM MANTHAKUR" ~ 50310720163,
    hhid == 50310721720 ~ 50310720172,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 75
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50307500820 ~ 50320750082,
    hhid == 50320760032 ~ 50320750032,
    hhid == 50350750075 ~ 50320750075,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 76
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 5032076059 ~ 50320760059,
    hhid == 30320760383 ~ 50320760383,
    hhid == 50320700360 ~ 50320760360,
    hhid == 50320700369 ~ 50320760369,
    hhid == 150320760084 ~ 50320760084,
    hhid == 50360760877 ~ 50320760877,
    hhid == 53207601680 ~ 50320760168,
    hhid == 58032760166 ~ 50320760166,
    hhid == 70320760183 ~ 50320760183,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 77
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 53307700130 ~ 50330770130,
    hhid == 250330770022 ~ 50330770022,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 78
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50330720115 ~ 50330780115,
    hhid == 50337800005 ~ 50330780005,
    hhid == 50337800680 ~ 50330780068,
    hhid == 53330780140 ~ 50330780140,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 79
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50350790191 ~ 50340790191,
    hhid == 50350790218 ~ 50340790218,
    hhid == 50340791079 ~ 50340790179,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 80
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50300800541 ~ 50340800541,
    hhid == 50310800251 ~ 50340800251,
    hhid == 50340500161 ~ 50340800161,
    hhid == 50640800542 ~ 50340800542,
    hhid == 5034080096  ~ 50340800096,
    hhid == 50340811071 ~ 50340800171,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 81
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50340810205 ~ 50350810205,
    hhid == 50340810253 ~ 50350810253,
    hhid == 50380810178 ~ 50350810178,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 82
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50350800042 ~ 50350820042,
    hhid == 50350320039 ~ 50350820039,
    hhid == 50950820195 ~ 50350820195,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 83
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50380830014 ~ 50350830014,
    is.na(hhid) & villageid == 83 & head2006 == "LALLU CHAUDHARY" ~ 50350830053,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 84
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 53508400157 ~ 50350840157,
    hhid == 150350840146 ~ 50350840146,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 86
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50368600150 ~ 50360860015,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 87
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 26037087049 ~ 60370870498,
    hhid == 30670870856 ~ 60370870856,
    hhid == 60037087116 ~ 60370870116,
    hhid == 60340871519 ~ 60370871519,
    hhid == 60370670022 ~ 60370870022,
    hhid == 60370670266 ~ 60370870266,
    hhid == 60370813310 ~ 60370871331,
    hhid == 603708 ~ 60370871317,
    hhid == 60370870000 & villageid == 87 & head2006 == "KANNAIYA LAL" ~ 60370870275,
    hhid == 70370870173 ~ 60370870173,
    hhid == 160370870020 ~ 60370870020,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 88
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 30670880433 ~ 60370880433,
    hhid == 60308000649 ~ 60370880649,
    hhid == 60370880000 ~ 60370880438,
    hhid == 63070880646 ~ 60370880646,
    hhid == 160370881267 ~ 60370881267,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 89
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50370890110 ~ 60370890110,
    hhid == 60330890356 ~ 60370890356,
    hhid == 60370850019 ~ 60370890019,
    hhid == 60370850597 ~ 60370890597,
    hhid == 60370850688 ~ 60370890688,
    hhid == 60370860052 ~ 60370890052,
    hhid == 60378900145 ~ 60370890145,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 90
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60370090244 ~ 60370900244,
    hhid == 60370200324 ~ 60370900324,
    hhid == 60370500313 ~ 60370900313,
    hhid == 60370500333 ~ 60370900333,
    hhid == 60370800279 ~ 60370900279,
    hhid == 60370990262 ~ 60370900262,
    hhid == 60380900009 ~ 60370900009,
    hhid == 60380900036 ~ 60370900036,
    hhid == 60390900104 ~ 60370900104,
    hhid == 63709000374 ~ 60370900374,
    hhid == 63709100089 ~ 60370900089,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 91
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60308910331 ~ 60380910331,
    hhid == 60308910431 ~ 60380910431,
    hhid == 60380930365 ~ 60380910365,
    hhid == 60380930366 ~ 60380910366,
    hhid == 60380930371 ~ 60380910371,
    hhid == 60389100429 ~ 60380910429,
    hhid == 60980910083 ~ 60380910083,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 92
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60380420092 ~ 60380920092,
    hhid == 60380820082 ~ 60380920082,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 93
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60238093003 ~ 60380930039,
    hhid == 60360930219 ~ 60380930219,
    hhid == 60380390001 ~ 60380930001,
    hhid == 60380390192 ~ 60380930192,
    hhid == 60380430278 ~ 60380930278,
    hhid == 60380530031 ~ 60380930031,
    hhid == 60380530062 ~ 60380930062,
    hhid == 60380920229 ~ 60380930229,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 94
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60309402610 ~ 60380940261,
    hhid == 60370894122 ~ 60380944127,
    hhid == 60370940549 ~ 60380940549,
    hhid == 60380440188 ~ 60380940188,
    hhid == 60380490478 ~ 60380940478,
    hhid == 60380901013 ~ 60380941013,
    hhid == 60980941294 ~ 60380941294,
    hhid == 60980941304 ~ 60380941304,
    hhid == 60980941319 ~ 60380941319,
    hhid == 160380940626 ~ 60380940626,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 95
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 63909500146 ~ 60390950146,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 96
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60390460093 ~ 60390960093,
    hhid == 60390460251 ~ 60390960251,
    hhid == 60390950051 ~ 60390960051,
    hhid == 63390960094 ~ 60390960094,
    hhid == 63909601390 ~ 60390960139,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 97
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60040097043 ~ 60400970043,
    hhid == 60040097174 ~ 60400970174,
    hhid == 60400097095 ~ 60400970095,
    hhid == 60400470124 ~ 60400970124,
    hhid == 60400470126 ~ 60400970126,
    hhid == 60400900059 ~ 60400970059,
    hhid == 60400970000 & head2006 == "MAHAVEER LOHARNAW" ~ 60400970244,
    hhid == 60400971099 ~ 60400970199,
    hhid == 60409070175 ~ 60400970175,
    hhid == 60409700036 ~ 60400970036,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 98
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60400490242 ~ 60400980242,
    hhid == 64000980390 ~ 60400980390,
    hhid == 60900980309 ~ 60400980309,
    hhid == 64009800271 ~ 60400980271,
    hhid == 64009800329 ~ 60400980329,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 99
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60401990136 ~ 60410990136,
    hhid == 60409900133 ~ 60410990133,
    hhid == 60409900158 ~ 60410990158,
    hhid == 60410990846 ~ 60410990246,
    hhid == 60410990910 ~ 60410990091,
    hhid == 60419900215 ~ 60410990215,
    hhid == 64010990051 ~ 60410990051,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 100
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60411002640 ~ 60411000264,
    hhid == 60411060296 ~ 60411000296,
    hhid == 60411100009 ~ 60411000009,
    hhid == 160411000002 ~ 60411000002,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 101
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160721010054 ~ 60421010054,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 102
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60401020631 ~ 60421020631,
    hhid == 60401020644 ~ 60421020644,
    hhid == 60410200251 ~ 60421020251,
    hhid == 60410201820 ~ 60421020187,
    hhid == 60420102331 ~ 60421020331,
    hhid == 60420102333 ~ 60421020333,
    hhid == 60420120590 ~ 60421020590,
    hhid == 60421010258 ~ 60421020258,
    hhid == 60421020 ~ 60421020642,
    hhid == 60421022425 ~ 60421020425,
    hhid == 60421025520 ~ 60421020552,
    hhid == 60421025550 ~ 60421020555,
    hhid == 60421025590 ~ 60421020559,
    hhid == 60421030420 ~ 60421020420,
    hhid == 60421040060 ~ 60421020060,
    hhid == 60421060059 ~ 60421020059,
    hhid == 60422102199 ~ 60421020199,
    hhid == 60481020302 ~ 60421020302,
    hhid == 64210200283 ~ 60421020283,
    hhid == 160421020068 ~ 60421020068,
    hhid == 160421020240 ~ 60421020240,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 103
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60430030075 ~ 60431030075,
    hhid == 604310300110 ~ 60431030011,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 104
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60431041058 ~ 60431040158,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 105
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60991050157 ~ 60441050157,
    hhid == 70441050372 ~ 60441050372,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 106
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60440106182 ~ 60441060182,
    hhid == 60441030313 ~ 60441060313,
    hhid == 60491060271 ~ 60441060271,
    hhid == 60941060270 ~ 60441060270,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 107
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60411070280 ~ 60441070280,
    hhid == 60440070414 ~ 60441070414,
    hhid == 60440070585 ~ 60441070585,
    hhid == 60440170579 ~ 60441070579,
    hhid == 60441071760 ~ 60441070176,
    hhid == 60451070509 ~ 60441070509,
    hhid == 160441070747 ~ 60441070747,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 108
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50441080510 ~ 60441080510,
    hhid == 60440180531 ~ 60441080531,
    hhid == 60441050212 ~ 60441080212,
    hhid == 60441050380 ~ 60441080380,
    hhid == 60441060031 ~ 60441080031,
    hhid == 60441081551 ~ 60441080551,
    hhid == 60441085220 ~ 60441080529,
    hhid == 61441080714 ~ 60441080714,
    hhid == 64410800451 ~ 60441080451,
    hhid == 64410803730 ~ 60441080373,
    hhid == 604410805150 ~ 60441080515,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 109
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60451060440 ~ 60451090440,
    hhid == 60451060720 ~ 60451090720,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 110
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60450011128 ~ 60451100128,
    hhid == 60450110109 ~ NA_real_,  # Setting as missing
    hhid == 60451100001 & head2006 == "BUDH RAM" ~ 60451100004,
    hhid == 60451110036 ~ 60451100036,
    hhid == 60454110006 ~ 60451100060,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 111
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 40461110511 ~ 70461110511,
    hhid == 60461110070 ~ 70461110072,
    hhid == 60461110483 ~ 70461110483,
    hhid == 70401110196 ~ 70461110196,
    hhid == 70401110480 ~ 70461110480,
    hhid == 70451110532 ~ 70461110532,
    hhid == 70461100060 ~ 70461110060,
    hhid == 70461100200 ~ 70461110200,
    hhid == 70461103100 ~ 70461110310,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 112
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 70461100115 ~ 70461120115,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 113
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 10471130041 ~ 70471130041,
    hhid == 40471130253 ~ 70471130253,
    hhid == 70047113080 ~ 70471130080,
    hhid == 70431130007 ~ 70471130007,
    hhid == 70431130051 ~ 70471130051,
    hhid == 170471130012 ~ 70471130012,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 114
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 70411400059 ~ 70471140059,
    hhid == 70471170139 ~ 70471140139,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 115
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 40481150288 ~ 70481150288,
    hhid == 70401150189 ~ 70481150189,
    hhid == 70451150510 ~ 70481150510,
    hhid == 70481115532 ~ 70481150532,
    hhid == 70481130513 ~ 70481150513,
    hhid == 70481155210 ~ 70481150531,
    hhid == 70481180030 ~ 70481150030,
    hhid == 70481180702 ~ 70481150702,
    hhid == 107048115070 ~ 70481150707,
    hhid == 170481150039 ~ 70481150036,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 116
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60481160030 ~ 70481160030,
    hhid == 70481660055 ~ 70481160055,
    hhid == 70481660157 ~ 70481160157,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 117
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 40481170305 ~ 70481170305,
    hhid == 40481171361 ~ 70481171361,
    hhid == 60481170083 ~ 70481170083,
    hhid == 60481170084 ~ 70481170084,
    hhid == 60481170113 ~ 70481170113,
    hhid == 60481170151 ~ 70481170151,
    hhid == 70401170700 ~ 70481170700,
    hhid == 70471170321 ~ 70481170321,
    hhid == 70471180205 ~ 70481170205,
    hhid == 70481117862 ~ 70481170862,
    hhid == 70481176813 ~ 70481170813,
    hhid == 70487110699 ~ 70481170699,
    hhid == 70487110715 ~ 70481170715,
    hhid == 70487110716 ~ 70481170716,
    hhid == 70491170895 ~ 70481170895,
    hhid == 74811170525 ~ 70481170525,
    hhid == 74811711850 ~ 70481171185,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 118
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 40481180344 ~ 70481180344,
    hhid == 40481180491 ~ 70481180491,
    hhid == 40481180492 ~ 70481180492,
    hhid == 40481180526 ~ 70481180526,
    hhid == 40481180595 ~ 70481180595,
    hhid == 40481180596 ~ 70481180596,
    hhid == 40481180622 ~ 70481180622,
    hhid == 40481180624 ~ 70481180624,
    hhid == 40481180630 ~ 70481180630,
    hhid == 40481180635 ~ 70481180635,
    hhid == 40481180726 ~ 70481180726,
    hhid == 47811800217 ~ 70481180217,
    hhid == 60481180538 ~ 70481180538,
    hhid == 70348118004 ~ 70481180004,
    hhid == 70441180335 ~ 70481180335,
    hhid == 70461180324 ~ 70481180324,
    hhid == 70471180276 ~ 70481180276,
    hhid == 70480080589 ~ 70481180589,
    hhid == 70481181528 ~ 70481180528,
    hhid == 70481181723 ~ 70481180723,
    hhid == 70491180310 ~ 70481180310,
    hhid == 170481180191 ~ 70481180191,
    hhid == 170781180343 ~ 70481180343,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 119
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 50791190743 ~ 70491190743,
    hhid == 70421192462 ~ 70491192462,
    hhid == 70451190367 ~ 70491190367,
    hhid == 70451192553 ~ 70491192553,
    hhid == 70481191125 ~ 70491191125,
    hhid == 70481192053 ~ 70491192053,
    hhid == 70490090815 ~ 70491190815,
    hhid == 70491100121 ~ 70491190121,
    hhid == 70491101108 ~ 70491191108,
    hhid == 70491161959 ~ 70491191959,
    hhid == 70491170241 ~ 70491190241,
    hhid == 70491170798 ~ 70491190798,
    hhid == 70491180462 ~ 70491190462,
    hhid == 70491197906 ~ 70491191906,
    hhid == 70941192061 ~ 70491192061,
    hhid == 70991192047 ~ 70491192047,
    hhid == 71491190706 ~ 70491190706,
    hhid == 71491191568 ~ 70491191566,
    hhid == 70491191568 & hhid14 == 70491191414 ~ NA_real_,  # Setting as missing
    hhid == 71491192041 ~ 70491192041,
    hhid == 74911901206 ~ 70491191206,
    hhid == 74911904240 ~ 70491190424,
    hhid == 90491190587 ~ 70491190587,
    hhid == 704911903137 ~ 70491190313,
    hhid == 704911907767 ~ 70491190776,
    hhid == 704911913437 ~ 70491191343,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 120
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 70490120038 ~ 70491200358,
    hhid == 70491203810 ~ 70491200381,
    hhid == 70492000851 ~ 70491200085,
    hhid == 70991200051 ~ 70491200051,
    hhid == 70991200052 ~ 70491200052,
    hhid == 70991200055 ~ 70491200055,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 167
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 107016700361 ~ 110701670036,
    hhid == 110701607405 ~ 110701670405,
    hhid == 110704670387 ~ 110701670387,
    hhid == 110711670095 ~ 110701670095,
    hhid == 110716700083 ~ 110701670083,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 168
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 130791870070 ~ 110711680070,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 169
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 110711690004 & hhid14 == 110711690479 ~ NA_real_,  # Setting as missing
    hhid == 110701690056 ~ 110711690056,
    hhid == 110711069456 ~ 110711690456,
    hhid == 110711390028 ~ 110711690028,
    hhid == 110711691551 ~ 110711690155,
    hhid == 117011690042 ~ 110711690042,
    hhid == 117011690099 ~ 110711690099,
    hhid == 117011690326 ~ 110711690326,
    hhid == 117011690359 ~ 110711690359,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 170
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 11072100106 ~ 110721700106,
    hhid == 110121700106 ~ 110721700106,
    hhid == 110217000284 ~ 110721700284,
    hhid == 110701700123 ~ 110721700123,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 171
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 110721700401 ~ 110721710040,
    hhid == 110721711640 ~ 110721710640,
    hhid == 110721770419 ~ 110721710419,
    hhid == 111072171041 ~ 110721710418,
    hhid == 117021710228 ~ 110721710228,
    hhid == 117201710310 ~ 110721710310,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 185
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 130781800108 ~ 130781850108,
    hhid == 130781815160 ~ 130781850160,
    hhid == 130781820269 ~ 130781850269,
    hhid == 130781858044 ~ 130781850044,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 186
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 13071860393 ~ 130791860393,
    hhid == 130191860492 ~ 130791860492,
    hhid == 130791790551 ~ 130791860551,
    hhid == 130891860341 ~ 130791860341,
    hhid == 130971860080 ~ 130791860080,
    hhid == 310791860319 ~ 130791860319,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 187
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 120791871420 ~ 130791871420,
    hhid == 130191871859 ~ 130791871859,
    hhid == 130781701156 ~ 130791871156,
    hhid == 130781870451 ~ 130791870451,
    hhid == 130781870942 ~ 130791870942,
    hhid == 130781871048 ~ 130791871048,
    hhid == 130781911192 ~ 130791871192,
    hhid == 130791711846 ~ 130791871846,
    hhid == 130791810896 ~ 130791870896,
    hhid == 130791821656 ~ 130791871656,
    hhid == 130791877792 ~ 130791871792,
    hhid == 130797871614 ~ 130791871614,
    hhid == 130798700580 ~ 130791870580,
    hhid == 130798701284 ~ 130791871284,
    hhid == 130798701587 ~ 130791871587,
    hhid == 130798710686 ~ 130791870686,
    hhid == 130971871184 ~ 130791871184,
    hhid == 137918701561 ~ 130791870156,
    hhid == 137918714341 ~ 130791871434,
    hhid == 138791870889 ~ 130791870889,
    hhid == 170791870765 ~ 130791870765,
    hhid == 170791871686 ~ 130791871686,
    hhid == 170791871772 ~ 130791871772,
    hhid == 180791870410 ~ 130791870410,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 188
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 180801880123 ~ 130801880123,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 190
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 110811900487 ~ 140811900487,
    hhid == 140819001001 ~ 140811900100,
    TRUE ~ hhid
  ))

# Recoding hhid values for villageid 191
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 110811910050 ~ 140811910050,
    hhid == 140810910088 ~ 140811910088,
    hhid == 144081191010 ~ 140811910105,
    TRUE ~ hhid
  ))


# Handle the recoding and transformations for villageid 192
combined_df <- combined_df %>%
  mutate(
    hhid = case_when(
      hhid == 140819200 ~ NA_real_,
      hhid == 140811920371 ~ 140811920037,
      TRUE ~ hhid
    ),
    str_newid = as.character(hhid),
    newint = as.numeric(substr(str_newid, nchar(str_newid) - 3, nchar(str_newid))),
    newid = stateid * 10000000000 + districtid * 10000000 + villageid * 10000 + newint
  ) %>%
  mutate(
    newid = if_else(villageid == 192, newid, NA_real_),
    hhid = if_else(villageid == 192, newid, hhid)
  ) %>%
  select(-newid, -newint, -str_newid) %>%
  arrange(hhid)


# village 193
combined_df <- combined_df %>%
    mutate(
    str_newid = as.character(hhid), 
    newint = as.numeric(substr(str_newid, nchar(str_newid) - 3, nchar(str_newid))), 
    newid = stateid * 10000000000 + districtid * 10000000 + villageid * 10000 + newint 
  ) %>%
  
  # Update newid based on villageid condition
  mutate(
    newid = if_else(villageid != 193, NA_real_, newid),
    hhid = if_else(villageid == 193, newid, hhid)
  ) %>%
  
  # Drop intermediate columns
  select(-newid, -newint, -str_newid) %>%
  
  # Arrange by hhid
  arrange(hhid)

# Recoding for village 194
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 140821040112 ~ 140821940112,
    hhid == 140821240051 ~ 140821940051,
    hhid == 140951940016 ~ 140821940016,
    hhid == 140951940017 ~ 140821940017,
    hhid == 140951940018 ~ 140821940018,
    TRUE ~ hhid
  ))

# Recoding for village 195
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 140182195033 ~ 140821950330,
    hhid == 140521950057 ~ 140821950057,
    hhid == 140811950088 ~ 140821950088,
    hhid == 140821950954 ~ 140821950254,
    hhid == 140821960273 ~ 140821950273,
    hhid == 140824950241 ~ 140821950241,
    hhid == 140891950375 ~ 140821950375,
    hhid == 140891950412 ~ 140821950412,
    hhid == 170821950084 ~ 140821950084,
    hhid == 190821950078 ~ 140821950078,
    hhid == 190821950444 ~ 140821950444,
    TRUE ~ hhid
  ))

# Recoding for village 196
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 140821940194 ~ 140821960194,
    hhid == 140921960096 ~ 140821960096,
    TRUE ~ hhid
  ))

# Recoding for village 197
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 14083190481 ~ 140831970481,
    hhid == 40831970399 ~ 140831970399,
    hhid == 140231970373 ~ 140831970373,
    hhid == 140811970245 ~ 140831970245,
    hhid == 140821970422 ~ 140831970422,
    hhid == 140830970151 ~ 140831970151,
    hhid == 140831070362 ~ 140831970362,
    hhid == 140831270225 ~ 140831970225,
    hhid == 140831470465 ~ 140831970465,
    hhid == 140831900036 ~ 140831970036,
    hhid == 140831900380 ~ 140831970380,
    hhid == 140831970634 ~ 140831970364,
    hhid == 140831971157 ~ 140831971157,
    hhid == 140831976413 ~ 140831970413,
    hhid == 140833970151 ~ 140831970151,
    hhid == 140837970523 ~ 140831970523,
    hhid == 170831970302 ~ 140831970302,
    TRUE ~ hhid
  ))

# Recoding for village 198
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 140381980199 ~ 140831980199,
    hhid == 140830980312 ~ 140831980312,
    hhid == 140830980361 ~ 140831980361,
    hhid == 140831198006 ~ 140831980006,
    hhid == 140831198008 ~ 140831980008,
    hhid == 140831398017 ~ 140831980179,
    hhid == 140831920332 ~ 140831980332,
    hhid == 140831970016 ~ 140831980016,
    hhid == 140831970149 ~ 140831980149,
    hhid == 140831970192 ~ 140831980192,
    hhid == 140831970298 ~ 140831980298,
    hhid == 140831970316 ~ 140831980316,
    hhid == 140831970405 ~ 140831980405,
    hhid == 140831980766 ~ 140831980166,
    hhid == 140831980801 ~ 140831980301,
    hhid == 140831981068 ~ 140831980168,
    hhid == 140881980147 ~ 140831980147,
    hhid == 140891980007 ~ 140831980007,
    hhid == 170831980417 ~ 140831980417,
    TRUE ~ hhid
  ))


# Recoding for village 210
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160592100050 ~ 160892100050,
    hhid == 160792100034 ~ 160892100034,
    hhid == 160862100063 ~ 160892100063,
    hhid == 160890100179 ~ 160892100179,
    hhid == 160892160236 ~ 160892100236,
    TRUE ~ hhid
  ))

# Recoding for village 211
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 108921700101 ~ 160892110101,
    hhid == 160382110344 ~ 160892110344,
    hhid == 160392110133 ~ 160892110133,
    hhid == 160872110138 ~ 160892110138,
    hhid == 160872110139 ~ 160892110139,
    hhid == 160872110141 ~ 160892110141,
    hhid == 160890110089 ~ 160892110089,
    hhid == 160892100274 ~ 160892110274,
    hhid == 160892511001 ~ 160892110001,
    hhid == 160982110343 ~ 160892110343,
    hhid == 168921104181 ~ 160892110418,
    TRUE ~ hhid
  ))

# Recoding for village 212
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 120902125525 ~ 160902125525,
    hhid == 140902125892 ~ 160902125892,
    hhid == 160190212332 ~ 160902123329,
    hhid == 160302124571 ~ 160902124571,
    hhid == 160502121023 ~ 160902121023,
    hhid == 160900126041 ~ 160902126041,
    hhid == 160900212604 ~ 160902126045,
    hhid == 160901210142 ~ 160902120142,
    hhid == 160901255621 ~ 160902125562,
    hhid == 160902100135 ~ 160902120135,
    hhid == 160902112194 ~ 160902120134,
    hhid == 160902127609 ~ 160902122609,
    hhid == 160902129886 ~ 160902123886,
    hhid == 160902129896 ~ 160902123896,
    hhid == 160902129941 ~ 160902122994,
    hhid == 160902140158 ~ 160902120158,
    hhid == 160902140159 ~ 160902120159,
    hhid == 160902140160 ~ 160902120160,
    hhid == 160902140226 ~ 160902120226,
    hhid == 160902140228 ~ 160902120228,
    hhid == 160902140230 ~ 160902120230,
    hhid == 160902140266 ~ 160902120266,
    hhid == 160902140384 ~ 160902120384,
    hhid == 160902140385 ~ 160902120385,
    hhid == 160902140418 ~ 160902120418,
    hhid == 160902163736 ~ 160902123736,
    hhid == 160902164009 ~ 160902124009,
    hhid == 160902175587 ~ 160902125587,
    hhid == 160905212456 ~ NA_real_,
    hhid == 160912123484 ~ 160902123484,
    hhid == 160912123485 ~ 160902123485,
    hhid == 160912123489 ~ 160902123489,
    hhid == 160912123491 ~ 160902123491,
    hhid == 160912123505 ~ 160902123505,
    hhid == 160912123507 ~ 160902123507,
    hhid == 160912123710 ~ 160902123710,
    hhid == 160912123711 ~ 160902123711,
    hhid == 160912123748 ~ 160902123748,
    hhid == 160912123789 ~ 160902123789,
    hhid == 160912123791 ~ 160902123791,
    hhid == 160920125014 ~ 160902125014,
    hhid == 160921200195 ~ 160902120195,
    hhid == 160962123906 ~ 160902123906,
    hhid == 160902122690 ~ 160902122698,
    hhid == 162090212269 ~ 160902122690,
    hhid == 169025120420 ~ 160902120420,
    hhid == 610902122699 ~ 160902122699,
    hhid == 610902122712 ~ 160902122712,
    TRUE ~ hhid
  ))

# Recoding for village 213
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160903130809 ~ 160902130809,
    hhid == 168902131015 ~ 160902131015,
    hhid == 160902131991 ~ 160902130991,
    TRUE ~ hhid
  ))

# Recoding for village 214
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 16090240282 ~ 160902140282,
    hhid == 160902149141 ~ 160902140914,
    hhid == 160902150053 ~ 160902140053,
    hhid == 160902400199 ~ 160902140199,
    TRUE ~ hhid
  ))

# Recoding for village 215
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 16090215590 ~ 160902150590,
    hhid == 16090215592 ~ 160902150592,
    hhid == 16090215649 ~ 160902150649,
    hhid == 60902150032 ~ 160902150032,
    hhid == 60902150295 ~ 160902150295,
    hhid == 60902150587 ~ 160902150587,
    hhid == 100902150106 ~ 160902150106,
    hhid == 160202150109 ~ 160902150109,
    hhid == 160702150326 ~ 160902150326,
    hhid == 160802150232 ~ 160902150232,
    hhid == 160900215012 ~ 160902150012,
    hhid == 160901250488 ~ 160902150488,
    hhid == 160901250695 ~ 160902150695,
    hhid == 160902115514 ~ 160902150514,
    hhid == 160902140706 ~ 160902150706,
    hhid == 160902155321 ~ 160902150321,
    hhid == 160912150344 ~ 160902150344,
  ))
    
    
# Recoding for village 217
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160892170046 ~ 160912170046,
    hhid == 160892170145 ~ 160912170145,
    hhid == 160912110006 ~ 160912170006,
    TRUE ~ hhid
  ))

# Recoding for village 219
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 60922190162 ~ 160922190162,
    hhid == 60922190183 ~ 160922190183,
    hhid == 60922190188 ~ 160922190188,
    hhid == 160921900024 ~ 160922190024,
    TRUE ~ hhid
  ))

# Recoding for village 220
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160932200881 ~ 160932200088,
    hhid == 160932210070 ~ 160932200070,
    hhid == 160932210077 ~ 160932200077,
    hhid == 160933220414 ~ 160932200414,
    hhid == 169322000245 ~ 160932200245,
    TRUE ~ hhid
  ))

# Recoding for village 223
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160442230075 ~ 160942230075,
    hhid == 160742230018 ~ 160942230018,
    hhid == 160922230053 ~ 160942230053,
    hhid == 160942223027 ~ 160942230027,
    TRUE ~ hhid
  ))

# Recoding for village 224
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160442240094 ~ 160942240094,
    hhid == 160642240331 ~ 160942240331,
    hhid == 160940240314 ~ 160942240314,
    hhid == 160942250010 ~ 160942240010,
    hhid == 160942250014 ~ 160942240014,
    hhid == 160942250109 ~ 160942240109,
    hhid == 190942240379 ~ 160942240379,
    TRUE ~ hhid
  ))

# Recoding for village 225
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 160342250032 ~ 160942250032,
    hhid == 160342250375 ~ 160942250375,
    hhid == 160940250232 ~ 160942250232,
    hhid == 160942200185 ~ 160942250185,
    hhid == 160942550018 ~ 160942250018,
    TRUE ~ hhid
  ))

# Recoding for village 226
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 170952300124 ~ 170952260124,
    hhid == 170975260187 ~ 170952260187,
    TRUE ~ hhid
  ))

# Recoding for village 227
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 1709522700 ~ 170952270029,
    hhid == 70952270091 ~ 170952270091,
    hhid == 130952270392 ~ 170952270392,
    hhid == 170592270062 ~ 170952270062,
    hhid == 170652270388 ~ 170952270388,
    hhid == 170952290396 ~ 170952270396,
    hhid == 170952710110 ~ 170952270110,
    hhid == 170955270194 ~ 170952270194,
    hhid == 170972270089 ~ 170952270089,
    hhid == 170972270147 ~ 170952270147,
    hhid == 190952270083 ~ 170952270083,
    TRUE ~ hhid
  ))

# Recoding for village 228
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 170462280139 ~ 170962280139,
    hhid == 170962880185 ~ 170962280185,
    hhid == 170992280157 ~ 170962280157,
    hhid == 170992280171 ~ 170962280171,
    hhid == 190962280250 ~ 170962280250,
    hhid == 170962280014 ~ 170962280015,
    hhid == 170962280163 ~ 170962280182,
    hhid == 170962280162 ~ 170962280163,
    TRUE ~ hhid
  ))

# Recoding for village 229
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 170692290603 ~ 170962290603,
    hhid == 170902290481 ~ 170962290481,
    hhid == 170960229002 ~ 170962290002,
    hhid == 170962270660 ~ 170962290660,
    hhid == 170962280269 ~ 170962290269,
    hhid == 170962280706 ~ 170962290706,
    hhid == 170962296265 ~ NA_real_,
    hhid == 170962990017 ~ 170962290017,
    hhid == 170962990376 ~ NA_real_,
    hhid == 170962990545 ~ 170962290545,
    hhid == 170962990637 ~ 170962290637,
    hhid == 170962990638 ~ 170962290608,
    hhid == 190962290312 ~ NA_real_,
    TRUE ~ hhid
  ))

# Recoding for village 230
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 130972300024 ~ 170972300024,
    hhid == 140972300008 ~ 170972300008,
    hhid == 170972310056 ~ 170972300368,
    hhid == 170972310064 ~ 170972300064,
    hhid == 170982300260 ~ 170972300260,
    hhid == 171972300195 ~ 170972300195,
    hhid == 171972300197 ~ 170972300197,
    hhid == 171972300200 ~ 170972300200,
    hhid == 171972300202 ~ 170972300202,
    hhid == 179072300011 ~ 170972300011,
    hhid == 179097230043 ~ 170972300430,
    TRUE ~ hhid
  ))

# Recoding for village 231
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 17092310281 ~ 170972310281,
    hhid == 170792310127 ~ 170972310127,
    hhid == 170970310045 ~ 170972310045,
    hhid == 170972300076 ~ 170972310076,
    TRUE ~ hhid
  ))

# Recoding for village 232
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 140972320179 ~ 170972320179,
    hhid == 140972321764 ~ 170972320764,
    hhid == 170072320688 ~ NA_real_,
    hhid == 170972130037 ~ 170972320037,
    hhid == 170972305391 ~ 170972320539,
    hhid == 170972320931 ~ 170972320531,
    hhid == 170973230850 ~ 170972320850,
    hhid == 170982320676 ~ 170972320676,
    TRUE ~ hhid
  ))

# Recoding for village 233
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 70982330151 ~ 170982330151,
    hhid == 170482330373 ~ 170982330373,
    hhid == 170982331046 ~ 170982330146,
    hhid == 170982830345 ~ 170982330345,
    TRUE ~ hhid
  ))

# Recoding for village 234
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 170492340359 ~ NA_real_,
    hhid == 170992340721 ~ 170992340721,
    hhid == 170992346108 ~ 170992340108,
    hhid == 170992370467 ~ 170992340467,
    hhid == 170992404681 ~ 170992340468,
    hhid == 170993404731 ~ NA_real_,
    TRUE ~ hhid
  ))

# Recoding for village 235
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 17100235094 ~ 171002350094,
    hhid == 170023501671 ~ 171002350167,
    hhid == 171002351161 ~ 171002350116,
    hhid == 171002530422 ~ 171002350422,
    TRUE ~ hhid
  ))

# Recoding for village 236
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 171011236040 ~ 171012360040,
    hhid == 171011260051 ~ 171012360051,
    hhid == 171062360249 ~ 171012360249,
    TRUE ~ hhid
  ))

# Recoding for village 237
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 171012370163 ~ 171022370163,
    hhid == 171012370175 ~ 171022370175,
    hhid == 171022310195 ~ 171022370195,
    hhid == 171022320141 ~ 171022370141,
    hhid == 171022320142 ~ 171022370142,
    hhid == 171022390361 ~ 171022370361,
    TRUE ~ hhid
  ))

# Recoding for village 238
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 171022280035 ~ NA_real_,
    hhid == 171022360064 ~ 171022380064,
    hhid == 171022381025 ~ 171022380035,
    hhid == 171022390010 ~ 171022380010,
    hhid == 171023380554 ~ 171022380554,
    TRUE ~ hhid
  ))

# Recoding for village 239
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 147103239038 ~ NA_real_,
    hhid == 171022390314 ~ 171032390314,
    hhid == 171022390325 ~ 171032390325,
    hhid == 171031080015 ~ 171032390015,
    hhid == 171032340542 ~ 171032390542,
    hhid == 171032360576 ~ NA_real_,
    TRUE ~ hhid
  ))

# Recoding for village 240
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 171022400494 ~ NA_real_,
    hhid == 171031590285 ~ 171032400285,
    hhid == 171032404031 ~ 171032400403,
    hhid == 171103240497 ~ NA_real_,
    hhid == 171302400332 ~ 171032400332,
    hhid == 171324002131 ~ 171032400215,
    hhid == 171324004451 ~ 171032400445,
    hhid == 173032400396 ~ 171032400641,
    TRUE ~ hhid
  ))

# Recoding for village 241
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 110424105301 ~ 171042410530,
    hhid == 121042410672 ~ 171042410672,
    hhid == 170042401174 ~ 171042410174,
    hhid == 170104241028 ~ 171042410286,
    hhid == 170942410162 ~ 171042410162,
    hhid == 171014241051 ~ 171042410515,
    hhid == 171024100634 ~ 171042410634,
    hhid == 171024210266 ~ 171042410266,
    hhid == 171024710314 ~ 171042410314,
    hhid == 171040410797 ~ 171042410797,
    hhid == 171042410981 ~ 171042410918,
    hhid == 171042418393 ~ 171042410393,
    hhid == 171042420136 ~ 171042410136,
    hhid == 171042910305 ~ 171042410305,
    hhid == 171042910391 ~ 171042410391,
    hhid == 171072410931 ~ 171042410931,
    TRUE ~ hhid
  ))

# Recoding for village 242
combined_df <- combined_df %>%
  mutate(hhid = case_when(
    hhid == 171024240207 ~ 171042420207,
    hhid == 171024240240 ~ NA_real_,
    hhid == 171042400013 ~ 171042420013,
    hhid == 171042400044 ~ 171042420044,
    hhid == 171042401981 ~ 171042420198,
    hhid == 171042410245 ~ 171042420246,
    hhid == 171042410255 ~ 171042420255,
    hhid == 171042421044 ~ 171042420144,
    hhid == 171042421046 ~ 171042420146,
    hhid == 171042421047 ~ 171042420147,
    hhid == 171072120227 ~ 171042420223,
    hhid == 171142420136 ~ 171042420136,
    hhid == 171423420219 ~ 171042420214,
    TRUE ~ hhid
  ))

# Drop columns that start with 'q'
combined_df <- combined_df %>%
  select(-starts_with("q"))

# Rename columns 'hhid' to 'hhid06' and 'hhid14' to 'hhid'
combined_df <- combined_df %>%
  rename(hhid06 = hhid,  # Renaming 'hhid' to 'hhid06'
         hhid = hhid14)   # Renaming 'hhid14' to 'hhid'

saveRDS(data, file = "./scripts/Indira_REDS/Deininger et al 2020- replication/Replication_Deininger et al/Clean_data/ID_file_2014.rds")

