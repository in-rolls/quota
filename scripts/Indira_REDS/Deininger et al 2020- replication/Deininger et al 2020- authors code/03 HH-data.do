clear
clear matrix

global reds "C:\Users\Dropbox\SEPRI"

global hsepri1 $reds\sepri1\hh
global hsepri2 $reds\sepri2\hh
global vsepri1 $reds\sepri1\village
global vsepri2 $reds\sepri2\village

*****************************************************
global path "C:\Users\Dropbox\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output


*===============================================================================



**Household's characteristics
use $hsepri1\SECTION01, clear
append using $hsepri2\SECTION01
ren (q1_4 q1_4a) (headname headfarther_name)

ren q1_1 hhid
ren q1_2 listingid2006
ren  q1_6a headoccup
ren q1_7 jati
ren q1_8 caste
ren q1_9 religion
ren q1_10 land
ren q1_10a irri_land
ren q1_10h unirri_land
ren q1_18 jobcard
ren q1_19 manyjobcard
egen jobcardyear=rowmin(q1_20e1_1 q1_20e1_2 q1_20e1_3 q1_20e1_4 q1_20e2_1 q1_20e2_2 q1_20e2_3 q1_20e2_4)
label var jobcardyear "Jobcard Issue/Renewed year"
replace religion=3 if religion>3
ren q1_21 nregwk_nplot
ren q1_22 nregwk_oplot
drop q*
save $wkdata\hh_characteristics, replace


**Head's Characteristics
use $hsepri1\SECTION07, clear
append using $hsepri2\SECTION07
ren q1_1 hhid
ren q7_3 hsex
ren q7_6 hage
ren q7_8 hmarried
ren q7_16 hedu
keep if q7_1==1

gen hoccp=1 if q7_24==1
replace hoccp=2 if q7_26==1
replace hoccp=3 if q7_28==1| q7_31==1
replace hoccp=4  if q7_34==1
replace hoccp=5  if q7_37 ==1
replace hoccp=6  if  hoccp==.
label def hoccp 1"self employed agri" 2"self employed non agri" 3"Casual labor" 4"Mnrega worker" 5"Salary" 6"Others"
label val hoccp hoccp

keep hhid hsex hage hmarried hedu hoccp
save $wkdata\head_char, replace


*Household variables
use $hsepri1\SECTION07, clear
append using $hsepri2\SECTION07
ren q1_1 hhid
gen x=1
gen hhsize=x
gen wkmale=x if q7_6>=14 & q7_6<=65 & q7_3==1
gen wkfemale=x if q7_6>=14 & q7_6<=65 & q7_3==2
gen children=x if q7_6 <14
gen mchildren=x if q7_6 <14 & q7_3==1
gen fchildren=x if q7_6 <14 & q7_3==2
gen Mmen=x if  q7_3==1 & q7_8==1 & q7_6>=14
gen Umen=x if  q7_3==1 & q7_8==2 & q7_6>=14
gen Wmen=x if  q7_3==1 & q7_8==3 & q7_6>=14
gen Mwomen=x if  q7_3==2 & q7_8==1 & q7_6>=14
gen Uwomen=x if  q7_3==2 & q7_8==2 & q7_6>=14
gen Wwomen=x if  q7_3==2 & q7_8==3 & q7_6>=14
collapse (sum)  hhsize wkmale wkfemale children mchildren fchildren Mmen Umen Wmen Mwomen Uwomen Wwomen, by(hhid)
label var Mmen "Married men in the HH"
label var Umen "UnMarried men in the HH"
label var Wmen "Wideowed/Seperated men in the HH"
label var Mwomen "Married women in the HH"
label var Uwomen "UnMarried women in the HH"
label var Wwomen "Wideowed/Seperated women in the HH"
save $wkdata\hh_info, replace

***Household's composition (Mean, Max and Variance of age and education)
use $hsepri1\SECTION07, clear
append using $hsepri2\SECTION07
ren q1_1 hhid
gen x=1
bysort  hhid: egen  max_eduhh=max(q7_16)
bysort  hhid: egen  mean_eduhh=mean(q7_16)
gen     deviation_edu= q7_16- mean_eduhh
replace  deviation_edu=0 if  deviation_edu==.
gen  sqrdev_edu= deviation_edu* deviation_edu
bysort hhid: egen var_eduhh=mean( sqrdev_edu)
bysort hhid: egen mean_agehh=mean(q7_6)
drop  deviation_edu sqrdev_edu x 
bysort hhid: keep if _n==1
keep hhid max_eduhh mean_eduhh var_eduhh mean_agehh
for var *: replace X=0 if X==.
label var max_eduhh "Max education in the HH"
label var mean_eduhh  "Mean education in the HH"
label var var_eduhh "Variance in education in the HH"
label var mean_agehh "Mean ahe of the HH"
sort hhid
save  $wkdata\hh_comp,replace

//Clean the conusmtion data. Ask Anupam to check the entry and veryfy it with hard copy of queationnaire

*===============================================================================
*Consumption expenditure food items
*===============================================================================
use $hsepri1\SECTION06A, clear
append using $hsepri2\SECTION06A
ren q1_1 hhid
ren q6a1 item
egen  consexp1=rsum(q6a7 q6a5)
gen cereals=consexp1       if item==1 |item==2|item==3|item==4|item==5|item==6
gen pulses=consexp1        if item==7
gen sugarspices=consexp1   if item==8 |item==9|item==10|item==17
gen edibleoil=consexp1     if item==11
gen milkmeateggs=consexp1  if item==12|item==13|item==14|item==15
gen vegfruits=consexp1     if item==16|item==20
gen otherfoods =consexp1  if item==18|item==19|item==21
gen medical1=consexp1      if item==29
gen nfood1=consexp1      if item==22|item==23|item==24|item==25|item==26|item==27|item==28|item==30
drop consexp1
*converted in yearly consumption
for var cereals  pulses  sugarspices  edibleoil  vegfruits  milkmeateggs  otherfoods medical1 nfood1: replace X=X*12
replace cereals=cereals/10 if cereals>= 100000
replace pulses=pulses/10 if pulses>= 100000
replace sugarspices=sugarspices/10 if sugarspices>=100000
replace edibleoil=edibleoil/10 if edibleoil>=100000
replace milkmeateggs=milkmeateggs/10 if milkmeateggs>=100000
replace vegfruits=vegfruits/10 if vegfruits>=100000
replace otherfoods= otherfoods/10 if  otherfoods>=100000

collapse cereals  pulses  sugarspices  edibleoil  vegfruits  milkmeateggs  otherfoods medical1 nfood1 , by(hhid)
egen foodexp=rsum(cereals  pulses  sugarspices  edibleoil  vegfruits  milkmeateggs  otherfoods )

sort hhid
save $tmp\consexp1, replace

//Do the same cleaning for all consumtion and income module. verify verything

*===============================================================================
*Section-6B 
*===============================================================================
use $hsepri1\SECTION06B, clear
append using $hsepri2\SECTION06B
ren q1_1 hhid
ren q6b1 item
gen consexp2= q6b4
gen medical2=consexp2 if item==1
gen eduexp=consexp2 if item==2|item==3|item==4
gen nfood2=consexp2 if item>4
collapse (sum) medical eduexp nfood2,by(hhid)
sort hhid
merge hhid using  $tmp\consexp1
drop _m
egen othnfood=rsum(nfood1 nfood2)
egen medical=rsum(medical1 medical2)
egen nfoodexp=rsum(othnfood medical eduexp)
replace othnfood=othnfood/10 if othnfood>100000
replace  medical= medical/10 if  medical>1000000
replace eduexp =eduexp /10 if eduexp >1000000
egen consexp=rsum(foodexp nfoodexp)
keep hhid cereals pulses sugarspices edibleoil milkmeateggs vegfruits otherfoods foodexp medical eduexp othnfood nfoodexp consexp
order hhid cereals pulses sugarspices edibleoil milkmeateggs vegfruits otherfoods foodexp medical eduexp othnfood nfoodexp consexp
label var consexp "Cons exp in last 30 days"
label var foodexp "Cons exp on food in last 30 days"
label var nfoodexp "Cons exp on non food items in last 30 days"
label var cereals "Exp on cereals in last 30 days"
label var pulses  "Exp on pulses in last 30 days"
label var sugarspices  "Exp on sugar and prices in last 30 days"
label var edibleoil  "Exp on edible oil in last 30 days"
label var milkmeateggs  "Exp on Milk, Meats and Eggs in last 30 days"
label var vegfruits  "Exp on Vegetables and fruits in last 30 days"
label var otherfoods  "Exp on other foods in last 30 days"
label var medical   "Exp on medical in last 30 days"
label var eduexp  "Exp on education  in last 30 days"
label var othnfood  "Exp on other nonfood  in last 30 days"
sort hhid
save $wkdata\consexp, replace



**************
*Combining the files
use $wkdata\hh_characteristics,clear
merge 1:1 hhid using $wkdata\head_char
drop _merge
merge 1:1 hhid  using $wkdata\hh_info
drop _merge
merge 1:1 hhid using $wkdata\hh_comp

***Poverty Index by State
**State Specific poverty line (Rural) for 2009-2010 (Rs per capita per month)
gen  POVL=693.8    if state==16  /*"ANDHRA PRADESH"*/ 
replace POVL=655.6  if state==11 /*"BIHAR"*/
replace POVL=617.3  if state==14 /*"CHHATTISGARH"*/ 
replace POVL=791.6  if state==7  /*"HARYANA"*/ 
replace POVL=616.3  if state==13 /*"JHARKHAND"*/ 
replace POVL=631.9  if state==5  /*"MADHYA PRADESH"*/
replace POVL=755.0  if state==6  /*"RAJASTHAN"*/
replace POVL=639.0  if state==17 /*"TAMIL NADU"*/

drop _merge
drop if state==4 //2 observatios for Gujarat
drop date_interview month_interview date_edit month_edit hour_start minute_start hour_end minute_end interviewer_name supervisor_name panel_hh locked_house reason
save $wkdata\hh_vars, replace



*===============================================================================
*===============================================================================
use $hsepri1\SECTION07, clear
append using $hsepri2\SECTION07
ren q1_1 hhid
ren q7_1  memid
ren q7_3  sex
ren q7_4  reltohead
ren q7_6  age
ren q7_8  mstatus
ren q7_16 education
******
replace mstatus=3 if mstatus==4|mstatus==5
replace mstatus=2 if mstatus==.
drop if memid==.
replace education=0 if education==16|education==.
replace education=1 if education==17
drop q*

***
save $wkdata\Mem_characteristics, replace

