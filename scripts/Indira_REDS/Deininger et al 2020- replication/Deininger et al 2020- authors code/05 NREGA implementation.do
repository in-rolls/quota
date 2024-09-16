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

** NREGS 
use "$vi14\VILLAGE_CODE.dta",clear
append using "$vsepri2\VILLAGE_CODE.dta"
save $tmp\Village_code, replace

use "$vi14\SECTION_16_A.dta", clear
append using "$vsepri2\SECTION_16_A.dta"
merge m:1 village_name using "$tmp\Village_code"
keep if _m==3

rename village_code_2006 villageid
recode villageid 940=093
recode villageid 901=092
replace villageid=241 if village_name=="CHADAYAM MANGALAM"
replace villageid=242 if village_name=="CHARAL VILAI"
replace villageid=094 if village_name=="FATEH NAGAR (TOWN)"
replace villageid=237 if village_name=="KANAVAI PATTI"
replace villageid=228 if village_name=="KARANUR"
replace villageid=225 if village_name=="KRISHNAPURAM"
replace villageid=229 if village_name=="RAYAPPANUR"
replace villageid=239 if village_name=="THETHOOR"

ta villageid

replace villageid=169 if village_name=="KANJANAYAKKAN PATTY"
replace villageid=235 if village_name=="PAIGA"
replace villageid=234 if village_name=="THIRUBUVANAM"

codebook villageid
ta villageid if _merge==2
drop _merge
// 1 village still seems to be missing! Mangawan (city) 

destring s16_a_p64_q01, ignore("A" "B" "C" "D" "a" "b" "c" "d") generate(quest) force

list s16_a_p64_q01 s16_a_p64_q03 if quest==.
drop if quest==.

recode s16_a_p64_q03 2 3 4 5 =0
ta s16_a_p64_q03

drop if quest== 39 | quest==20 | quest==49
recode s16_a_p64_q03 1=.  if quest==45
recode s16_a_p64_q03 0=1  if quest==45
recode s16_a_p64_q03 1=.  if quest==46
recode s16_a_p64_q03 0=1  if quest==46
recode s16_a_p64_q03 1=.  if quest==11
recode s16_a_p64_q03 0=1  if quest==11


egen regis = anymatch(quest) , v(1 2 3 4)
egen jobcard = anymatch(quest) , v(5 6 7 8 9 10 11 12 13 )
egen applic = anymatch(quest) , v(14 15 16 17 18 19 20 21 22)
egen planfw = anymatch(quest) , v(23 24 25 26 27 28)
egen implew = anymatch(quest) , v(29 30 31 32 33 34 35 36)
egen payment = anymatch(quest) , v(37 38 39 40 41 42 43 44 45 46)
egen monitor = anymatch(quest) , v(47 48 49 50)
egen socaudit = anymatch(quest) , v(51 52 53 54 55 56 57 58)
egen worksite = anymatch(quest) , v(61 62 63 64)


foreach n in regis jobcard applic planfw implew payment monitor socaudit worksite {
gen `n'_c= s16_a_p64_q03 if `n'==1
}
drop if regis==.

drop regis jobcard applic planfw implew payment monitor socaudit worksite

foreach n in regis jobcard applic planfw implew payment monitor socaudit worksite {
bysort villageid: egen `n' = mean(`n'_c)
}

collapse (mean) regis jobcard applic planfw implew payment monitor socaudit worksite, by(villageid)

foreach n in regis jobcard applic planfw implew payment monitor socaudit worksite {
recode `n' .=0
}
su 

egen nregaimpl= rowmean(regis jobcard applic planfw implew payment monitor socaudit worksite)

label var regis "NREGA Registration Process open to all"
label var jobcard "NREGA Job Cards issued"
label var applic "NREGA Norms For Application Of Work followed"
label var planfw "NREGA Plan For Work inclusive"
label var implew "NREGA Implementation Of Works transparent"
label var payment "NREGA Wage Payment correct and on time"
label var monitor "NREGA Monitoring Commitees in place"
label var socaudit "NREGA Social Audits conducted"
label var worksite "NREGA Worksite facilities" 
label var nregaimpl "NREGA implementation"

save "$wkdata\NREGA implementation.dta", replace

clear


**********************************************************
