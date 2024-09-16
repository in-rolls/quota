clear
clear matrix

*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output
************************************************************
use "$wkdata\ID_file_2014.dta",clear
keep stateid districtid taluk block panchayat villageid
duplicates drop villageid, force 
ren villageid villagecode
merge 1:1 villagecode using $tmp\vid13

**** Merge village level data

use  $tmp\village_cha,clear
merge 1:1 villageid using $tmp\Pradhan_cha
drop _merge
merge 1:1 villageid using $wkdata\VRES13
drop _merge
merge 1:1 villageid using $wkdata\VVOTE_MNREGS_ISSUE13
drop _merge
merge 1:m villageid using $wkdata\NREGA_work_cumulative
drop _merge
merge 1:m villageid using $tmp\village_caste
 drop if _m==2
 drop _merge
for var empl_nrega1- spending5: replace X=0 if X==.

save $wkdata\VREG_RES_CUMUL, replace



***
use  $tmp\village_cha,clear
merge 1:1 villageid using $tmp\Pradhan_cha
drop _merge
merge 1:1 villageid using $wkdata\VRES13
drop _merge
merge 1:1 villageid using $wkdata\VVOTE_MNREGS_ISSUE13
drop _merge
merge 1:m villageid using $wkdata\NREGA_work_year
keep if _merge==3
drop _merge
merge 1:m villageid using $tmp\village_caste
keep if _m==3
drop _merge

for var empl_nrega1- spending5: replace X=0 if X==.
save $wkdata\VREG_RES_AGG, replace
