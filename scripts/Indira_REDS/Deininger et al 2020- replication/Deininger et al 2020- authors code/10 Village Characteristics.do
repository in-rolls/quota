clear
clear matrix
*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output

*==============================================================
*Village codes
*==============================================================

 use $vsepri1\VILLAGE_CODE, clear
 append using  $vsepri2\VILLAGE_CODE

  drop village_sr_no  panchayat_code
  ren village_name village
  ren gram_panchayat panchayat
  ren tehsil_taluka tehsil
  ren cd_block block
  ren current_village_code villageid_13
  ren village_code_2006 villageid_06
  ren state statename
  ren district distname

  replace villageid_13=villageid_06 if villageid_13==0|villageid_13==.
  replace villageid_06=villageid_13 if villageid_06==0|villageid_06==.
  replace villageid_13=234 if village=="THIRUBUVANAM"
  replace villageid_06=234 if village=="THIRUBUVANAM"
  replace villageid_06=228 if village=="KARANUR"
  replace villageid_13=228 if village=="KARANUR"
  replace villageid_06=225 if village=="KRISHNAPURAM"
  replace villageid_13=225 if village=="KRISHNAPURAM"
  replace villageid_06=242 if village=="CHARAL VILAI"
  replace villageid_13=242 if village=="CHARAL VILAI"
  replace villageid_06=241 if village=="CHADAYAM MANGALAM"
  replace villageid_13=241 if village=="CHADAYAM MANGALAM"
  replace villageid_06=229 if village=="RAYAPPANUR"
  replace villageid_13=229 if village=="RAYAPPANUR"
  replace villageid_06=235 if village=="KANJANAYAKKAN PATTY"
  replace villageid_13=235 if village=="KANJANAYAKKAN PATTY"
  replace villageid_06=169 if village=="PAIGA"
  replace villageid_13=169 if village=="PAIGA"
  replace villageid_06=92  if village=="ACHLANA"
  replace villageid_13=92  if village=="ACHLANA"
  replace villageid_06=93  if village=="PANOOND"
  replace villageid_13=93  if village=="PANOOND"
  replace villageid_06=94  if village=="FATEH NAGAR (TOWN)"
  replace villageid_13=94  if village=="FATEH NAGAR (TOWN)"
  gen villagecode=villageid_13
  replace villagecode=177 if village=="MANDALGRAM" & statename=="WEST BENGAL"
  replace villagecode=43 if village=="SURALI" 
  replace villagecode=39 if village=="SUBHANPUR" 
  replace villagecode=38 if village=="PIMPAL GAON SUTAR" 
  replace villagecode=161 if village=="BHARHULIYA" 
 
  sort village
  
  keep village villagecode statename distname
  save $tmp\vid13, replace
  
  


use $vsepri1\VILLAGE_DETAIL.dta,clear 
append using $vsepri2\VILLAGE_DETAIL.dta 

ren village_name village
ren q1_2a_census_2013 pop
ren q1_2b_census_2013 hh
  
keep village pop hh
sort village
save $tmp\vdeck13_1,replace


*==============================================================
***Basic Village Structure***
*==============================================================
use $vsepri1\SECTION_1_3.dta,clear 
append using $vsepri2\SECTION_1_3.dta 

drop village_sr_no gram_panchayat tehsil_taluka cd_block 
ren village_name village
ren q1_3_slno srno
ren p4_q3 dist
keep village district srno dist
bysort village srno: gen x=_n
tab x
drop if x>1
drop x
reshape wide dist, i(village) j( srno)

*****Renaming the variables***
ren 	dist1	distDHQ
ren 	dist2	distTHQ
ren 	dist3	distBHQ
ren 	dist4	distBS
ren 	dist5	distRS
ren 	dist6	distPO
ren 	dist7	distTown
ren 	dist8	distProad
keep district village distDHQ distTHQ distBHQ distBS distRS distPO distTown distProad

foreach var of varlist distDHQ distTHQ distBHQ distBS distRS distPO distTown distProad{
bysort district :egen `var'_a = mean(`var')
egen `var'_aa = mean(`var')
replace `var'=`var'_a  if  `var'==.
replace `var'=`var'_aa  if  `var'==.
drop `var'_a `var'_aa
}

drop district

sort village
save $tmp\vdeck13_2,replace
***============================================================

*Merged
use $tmp\vdeck13_1,clear
merge 1:1 village using $tmp\vdeck13_2
keep if _m==3
drop _merge
sort village
merge 1:1 village using  $tmp\vid13
drop _merge
drop distname statename village
ren villagecode villageid
save $tmp\village_cha,replace














