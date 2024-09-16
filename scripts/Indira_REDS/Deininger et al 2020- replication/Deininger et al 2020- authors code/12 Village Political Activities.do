clear
clear matrix

*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output
*===============================================================================
*Governance and Political Activities
*===============================================================================
//Note-check with anupam and jp singh why there are still missing in village data. 
use $vsepri1\SECTION_11_D,clear
append using  $vsepri2\SECTION_11_D

drop village_sr_no gram_panchayat tehsil_taluka cd_block 
ren village_name village

ren s11_d1_p35_q01 srno

 ren s11_d1_p35_q03 ele_year
 ren s11_d1_p35_q04 ele_month
 ren s11_d1_p35_q05 ele_memno
 ren s11_d1_p35_q06 pop_panch
 ren s11_d1_p35_q07 res_w
 ren s11_d1_p35_q08 res_sc
 ren s11_d1_p35_q09 res_st
 ren s11_d1_p35_q10 res_obc
replace srno=2 if village=="HARRAEE" & district=="CHHATARPUR" & ele_year==2004

reshape wide ele_year ele_month ele_memno pop_panch res_w res_sc res_st res_obc, i(village) j(srno)
for var res_*: replace X=0 if X>1 
sort village
merge village using  $tmp\vid13
drop _merge
order  district village villagecode
sort villagecode
save $tmp\reservation13, replace
ren villagecode villageid
save $wkdata\VRES13, replace

use $wkdata\VRES13,clear
replace statename="ANDHRA PRADESH" if statename=="ANDHRA P RADESH "
replace statename="ANDHRA PRADESH" if statename=="ANDHRA P RADESH"
replace statename="ANDHRA PRADESH" if statename=="ANDRA PRADESH "
replace statename="ANDHRA PRADESH" if statename=="ANDRA PRADESH"
replace statename="CHHATISGARH" if statename=="CHHATTISGARH "
replace statename="CHHATISGARH" if statename=="CHHATTISGARH"
replace statename="HARYANA" if statename=="HARIYANA"
replace statename="MAHARASHTRA" if statename=="MAHARASTHRA"
replace statename="MAHARASHTRA" if statename=="MAHARASTRA"
replace statename="MAHARASHTRA" if statename=="MAHRASHTRA"
tab state
replace statename="ODISHA" if statename=="ODISHA  15"
replace statename="RAJASTHAN" if statename=="RAJASTHAN-06"
replace statename="TAMIL NADU" if statename=="TAMILADU"
replace statename="TAMIL NADU" if statename=="TAMILANADU"
replace statename="TAMIL NADU" if statename=="TAMILNADU"
replace statename="TAMIL NADU" if statename=="TMIL NADU"
replace statename="TAMIL NADU" if statename=="TMILNADU "
replace statename="TAMIL NADU" if statename=="TMILNADU"
replace statename="UTTAR PRDESH" if statename=="UTTAR PRADESH"
replace statename="UTTAR PRDESH" if statename=="U.P"
replace statename="WEST BENGAL" if statename=="WESE BENGAL"



***************************
*Secton 11 on MNREGA- issue discussed in gram sabha

use $vsepri1\SECTION_11_9,clear
append using $vsepri2\SECTION_11_9

ren s11_9_p41_q31 vmn_persond
ren s11_9_p41_q32 vmn_personv
ren s11_9_p41_q33 vmn_projectd
ren s11_9_p41_q34 vmn_projectv
ren s11_9_p41_q35 vmn_workd
ren s11_9_p41_q36 vmn_workv
ren s11_9_p41_q37 vmn_auditd
ren s11_9_p41_q38 vmn_auditv
ren s11_9_p41_q39 vmn_operd
ren s11_9_p41_q40 vmn_operv
ren village_name village
ren s11_9_p41_q01 srno
keep village srno vmn_*
duplicates drop village srno, force

for var vmn_*d: replace X=0 if X>=2
for var vmn_*v: replace X=0 if X==1
for var vmn_*v: replace X=1 if X>1

sort village
merge village using  $tmp\vid13
drop _merge
order   village villagecode
drop village 
ren villagecode villageid
save $wkdata\VVOTE_ISSUE13, replace
collapse (sum ) vmn_persond- vmn_operv, by(villageid)
label var vmn_persond "Identification of persons for work under MGNREGS-Discussed"
label var vmn_personv "Identification of persons for work Under MGNREGS-Voted"
label var vmn_projectd "Works Under MGNREGS-Discussed"
label var vmn_projectv  "Works Under MGNREGS-voted"
label var vmn_workd  "Works Approved under MGNREGS-Discussed"
label var vmn_workv  "Works Approved under MGNREGS-voted"
label var vmn_auditd "Recommendations of the Social Audit-Discussed"
label var vmn_auditv "Recommendations of the Social Audit-Voted"
label var vmn_operd "Operation of the MNREGS-Discussed"
label var vmn_operv "Operation of the MNREGS-Voted"

save $wkdata\VVOTE_MNREGS_ISSUE13, replace

//Anupam-please check for missing-

*===============================================================================
*Pradhan's characteristics
use $vsepri1\SECTION_11_D_2,clear
append using  $vsepri2\SECTION_11_D_2
drop village_sr_no gram_panchayat tehsil_taluka cd_block 
ren village_name village

ren s11_d2_p36_q01 srno
ren s11_d2_p36_q02 candidate
ren s11_d2_p36_q03 Listingid
ren s11_d2_p36_q05 pcontest
ren s11_d2_p36_q06 sino_prev
ren s11_d2_p36_q07 psex
ren s11_d2_p36_q08 pedu
ren s11_d2_p36_q09 pcaste
ren s11_d2_p36_q10 preligion
ren s11_d2_p36_q11 pposition
**keep if srno==1
ren s11_d2_p36_q14 vote
ren s11_d2_p36_q13 pvoted
keep if candidate ==1

sort village
merge village using  $tmp\vid13
drop _merge
order  district village villagecode
sort villagecode
save $tmp\reservation13, replace
drop village district
ren villagecode villageid
keep villageid p* vote  srno

gen pp=pedu
tostring pp,replace
gen pp1=substr(pp,1,1)
destring pp1, replace
replace pedu=pp1 if pedu>5
replace pedu=5 if pedu>5
drop pp pp1
 replace srno=2 if srno==.
**
reshape wide pcontest psex pedu pcaste preligion pposition,i(villageid) j(srno)
for var pcontest1 psex1  pposition1 psex2 pcontest2 pposition2: replace X=0 if X==2
for var pcontest1 psex1  pposition1 psex2 pcontest2 pposition2: replace X=0 if X==.
sum pcontest1 psex1  pposition1 psex2 pcontest2 pposition2
for var pposition1  pposition2: replace X=1 if X~=0
sum pcontest1 psex1  pposition1 psex2 pcontest2 pposition2
for var pedu1 pedu2: replace X=2 if X==.
replace pcaste2=4 if pcaste2==.
replace pcaste1=4 if pcaste1==.
replace preligion2=1 if preligion2==.
replace preligion1=1 if preligion1==.
ren (psex1 pedu1 pcaste1 preligion1 pposition1 pcontest1) (psex pedu pcaste preligion  pposition pcontest)
save $tmp\Pradhan_cha,replace //current pradhan





*===============================================================================
**==============================================================================
                              **Analysis
*===============================================================================
****
use $tmp\Pradhan_cha,clear
merge 1:1 village using $wkdata\VRES13
gen lost1= psex==1  & psex2==0 & res_w2==0
gen lost2= (psex==1 & res_w1==1) & psex2==0 & res_w2==0
sum lost1 lost2



use $wkdata\VRES13,clear


gen never=res_w1==0 & res_w2==0
gen switch=res_w1==0 & res_w2==1
gen continues=res_w1==1 & res_w2==1
gen new=res_w1==1 & res_w2==0
keep  never switch continues new village res*
 save $tmp\res_characteristics,replace


*===============================================================================
***Table for characteristics of candidates for pradhan
*===============================================================================
**Voting population
use $wkdata\NREGS_RES_MEM_REG, clear
ren villageid villagecode
drop village
drop _merge
merge 1:1 villagecode using $tmp\vid13,force
gen vpop= 1 if age>=21
collapse (sum) vpop (mean)pop hh, by( village)
save $tmp\vpop,replace



use $vsepri1\SECTION_11_D,clear
append using  $vsepri2\SECTION_11_D
save $tmp\res_temp,replace

use $vsepri1\SECTION_11_D_2,clear
append using  $vsepri2\SECTION_11_D_2
merge m:1 village_name using $tmp\res_temp
keep if _m==3
drop _merge
ren village_name village
merge m:1 village using $tmp\res_characteristics

drop _merge
drop village_sr_no gram_panchayat tehsil_taluka cd_block 
ren s11_d2_p36_q01 pperiod

merge m:1 village using $tmp\vpop
drop _merge

ren s11_d2_p36_q08 pedu
gen pp=pedu
tostring pp,replace
gen pp1=substr(pp,1,1)
destring pp1, replace
replace pedu=pp1 if pedu>5
replace pedu=5 if pedu>5
drop pp pp1
replace s11_d2_p36_q14= s11_d2_p36_q14/10 if s11_d2_p36_q14>100
replace s11_d2_p36_q14= s11_d2_p36_q14/10 if s11_d2_p36_q14>100
replace s11_d2_p36_q14=100 if s11_d2_p36_q14==0
ren s11_d2_p36_q14 vote

****
*Eclection pattern

gen candidate=1
gen women=1 if s11_d2_p36_q07==2

gen primery=pedu==1
gen secondary=pedu==2
gen higher=pedu==3|pedu==4
gen graduate=pedu==5

gen sc=s11_d2_p36_q10==1
gen st=s11_d2_p36_q10==2
gen obc=s11_d2_p36_q10==3
gen oc=s11_d2_p36_q10==4

gen women_1=women==1 & s11_d2_p36_q02==1 & pperiod==1
gen women_2=women==1 & s11_d2_p36_q02==1 & pperiod==2
gen  reserved= s11_d1_p35_q07==1

**People voted
gen vp= s11_d2_p36_q13
replace vpop=vpop/10 if vpop>10000
gen vpshare=vp/hh
save $tmp\reservation_switch, replace

use  $tmp\reservation_switch, clear

*collapse (sum) candidate women  (mean) reserved primery secondary higher graduate sc st obc oc women_1 women_2, by(village pperiod)
collapse (sum) candidate women  (mean) vote reserved primery secondary higher graduate sc st obc oc women_1 women_2 vpop pop hh vp vpshare, by(village never)
sum if never==1
use  $tmp\reservation_switch, clear
collapse (sum) candidate women  (mean) vote reserved primery secondary higher graduate sc st obc oc women_1 women_2 vpop pop hh vp vpshare, by(village switch)
sum if switch==1
use  $tmp\reservation_switch, clear

collapse (sum) candidate women  (mean) vote reserved primery secondary higher graduate sc st obc oc women_1 women_2 vpop pop hh vp vpshare, by(village continues)
sum if continues==1
use  $tmp\reservation_switch, clear

collapse (sum) candidate women  (mean) vote reserved primery secondary higher graduate sc st obc oc women_1 women_2 vpop pop hh vp vpshare, by(village new)
sum if new==1


*Regression table

use  $tmp\reservation_switch, clear
gen x=1
bysort village: egen shw=sum (x) if s11_d2_p36_q07==2 & pperiod==1
bysort village: egen shwm=max(shw)
gen votes=s11_d2_p36_q13
bysort village: egen shm=sum (x) if  pperiod==1
bysort village: egen shmm=max(shm)
gen shw1=shwm/shmm
areg shw1 res_w2,ab( district )
gen polparty=s11_d2_p36_q20 
replace polparty=0 if polparty==2
gen withincast=s11_d2_p36_q15 
replace votes=votes/100

rreg shwm res_w2 ,ab(district)
outreg2 using $out\Women_ch.xls, replace auto(3)  bdec(3) label noobs 

reg shw1 res_w2,ab(district)
outreg2 using $out\Women_ch.xls, append auto(3)  bdec(3) label  noobs 

areg votes res_w2, ab(district)
outreg2 using $out\Women_ch.xls, append auto(3)  bdec(3) label noobs 

areg polparty res_w2,ab(district)
outreg2 using $out\Women_ch.xls, append auto(3)  bdec(3) label noobs


*8Dropped current panchayat
use  $tmp\reservation_switch, clear
gen x=1
bysort village: egen shw=sum (x) if s11_d2_p36_q07==2 & pperiod==1
bysort village: egen shwm=max(shw)
gen votes=s11_d2_p36_q13
bysort village: egen shm=sum (x) if  pperiod==1
bysort village: egen shmm=max(shm)
gen shw1=shwm/shmm
areg shw1 res_w2,ab( district )
gen polparty=s11_d2_p36_q20 
replace polparty=0 if polparty==2
gen withincast=s11_d2_p36_q15 
replace votes=votes/100
drop if res_w1==1

reg shwm res_w2
outreg2 using $out\Women_ch_current.xls, replace auto(3)  bdec(3) label noobs 

reg shw1 res_w2
outreg2 using $out\Women_ch_current.xls, append auto(3)  bdec(3) label  noobs 

areg votes res_w2, ab(district)
outreg2 using $out\Women_ch_current.xls, append auto(3)  bdec(3) label noobs 

areg polparty res_w2,ab(district)
outreg2 using $out\Women_ch_current.xls, append auto(3)  bdec(3) label noobs
