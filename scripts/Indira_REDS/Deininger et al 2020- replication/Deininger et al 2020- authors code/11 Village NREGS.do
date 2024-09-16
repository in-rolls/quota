clear
clear matrix
*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output

*==========================================================

** NREGS 

/*** not necessary... 
use "$wkdata\dateint.dta", clear
collapse (mean) stateid, by(villageid)
ta stateid
save "$wkdata\village state.dta"
clear
*/
***** 
use "$vsepri1\SECTION_17.dta", clear
merge m:1 village_name using "$vsepri1\VILLAGE_CODE.dta"
drop _merge village_sr_no gram_panchayat tehsil_taluka cd_block 
ren village_name village
sort village
merge village using  $tmp\vid13
drop _merge
order  district village villagecode
sort villagecode
drop village district
ren villagecode villageid



gen year1= 2006 if perioe==1 // 2006 stand for FY 2006-07
gen year2= 2007 if perioe==1
replace year1=2008 if perioe==2
replace year2=2009 if perioe==2
replace year1=2010 if perioe==3
replace year2=2011 if perioe==3
replace year1=2012 if perioe==4
replace year2=2013 if perioe==4

su _all if perioe==.

bysort villageid: egen info=total(perioe),m
ta villageid if info==. // 4 villages
bysort villageid: gen n=_n
replace year1=2013 if n==1 & info==.
replace perioe=4 if year1==2013 & info==.
drop n info

drop if perioe==. & s17_p70_q01==.

rename (s17_p70_q03 s17_p70_q04 s17_p70_q05 s17_p70_q06 s17_p70_q07) ///
		( works_takeup1 works_compl1 exp_mat1 exp_wage1 empl_nrega1)
rename (s17_p70_q08 s17_p70_q09 s17_p70_q10 s17_p70_q11 s17_p70_q12) ///
		(works_takeup2 works_compl2 exp_mat2 exp_wage2 empl_nrega2)
		
collapse (sum) 	works_takeup1- empl_nrega2 (max) year2 year1 , by(villageid  perioe s17_p70_q01)
		
reshape long works_takeup works_compl exp_mat exp_wage empl_nrega year	///
, i(villageid s17_p70_q01 perioe) j(j)

drop j
*drop if year==2013 // use only info up to beginning of  relevant agricutlural year

su
/*
merge m:1 year using "N:\Data\CPI\CPI India.dta"
keep if _merge==3
drop _merge
*/
ta s17_p70_q01
recode s17_p70_q01 0=11 12=11
rename s17_p70_q01 nregworks

label define nregworks 1 "Water conservation" 2 "Drought Proofing" 3 "Irrigation Facilities" 4 "Renovation of Water Bodies" ///
	5 "Land Development" 6 "Flood Control" 7 "Rural Connectivity" 8 "Drainage Works" 9 "Building/Maintenance" ///
	10 "School Building" 11 "Others"
	
lab val nregworks nregworks	

ta nregworks
drop perioe

gen nr=year if empl_nrega!=0
bysort villageid: egen nrega_year=min(nr)

egen spending= rowtotal(exp_mat exp_wage)

**Adding some line 

gen ngwk=1 if nregwork==1|nregwork==2|nregwork==6|nregwork==8 //Water conservation etc
replace ngwk=2 if nregwork==3|nregwork==4 //Irrigation and water bodies development
replace ngwk=3 if nregwork==5 //Land development
replace ngwk=4 if nregwork==7 //Rural connectivity
replace ngwk=5 if nregwork==9|nregwork==10|nregwork==11 //Others

collapse (sum) empl_nrega spending exp_mat exp_wage, by(villageid year ngwk)
 drop if ngwk==.
save $tmp\NREGA_work_sepri1, replace
*==========================================================
 ***FROM SEPRI2 (In sepri 2 information on section 17 was not collected since it was dulicates rather sesction 16 was expanded)
*==========================================================
use $vsepri2\SECTION_16_SEPT12_FEB14.dta,clear
gen  village=village_name

merge m:1 village_name using "$vsepri2\VILLAGE_CODE.dta"
drop _merge village_sr_no gram_panchayat tehsil_taluka cd_block 
sort village
merge village using  $tmp\vid13
drop _merge
order  district village villagecode
sort villagecode
drop village district
ren villagecode villageid


tostring period,replace
gen str yr=substr(period,-2, 2)
gen year="20"+yr
drop yr period
destring year,replace
*****in sepri2 we construct the same variables using section 16 because survey is messed up section 17
ren s16_b_p66_q01 nregwork
replace nregwork="01" if nregwork=="1A"|nregwork=="1B"|nregwork=="1C"|nregwork=="1D"
replace nregwork="01" if nregwork=="1"
replace nregwork="02" if nregwork=="3"|nregwork=="2"
replace nregwork="03" if nregwork=="4"
replace nregwork="05" if nregwork=="6"|nregwork=="5"|nregwork=="12B"|nregwork=="12D"|nregwork=="13A"
replace nregwork="07" if nregwork=="7"
replace nregwork="08" if nregwork=="8"
replace nregwork="09" if nregwork=="9"
replace nregwork="05" if nregwork=="11D"|nregwork=="11A"|nregwork=="11B"
replace nregwork="05" if nregwork=="13"
replace nregwork="11" if nregwork=="15"
replace nregwork="11" if nregwork=="21"|nregwork=="20"||nregwork=="18"
destring nregwork, replace force
egen spending=rsum(s16_b_p66_q06 s16_b_p66_q07)
egen exp_mat=rsum(s16_b_p66_q06)
replace exp_mat=exp_mat*100000
egen exp_wage= rsum(s16_b_p66_q07)
replace exp_wage=exp_wage*100000
replace spending=spending*100000
egen empl_nrega=rsum(s16_b_p66_q08)
collapse (sum) empl_nrega spending exp_mat exp_wage,  by( villageid  year nregwork)


gen ngwk=1 if nregwork==1|nregwork==2|nregwork==6|nregwork==8 //Water conservation etc
replace ngwk=2 if nregwork==3|nregwork==4 //Irrigation and water bodies development
replace ngwk=3 if nregwork==5 //Land development
replace ngwk=4 if nregwork==7 //Rural connectivity
replace ngwk=5 if nregwork==9|nregwork==10|nregwork==11 //Others
****
collapse (sum) empl_nrega spending exp_mat exp_wage, by(villageid year ngwk)
drop if ngwk==.
replace spending=spending/100000 if  spending>2000
replace spending=r(p95) if spending>r(p95) & spending!=.
replace spending=spending*100000
destring year, replace
append using $tmp\NREGA_work_sepri1
recode year 2015=2014 2016=2014
collapse (sum) empl_nrega spending exp_mat exp_wage, by(villageid ngwk year)
reshape wide empl_nrega spending exp_mat exp_wage , i( villageid year) j( ngwk)
egen empl_nrega=rsum( empl_nrega1 empl_nrega2 empl_nrega3 empl_nrega4 empl_nrega5)
egen spending =rsum( spending1 spending2 spending3 spending4 spending5 )
egen exp_mat=rsum(exp_mat1 exp_mat2 exp_mat3 exp_mat4 exp_mat5)
egen exp_wage=rsum(exp_wage1 exp_wage2 exp_wage3 exp_wage4 exp_wage5)

save $wkdata\NREGA_work_year, replace



collapse (sum) empl_nrega* spending* exp_mat* exp_wage*,by(villageid)

save $wkdata\NREGA_work_cumulative, replace

/*




***
gen round=1 if year==2006|year==2007
replace round=2 if year==2009|year==2010|year==2011|year==2012
replace round=2 if year==2013 & state=="UP"|year==2014 & state=="UP"

collapse (sum)empl_nrega* spending* , by(round villageid)
drop if round==.

reshape wide empl_nrega* spending*, i( villageid) j(round)

save $tmp\NREGA_work, replace

drop *2
ren *1 *
save $wkdata\NREGA_work2006, replace

use $tmp\NREGA_work, clear
drop *1
ren *2 *
save $wkdata\NREGA_work2014, replace



**For crosssection data
use $wkdata\NREGA_work_year, clear
replace year=2008 if year==2007
replace year=2012 if year==2013
replace year=2012 if year==2014|year==2015|year==2016
drop if year==2006|year==2007

collapse(sum) empl_nrega* spending* ,by(year villageid)
egen empl_nrega_tot=rowtotal( empl_nrega1 empl_nrega2 empl_nrega3 empl_nrega4 empl_nrega5), missing
egen spending_tot=rowtotal(spending1 spending2 spending3 spending4 spending5), missing

for var empl_nrega* spending*: gen lnX=ln(X)
keep ln* year villageid
save $wkdata\crosssection_NREGS_work_long, replace


gen time="t" if year==2012
replace time ="t_1" if year==2011
replace time ="t_2" if year==2010
replace time ="t_3" if year==2009
replace time ="t_4" if year==2008
drop year
reshape wide lnempl_nrega* lnspending*,i(villageid) j(time) string
save $wkdata\crosssection_NREGS_work, replace





/*
/*
gen spend_wc= spending if nregworks==1
bysort villageid: egen spending_wc = sum(spend_wc)
gen spend_dp= spending if nregworks==2
bysort villageid: egen spending_dp = sum(spend_dp)
gen spend_if= spending if nregworks==3
bysort villageid: egen spending_if = sum(spend_if)
gen spend_wb= spending if nregworks==4
bysort villageid: egen spending_wb = sum(spend_wb)
gen spend_ld= spending if nregworks==5
bysort villageid: egen spending_ld = sum(spend_ld)
gen spend_fc= spending if nregworks==6
bysort villageid: egen spending_fc = sum(spend_fc)
gen spend_rc= spending if nregworks==7
bysort villageid: egen spending_rc = sum(spend_rc)
gen spend_dw= spending if nregworks==8
bysort villageid: egen spending_dw = sum(spend_dw)
gen spend_bm= spending if nregworks==9
bysort villageid: egen spending_bm = sum(spend_bm)
gen spend_sb= spending if nregworks==10
bysort villageid: egen spending_sb = sum(spend_sb)
gen spend_ot= spending if nregworks==11
bysort villageid: egen spending_ot = sum(spend_ot)

collapse (sum) works_takeup works_compl exp_mat exp_wage empl_nrega spending spend_* (min) nrega_year (mean) spending_*, by(villageid year) 

foreach n in wc dp if wb ld fc rc dw bm sb ot {
recode spend_`n' .=0
}

foreach n in wc dp if wb ld fc rc dw bm sb ot {
gen sharespending_`n' = spend_`n'/spending
recode sharespending_`n' .=0
}
egen share_tot= rowtotal(sharespending_*)
drop share_tot 

eststo clear
reg villageid year
eststo: estpost tabstat sharespending_* if year==2006,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2007,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2008,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2009,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2010,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2011,  stat(N mean sd) columns(statistics) 
eststo: estpost tabstat sharespending_* if year==2012,  stat(N mean sd) columns(statistics) 

esttab est1 est2 est3 est4 est5 est6 est7 using "$latex4\summ5.tex", ///
	cells("mean(fmt(a2)label (Mean))")  ///
	replace nostar label noobs nonote nonumber  ///
	title(NREGS projects\label{nregsprojects}) ///
	mtitles("2006" "2007" "2008" "2009" "2010" "2011" "2012")

*gen empl_nrega_2006 = empl_nrega if year==2006
*gen empl_nrega_2007 = empl_nrega if year==2007
*gen empl_nrega_2008 = empl_nrega if year==2008
*gen empl_nrega_2009 = empl_nrega if year==2009
*gen empl_nrega_2010 = empl_nrega if year==2010
gen empl_nrega_2011 = empl_nrega if year==2011
gen empl_nrega_2012 = empl_nrega if year==2012
collapse (sum) works_takeup works_compl exp_mat exp_wage empl_nrega* spending (min) nrega_year (mean) spending_*, by(villageid) 

foreach n in wc dp if wb ld fc rc dw bm sb ot {
recode spending_`n' .=0
}

foreach n in wc dp if wb ld fc rc dw bm sb ot {
gen sharespending_`n' = spending_`n'/spending
recode sharespending_`n' .=0
}

gen labor_ratio = exp_wage / spending
recode labor_ratio .=0
gen capital_ratio = exp_mat / spending
recode capital_ratio .=0

gen compl_rate = works_compl/ works_takeup
recode compl_rate .=0
replace compl_rate=1 if compl_rate>=1

su

label var works_takeup "No of work taken up since NREGA implementation"
label var works_compl "No of work completed since NREGA implementation"
label var exp_mat "Material spending, NREGA (INR 100,000)"
label var exp_wage "Wage spending, NREGA (INR 100,000)" 
label var spending "Total spending, NREGA (INR 100,000)"
label var capital_ratio "Material spending to toal spending, NREGA"
label var labor_ratio "Wage spending to toal spending, NREGA (INR 100,000)" 
label var empl_nrega "Total employment (person-days) generated, NREGA"
label var nrega_year "Year, NREGA works started"
label var spending_wc "Spending on water conservation, NREGA"
label var spending_dp "Spending on drought proofing, NREGA"
label var spending_if "Spending on irrigation facilities, NREGA"
label var spending_wb "Spending on renovation of water bodies, NREGA"
label var spending_ld "Spending on land development, NREGA"
label var spending_fc "Spending on flood control, NREGA"
label var spending_rc "Spending on rural connectivity, NREGA"
label var spending_dw "Spending on drainage works, NREGA"
label var spending_bm "Spending on building/ maintenance, NREGA"
label var spending_sb "Spending on school buildings, NREGA"
label var spending_ot "Spending on other projects, NREGA"
label var sharespending_wc "Share of spending on water conservation, NREGA"
label var sharespending_dp "Share of spending on drought proofing, NREGA"
label var sharespending_if "Share of spending on irrigation facilities, NREGA"
label var sharespending_wb "Share of spending on renovation of water bodies, NREGA"
label var sharespending_ld "Share of spending on land development, NREGA"
label var sharespending_fc "Share of spending on flood control, NREGA"
label var sharespending_rc "Share of spending on rural connectivity, NREGA"
label var sharespending_dw "Share of spending on drainage works, NREGA"
label var sharespending_bm "Share of spending on building/ maintenance, NREGA"
label var sharespending_sb "Share of spending on school buildings, NREGA"
label var sharespending_ot "Share of spending on other projects, NREGA"
label var compl_rate "Completion rate, NREGA works"
*/


*save "$wkdata\NREGA works.dta", replace

/*
keep if year==2006
save "$wkdata\NREGA works 2006.dta", replace

***
use "$wkdata\NREGA works.dta", clear
keep if year==2014
save "$wkdata\NREGA works 2014.dta", replace
*/


clear


**********************************************************


**================================================================================
*MNREGA project and finances and empolyment generated
*===============================================================================
use $vi14\SECTION_17,clear
ren village_name village
gen year1= 2006 if perioe==1 // 2006 stand for FY 2006-07
gen year2= 2007 if perioe==1
replace year1=2008 if perioe==2
replace year2=2009 if perioe==2
replace year1=2010 if perioe==3
replace year2=2011 if perioe==3
replace year1=2012 if perioe==4
replace year2=2013 if perioe==4

su _all if perioe==.

bysort village: egen info=total(perioe),m
ta village if info==. // 4 villages
bysort village: gen n=_n
replace year1=2013 if n==1 & info==.
replace perioe=4 if year1==2013 & info==.
drop n info

drop if perioe==. & s17_p70_q01==.

rename (s17_p70_q03 s17_p70_q04 s17_p70_q05 s17_p70_q06 s17_p70_q07) ///
		( works_takeup1 works_compl1 exp_mat1 exp_wage1 empl_nrega1)
rename (s17_p70_q08 s17_p70_q09 s17_p70_q10 s17_p70_q11 s17_p70_q12) ///
		(works_takeup2 works_compl2 exp_mat2 exp_wage2 empl_nrega2)
		
collapse (sum) 	works_takeup1- empl_nrega2 (max) year2 year1 , by(village  village_sr_no district perioe s17_p70_q01)
		
reshape long works_takeup works_compl exp_mat exp_wage empl_nrega year	///
, i(village s17_p70_q01 perioe) j(j)

drop j
*drop if year==2013 // use only info up to beginning of  relevant agricutlural year

su
/*
merge m:1 year using "N:\Data\CPI\CPI India.dta"
keep if _merge==3
drop _merge
*/
ta s17_p70_q01
recode s17_p70_q01 0=11 12=11
rename s17_p70_q01 nregworks

label define nregworks 1 "Water conservation" 2 "Drought Proofing" 3 "Irrigation Facilities" 4 "Renovation of Water Bodies" ///
	5 "Land Development" 6 "Flood Control" 7 "Rural Connectivity" 8 "Drainage Works" 9 "Building/Maintenance" ///
	10 "School Building" 11 "Others"
	
lab val nregworks nregworks	

ta nregworks
drop perioe

gen nr=year if empl_nrega!=0
bysort village: egen nrega_year=min(nr)

egen spending= rowtotal(exp_mat exp_wage)

**Adding some line 

gen ngwk=1 if nregwork==1|nregwork==2|nregwork==6|nregwork==8 //Water conservation etc
replace ngwk=2 if nregwork==3|nregwork==4 //Irrigation and water bodies development
replace ngwk=3 if nregwork==5 //Land development
replace ngwk=4 if nregwork==7 //Rural connectivity
replace ngwk=5 if nregwork==9|nregwork==10|nregwork==11 //Others
collapse (sum) empl_nrega spending, by(village year ngwk)
drop if ngwk==.
save $tmp\NREGA_work_sepri1, replace

***FROM SEPRI2 (In sepri 2 information on section 17 was not collected since it was dulicates rather sesction 16 was expanded)
use $vsepri2\SECTION_17,clear
ren village_name village
gen str year=substr(s16_q04,-4, 4)
replace year="2014" if year=="/214"
replace year="2013" if year=="0132"
replace year="2006" if year=="2001"
replace year="2014" if year=="2144"
ren s16_q01 nregwork

*****in sepri2 we construct the same variables using section 16 because survey is messed up section 17
replace nregwork="01" if nregwork=="1A"
replace nregwork="01" if nregwork=="1B"
replace nregwork="01" if nregwork=="1"
replace nregwork="02" if nregwork=="3"
replace nregwork="03" if nregwork=="4"
replace nregwork="05" if nregwork=="6"
replace nregwork="07" if nregwork=="7"
replace nregwork="08" if nregwork=="8"
replace nregwork="09" if nregwork=="9"
replace nregwork="05" if nregwork=="11B"
replace nregwork="05" if nregwork=="13"
replace nregwork="11" if nregwork=="15"
replace nregwork="11" if nregwork=="21"
destring nregwork, replace
egen spending=rsum(s16_q08 s16_q09)
egen empl_nrega=rsum(s16_q10)
collapse (sum) empl_nrega spending,  by( village district year nregwork)

gen ngwk=1 if nregwork==1|nregwork==2|nregwork==6|nregwork==8 //Water conservation etc
replace ngwk=2 if nregwork==3|nregwork==4 //Irrigation and water bodies development
replace ngwk=3 if nregwork==5 //Land development
replace ngwk=4 if nregwork==7 //Rural connectivity
replace ngwk=5 if nregwork==9|nregwork==10|nregwork==11 //Others
collapse (sum) empl_nrega spending, by(villag year ngwk)
drop if ngwk==.
replace spending=spending/100000 if  spending>2000
replace spending=r(p95) if spending>r(p95) & spending!=.
replace spending=spending*100000
destring year, replace
append using $tmp\NREGA_work_sepri1

***
drop if year>2014
collapse (sum) empl_nrega spending, by(villag ngwk)
reshape wide empl_nrega spending, i(village ) j(ngwk)
sort village
save $tmp\NREG_WORK, replace

