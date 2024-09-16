clear
clear matrix

*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output
*===========================================================

use "$wkdata\ID_file_2014.dta",clear
merge 1:m hhid using  $wkdata\Mem_characteristics
keep if _m==3
drop _merge
merge 1:1 hhid memid using $wkdata\MNREGS_activities
keep if _m==3
drop _merge
drop if age<18
///(3 observations deleted)
save $wkdata\Member_info_merged, replace

***extensive and intensive rationing

*Demand for more work
egen nregwks=rsum(nregwks1 nregwks2 nregwks3)
egen moredays=rsum(moredays1 moredays2 moredays3)
for var moredays moredays1 moredays2 moredays3: replace X=0 if X==.
replace nregwks=1 if nregwks>1

**Rationed variables
gen extr=formalapp==1 & nregwks==0
gen intr=nregwks==1 & morewks==1
save $wkdata\Member_data_all, replace


*================================================================
**MNREGS at HH level
use $wkdata\Member_data_all,clear
gen Tmemwk_NG=indvnregwk
gen Mmemwk_NG=indvnregwk if sex==1
gen Fmemwk_NG=indvnregwk if sex==2
gen Mnregday=nregdays if sex==1
gen Fnregday=nregdays if sex==2
for var formalapp everwork extr  morewks1 morewks2 morewks3 : replace X=0 if X==2


collapse (sum) formalapp everwork extr intr Tmemwk_NG Mmemwk_NG Fmemwk_NG   Mnregday  Fnregday nregwks1 nregdays1 ///
morewks1 moredays1 nregwks2 nregdays2 morewks2 moredays2 nregwks3 nregdays3  morewks3 ///
moredays3 nreginc1 nreginc2 nreginc3 nregdays nreginc  morewks (mean) nregwages1 nregwages2 nregwages3 nregwage , by(hhid)
gen anymwk_NG= Tmemwk_NG>0
tab anymwk_NG
label var Tmemwk_NG "Total # of member worked in MNREGA in a HH"
label var Mmemwk_NG "Total # of male member worked in MNREGA in a HH"
label var Fmemwk_NG "Total # of female member worked in MNREGA in a HH"
label var anymwk_NG "HH with atleast one member worked in MNREGA"
sum Tmemwk_NG, d

for var nregwages1 nregwages2 nregwages3 nregwage: replace X=. if X==0
drop nreginc nregdays  morewks
************
egen nreginc=rsum(nreginc1 nreginc2 nreginc3)
egen nregwks=rsum(nregwks1 nregwks2 nregwks3)
egen morewks=rsum(morewks1 morewks2 morewks3)
egen nregdays=rsum(nregdays1 nregdays2 nregdays3)
egen moredays=rsum(moredays3 moredays2 moredays1)
gen day100= nregdays>=100 & nregdays~=.
replace day100=. if  nregdays==.

for var formalapp everwork nregwks morewks nregwks1 morewks1 nregwks2 morewks2 nregwks3 morewks3: replace X=1 if X>1


label var nregwks1 "Did anywork under mnregs in season1"
label var nregwks2 "Did anywork under mnregs in season2"
label var nregwks3 "Did anywork under mnregs in season3"
label var nregdays1 "# days worked under mnregs in season1"
label var nregdays2 "# days worked under mnregs in season2"
label var nregdays3 "# days worked under mnregs in season3"
label var nregdays  "# days worked under mnregs"
label var morewks1 "Wanted to work more under mnregs in season1"
label var morewks2 "Wanted to work under mnregs in season2"
label var morewks3 "Wanted to work under mnregs in season3"
label var morewks  "Wanted to work under mnregs"
label var moredays1 "# of more days wanted to work under mnregs in season1"
label var moredays2 "# of more days wanted to work under mnregs in season2"
label var moredays3 "# of more days wanted to work under mnregs in season3"
label var nregwages1 "Wages under mnregs in season1"
label var nregwages2 "Wages under mnregs in season2"
label var nregwages3 "Wages under mnregs in season3"
label var nreginc1 "Total income under mnregs in season1"
label var nreginc2 "Total income under mnregs in season2"
label var nreginc3 "Total income under mnregs in season3"
label var nreginc "Total income under mnregs"
label var day100 "Household who completed 100 days"
label var Mnregday "Male days in MNREGS"
label var Fnregday "Female days in MNREGS"

for var morewks1 morewks2 morewks3 morewks: replace X=1 if X>0
for var nregwks nregwks1 nregwks2  nregwks3:replace X=1 if X>0
for var extr intr: replace X=1 if X>0
gen less_100d=nregdays<100 & nregdays~=0
label var extr " Rationed on extensive margin"
label var intr "Rationed on intensive margin"

sort hhid

drop extr intr

gen extensive=formalapp==1 & nregwks==0
gen intensive=nregwks==1 & morewks==1

save $tmp\HH_nreg_activities, replace
*===============================================================

*Merging HH level information

use "$wkdata\ID_file_2014.dta",clear
merge m:1 hhid using $wkdata\hh_vars
keep if _merge==3
merge 1:1 hhid using $tmp\HH_pol_activities
keep if _m==3
drop _merge
merge 1:1 hhid using $tmp\HH_nreg_activities
keep if _m==3
drop _merge
merge m:1 villageid using $tmp\village_caste
drop _merge
merge m:1  villageid using $wkdata\VRES13
keep if _m==3
drop _merge
merge m:1  villageid using $tmp\pradhan_cha
drop _merge
merge m:1  villageid using $tmp\village_cha
drop _merge
save $tmp\hh_reservation_info, replace






*===============================================================================
***merging HH and Member level data

use $wkdata\Member_data_all,clear
merge m:1 hhid using $wkdata\hh_vars
keep if _merge==3
drop _merge
merge m:1 villageid using $tmp\village_caste
drop _merge
merge m:1  villageid using $wkdata\VRES13
keep if _m==3
drop _merge
merge m:1  villageid using $tmp\pradhan_cha
drop _merge
merge m:1  villageid using $tmp\village_cha
drop _merge
save $tmp\mem_reservation_info, replace




*======================================================
*****Some correction in data
*=======================================================
use $tmp\mem_reservation_info,clear
gen edu=education
replace edu=14 if education>14
/// 1=No education 2= Primary 3= Middle 4= Highschool 5= Secondary 6 Collage and above
drop if state==.
sort hhid
for var res_*: replace X=0 if X==.
sort state
merge state using $tmp\Iyar_data
tab _merge
****
recode sex 1=0 2=1 //Female
gen lnage=ln(age)
tab mstatus, gen(mst)
gen lnedu=ln(education)
gen lnhhsize=ln(hhsize)
gen fcratio=fchildren/children 
gen lnmedu=ln(mean_eduhh)
egen worker=rsum(wkmale wkfemale)
gen fworker=wkfemale/worker
for var lnage mst1 mst2 mst3 lnedu lnhhsize fcratio lnmedu worker fworker: replace X=0 if X==.
gen lnage2=lnage*lnage
gen lnedu2=lnedu*lnedu
for var nregdays1 nregdays2 nregdays3: replace X=0 if X==.
for var morewks morewks1 morewks2 morewks3: replace X=0 if X==.
gen allwk=selfagwk==1|selfnagwk==1|aglabwk==1|naglabwk==1|nreglabwk==1
egen alldays=rsum(selfagday selfnagday aglabday naglabday nreglabday)
gen lnalldays=ln(alldays)
replace lnalldays=0 if lnalldays==.
egen allinc=rsum(aglabinc naglabinc nreglabinc) 
gen lnallinc=ln(allinc)
replace lnallinc=0 if lnallinc==. 
tab religion , gen (reg)
gen lnmaxedu=ln(max_eduhh)
replace lnmaxedu=0 if lnmaxedu==.
gen lnhage=ln(hage)
*
label var sex "Female"
label var lnage "Age"
label var lnage2 "Age^2"
label var lnedu "Education"
label var lnedu2 "Education^2"
label var mst1 "Married"
label var mst3 "Widowed/Seperated"
label var jobcardm "Enrolled in MNREGA"

*********************************************************
replace hedu=0 if hedu==16|hedu==17|hedu==.
gen lnhedu=ln(hedu)
gen lnland=ln(land)
replace lnland=0 if lnland==.
gen lnmage=ln(mean_agehh)
replace lnmage=0 if lnmage==.
gen lnmaxedu2=lnmaxedu*lnmaxedu
gen lnmage2=lnmage*lnmage
gen lnhage2=lnhage*lnhage
gen lnhedu2=lnhedu*lnhedu
egen mm=rsum(Mmen Umen Wmen)
gen shUmen=Umen/mm
replace shUmen=0 if shUmen==.
egen ff=rsum(Mwomen Uwomen Wwomen)
gen shUwomen=Uwomen/ff
replace shUwomen=0 if shUwomen==.
drop mm ff
gen pcconsexp= consexp/hhsize
gen lnpccons=ln(pcconsexp)
replace lnpccons=0 if lnpccons==.
gen lnconsexp=ln(consexp)
recode hsex 1=0 2=1
xtile cons=consexp,nq(3)


label var hsex     "Female household head"
label var lnhage   "Household head's age"
label var lnhage2  "Household head's age^2"
label var lnhedu   "Household head's education"
label var lnhedu2  "Household head's education^2"
label var lnhhsize  "Household size"
label var fworker   "Share of female prime worker"
label var fcratio   "Share of girl child"
label var lnland    "Land owner"
label var lnmaxedu  "Max education level in HH"
label var lnmaxedu2 "Sqr of Max education level in HH"
label var lnmage    "Average age of HH"
label var lnmage2   "Sqr of average age of HH"
label var nregwk_nplot "MNREGS work carried out near own plot"
label var nregwk_oplot "MNREGS work carried out on own plot"


gen lnpop=ln(pop)
sum lnpop,d
replace lnpop=r(p95) if lnpop==.
gen proad=distProad==0
gen lndistTown=ln( distTown)
replace lndistTown=0 if lndistTown==.


save $wkdata\NREGS_RES_MEM_REG, replace


*******************************************************************************
**Correcting HH level data
use $tmp\hh_reservation_info,clear
/// 1=No education 2= Primary 3= Middle 4= Highschool 5= Secondary 6 Collage and above
drop if state==.
sort hhid
for var res_*: replace X=0 if X==.
sort state
merge state using $tmp\Iyar_data
tab _merge
****
gen lnhhsize=ln(hhsize)
gen fcratio=fchildren/children 
gen lnmedu=ln(mean_eduhh)
egen worker=rsum(wkmale wkfemale)
gen fworker=wkfemale/worker
for var  lnhhsize fcratio lnmedu worker fworker: replace X=0 if X==.
for var nregdays1 nregdays2 nregdays3: replace X=0 if X==.
for var morewks morewks1 morewks2 morewks3: replace X=0 if X==.
tab religion , gen (reg)
gen lnmaxedu=ln(max_eduhh)
replace lnmaxedu=0 if lnmaxedu==.
gen lnhage=ln(hage)

*********************************************************
replace hedu=0 if hedu==16|hedu==17|hedu==.
gen lnhedu=ln(hedu)
gen lnland=ln(land)
replace lnland=0 if lnland==.
gen lnmage=ln(mean_agehh)
replace lnmage=0 if lnmage==.
gen lnmaxedu2=lnmaxedu*lnmaxedu
gen lnmage2=lnmage*lnmage
gen lnhage2=lnhage*lnhage
gen lnhedu2=lnhedu*lnhedu
egen mm=rsum(Mmen Umen Wmen)
gen shUmen=Umen/mm
replace shUmen=0 if shUmen==.
egen ff=rsum(Mwomen Uwomen Wwomen)
gen shUwomen=Uwomen/ff
replace shUwomen=0 if shUwomen==.
drop mm ff
gen pcconsexp= consexp/hhsize
gen lnpccons=ln(pcconsexp)
replace lnpccons=0 if lnpccons==.
gen lnconsexp=ln(consexp)
recode hsex 1=0 2=1
xtile cons=consexp,nq(3)


label var hsex     "Female household head"
label var lnhage   "Household head's age"
label var lnhage2  "Household head's age^2"
label var lnhedu   "Household head's education"
label var lnhedu2  "Household head's education^2"
label var lnhhsize  "Household size"
label var fworker   "Share of female prime worker"
label var fcratio   "Share of girl child"
label var lnland    "Land owner"
label var lnmaxedu  "Max education level in HH"
label var lnmaxedu2 "Sqr of Max education level in HH"
label var lnmage    "Average age of HH"
label var lnmage2   "Sqr of average age of HH"
label var nregwk_nplot "MNREGS work carried out near own plot"
label var nregwk_oplot "MNREGS work carried out on own plot"


gen lnpop=ln(pop)
sum lnpop,d
replace lnpop=r(p95) if lnpop==.
gen proad=distProad==0
gen lndistTown=ln( distTown)
replace lndistTown=0 if lndistTown==.


save $wkdata\NREGS_RES_HH_REG, replace

