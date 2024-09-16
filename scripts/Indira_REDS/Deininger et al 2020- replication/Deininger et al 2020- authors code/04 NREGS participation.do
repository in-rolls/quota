clear
clear matrix

clear
clear matrix

global reds "C:\Users\Dropbox\SEPRI"

global hsepri1 $reds\sepri1\hh
global hsepri2 $reds\sepri2\hh
global vsepri1 $reds\sepri1\village
global vsepri2 $reds\sepri2\village

*****************************************************
*****************************************************
global path "C:\Users\Dropbox\SAS\RESERV"
global wkdata $path\Wkdata
global do $path\Do
global tmp $path\Temp
global out $path\Output
*===============================================================================
* Information on MNREGA work and participation
*===============================================================================
use $hsepri1\SECTION09, clear
append using $hsepri2\SECTION09
ren q1_1    hhid
ren q9_2    memid
ren q9_3    uid
ren q9_4    uid_year
ren q9_5    uid_month
ren q9_6    jobcardm
ren q9_7    everapply
ren q9_8    reason_not_applied
ren q9_9    reason_not_gettingcard
ren q9_12   everwork
ren q9_13   reason_not_work


*****data corrections
replace uid_year=2011 if  uid_year <2010
replace uid_year=2014 if  uid_year >2014 & uid_year~=.
replace everapply=0 if everapply==.
replace jobcardm =0 if jobcardm==2
replace jobcardm =0 if jobcardm==.


label def reason_not_applied 1"Wage is low" 2"Process is tedius" 3"Have other job" 4"Because of prestige" 5"Applied but not rejected" 6"Not aware whome to contact" 7"Not allowed to work outside" 8" No one to take care of children" 9"Household Work" 10"Recently Married" 11"Too old to apply" 12"Student"
label val reason_not_applied reason_not_applied

replace reason_not_work=9 if reason_not_work==. & everwork==0
replace reason_not_work=1 if reason_not_work==.
recode reason_not_work 2/6=2 8=2  7=3 9/10=3  11=1  12=4 15=4 13=5 14=6
 /* 1 = wage is low 2=Workplace is far/No work available 4 = Domestic work(hh work, kids,) 5=Too old/young 6 = do not have jobcard */
tab reason_not_work, g(rnwx)


ren   q9_14 formalapp
replace formalapp=0 if formalapp==3|formalapp==. 
replace formalapp=1 if formalapp==1|formalapp==2 
**************************
ren q9_141  appdocument
ren q9_142  allowance
tab  allowance
ren q9_14a  indvnregwk
replace indvnregwk=0 if indvnregwk==.
ren q9_15   wkmore
ren q9_16   resnoapply
gen savingacc=q9_17a ==1| q9_20==1
ren q9_20   nregacc 
ren q9_17b  postacc
ren q9_21   socialaudit
ren q9_22   wkbenefit

*NREGS Days by seasons
for var q9_30 q9_34 q9_38:replace X=. if X==0
gen nregwks1=q9_30~=.
gen nregwks2=q9_34~=.
gen nregwks3=q9_38~=.
ren q9_30   nregdays1
ren q9_34   nregdays2
ren q9_38   nregdays3
ren q9_31   nreginc1
ren q9_35   nreginc2
ren q9_39   nreginc3
ren q9_32   morewks1
ren q9_36   morewks2
ren q9_40   morewks3
ren q9_33   moredays1
ren q9_37   moredays2
ren q9_41   moredays3

gen nregwages1 =nreginc1/nregdays1
gen nregwages2 =nreginc2/nregdays2
gen nregwages3 =nreginc3/nregdays3

//checl with Anupam and Jp singh whether it entry error or reporting error-veryfy NREGS module
replace nregwages1=nregwages1/100 if nregwages1>=10000
replace nregwages1=nregwages1/10  if nregwages1>=1000
replace nregwages2=nregwages2/100 if nregwages2>=10000
replace nregwages2=nregwages2/10  if nregwages2>=1000
replace nregwages3=nregwages3/100 if nregwages3>=10000
replace nregwages3=nregwages3/10  if nregwages3>=1000

drop nreginc1 nreginc2 nreginc3
gen  nreginc1=nregwages1*nregdays1
gen  nreginc2=nregwages2*nregdays2
gen  nreginc3=nregwages3*nregdays3
********************************************************************************

egen nregdays=rsum(nregdays1 nregdays2 nregdays3)
egen nreginc=rsum(nreginc1 nreginc2 nreginc3)
egen nregwage=rmean(nregwages1 nregwages2 nregwages3)

replace nregdays=. if nregdays==0
gen  morewks=morewks1==1|morewks1==1|morewks1==1
replace  morewks=. if nregdays==.
replace nreginc=. if nreginc==0
***************************************
drop q*


label 	var	uid	        "Has UID #"
label 	var	everapply	"If you dont have job card, Did you ever applied for it"
label 	var	jobcardm	"Has jobcard/listed on a jobcard"
label 	var	everwork	"Ever wanted to work in MNREGS"
label 	var	rnwx1	    "Reason for not wanted o work, wage too low"
label 	var	rnwx2	    "Reason for not wanted o work,, work site is far/no work available"
label 	var	rnwx3	    "Reason for not wanted o work,, domestic duties (inc hh work, child care etc)"
label 	var	rnwx4	    "Reason for not wanted o work,, too old/young/student"
label 	var	rnwx5	    "Reason for not wanted o work,, do not have job card"
label 	var	rnwx6	    "Reason for not wanted o work,, have other job"
label 	var	formalapp	"Applied for work formaly"
label 	var	appdocument	"Application documented"
label 	var	allowance	"Received unemployment allowance"
label 	var	indvnregwk	"Worked for MNREGS last 12 months"
label 	var	wkmore	    "Wanted to work more"
label 	var	savingacc	"Have bank savings account"
label 	var	nregacc	    "Account opend under MNREGS"
label 	var	postacc	    "Have post office savings account"
label 	var	socialaudit	"Ever participated in social audit"
label 	var	wkbenefit	"Work benefited my productivity"
label 	var	nregwks1	"Worked for MNREGS Feb-Jun 2013"
label 	var	nregdays1	"NO. of days worked for MNREGS Feb-Jun 2013"
label 	var	nregwages1	"Wage received Feb-Jun 2013"
label 	var	morewks1	"Would have wanted to work more Feb-Jun 2013"
label 	var	moredays1	"# of days wanted to work Feb-Jun 2013"
label 	var	nregwks2	"Worked for MNREGS Jul-Nov. 2013"
label 	var	nregdays2	"No.of days worked for MNREGS Jul-Nov. 2013"
label 	var	nregwages2	"wage received Jul-Nov. 2013"
label 	var	morewks2	"Would have wanted to work more Jul-Nov. 2013"
label 	var	moredays2	"# of days wanted to work Jul-Nov. 2013"
label 	var	nregwks3	"Worked for MNREGS Dec-Apr. 2014"
label 	var	nregdays3	"NO. of days worked for MNREGS Dec-Apr. 2014"
label 	var	nregwages3	"wage received Dec-Apr. 2014"
label 	var	morewks3	"Would have wanted to work more Dec-Apr. 2014"
label 	var	moredays3	"# of days wanted to work Dec-Apr. 2014"
label   var nreginc1   "MNREGS income in season 1"
label   var nreginc2   "MNREGS income in season 2"
label   var nreginc3   "MNREGS income in season 3"
label   var morewks    "Would you have wanted to work more"
label   var nregdays   "Total no of mnrega days"
label   var nreginc    "MNREGA income Total"
label   var nregwage "Average wage under NREGS across three seasons"
drop si
********************************************************************************
drop if state==4 //2 observatios for Gujarat

save $wkdata\MNREGS_activities,replace

