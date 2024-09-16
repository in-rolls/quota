clear
clear matrix
set more off

*===============================================================================
*Household regressions
*===============================================================================
use $wkdata\NREGS_RES_HH_REG,clear
gen landowner=land>0&land~=.
recode caste 2=1 3=2 4=3
tab caste, gen(cst)
egen  Tnregday=rsum(Mnregday Fnregday)
gen Hday100= Tnregday>=100

for var Tnregday Mnregday Fnregday: gen lnX=ln(X)
for var Tnregday Mnregday Fnregday: replace lnX=0 if lnX==.
gen shfdays=Fnregday/Tnregday
replace shfdays=0 if shfdays==.
replace lnhage=0 if lnhage==.
replace hsex=0 if hsex==.
replace caste=1 if caste==.
***=================================
**Member level regression
label var res_w1 "Res. now"
label var res_w2 "Res. before"
*Reserved twice
gen res_tw=res_w1*res_w2
*Ever reserved
gen res_w=res_w1==1|res_w2==1
 **Member level regression
label var res_w1 "Res. now"
label var res_w2 "Res. before"
label var res_w "Reserved"
label var res_tw "Res. twice"
********************************************************************************
global xvar "i.religion i.caste  hsex lnhage lnmaxedu lnhhsize fworker fcratio landowner"
global pchar "pedu i.pcaste i.preligion"
global villvar "vsc vst vobc vhindu vmuslim lnpop proad lndistTown"
replace jobcard=0 if jobcard==2

areg jobcard res_w1 res_w2  $xvar  $villvar $pchar, absorb(district) robust
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Reg_household.xls, replace auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, HH Control, Yes,Pradhan Characteristics, Yes, Village Characteristics,Yes) nocons


areg lnTnregday res_w1 res_w2  $xvar $pchar $villvar  if jobcard==1, absorb(district) robust
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Reg_household.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, HH Control, Yes,Pradhan Characteristics, Yes, Village Characteristics,Yes) nocons


areg shfdays res_w1 res_w2  $xvar $pchar $villvar  if jobcard==1, absorb(district) robust
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Reg_household.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, HH Control, Yes,Pradhan Characteristics, Yes, Village Characteristics,Yes) nocons


areg Hday100 res_w1 res_w2  $xvar $pchar $villvar  if jobcard==1, absorb(district) robust
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Reg_household.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, HH Control, Yes,Pradhan Characteristics, Yes, Village Characteristics,Yes) nocons
