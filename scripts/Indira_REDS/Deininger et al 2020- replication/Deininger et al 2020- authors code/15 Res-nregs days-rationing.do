clear
clear matrix

*===============================================================================
use $wkdata\NREGS_RES_MEM_REG,clear
gen lnpedu =ln(pedu)
replace lnpedu=0 if lnpedu==.

gen res_w1_f1=res_w1*(sex==1)
gen res_w1_m1=res_w1*(sex==0)
gen res_w2_f2=res_w2*(sex==1)
gen res_w2_m2=res_w2*(sex==0)


*Reserved twice
gen res_tw=res_w1*res_w2
gen res_tw_f=res_w1_f1*res_w2_f2
gen res_tw_m=res_w1_m1*res_w2_m2

*Ever reserved
gen res_w=res_w1==1|res_w2==1
gen res_w_f=res_w*(sex==1)
gen res_w_m=res_w*(sex==0)

 
**Member level regression
label var res_w1 "Res. now"
label var res_w2 "Res. before"
label var res_w1_f1 "Res. now * female"
label var res_w2_f2 "Res. before* female"
label var res_w1_m1 "Res. now * male"
label var res_w2_m2 "Res. before* male"

label var res_w "Reserved"
label var res_w_f "Reserved * female"
label var res_w_m "Reserved * male"


label var res_tw "Res. twice"
label var res_tw_f "Res. twice * male"
label var res_tw_m "Res. twice * male"

*******************************
****=====
for var postacc nregacc savingacc: replace X=0 if X==2
replace nregacc=0 if nregacc==.
replace  jobcard=0 if jobcard==2
gen landowner=land>0
keep if jobcard==1

recode everwork 2=0 
gen nofairtreat=rnwx3 ==1
gen bribe=reason_not_gettingcard==3


**Reason for not working 
gen no_contact=reason_not_applied==6
gen tedius=reason_not_applied==2

*Interaction terms if  panchayat is reserved twice and female members
gen res_w22= res_w1 *res_w2
gen res_w_f22= res_w1_f1 *res_w2_f2

for var jobcardm everwork formalapp appdocument socialaudit allowance:replace X=0 if X==2 
for var jobcardm everwork formalapp appdocument socialaudit allowance:replace X=0 if X>12 

save $tmp\FINAL_RES_MEM_REG,replace



*===============================================
use $tmp\FINAL_RES_MEM_REG,clear
 gen rationing=extr==1|intr==1
for var nregdays nregdays3 nregdays2 nregdays1 moredays: gen lnX=ln(X)
for var nregdays nregdays3 nregdays2 nregdays1 moredays: replace  lnX=0 if lnX==.
global xvar "sex lnage lnage2  lnedu lnedu2 i.religion i.caste  hsex lnhage lnmaxedu lnhhsize fworker fcratio landowner"
global pchar "pedu i.pcaste i.preligion"
global villvar "vsc vst vobc voc vhindu vmuslim lnpop proad lndistTown"




areg nregwks res_w1 res_w2 res_w1_f1 res_w2_f2  , absorb(district) cluster(villageid)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender_no_controls.xls, replace auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg lnnregdays res_w1 res_w2 res_w1_f1 res_w2_f2  , absorb(district) cluster(villageid)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender_no_controls.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg rationing res_w1 res_w2 res_w1_f1 res_w2_f2   , absorb(district) cluster(villageid) 
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender_no_controls.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons

areg extr res_w1 res_w2 res_w1_f1 res_w2_f2  , absorb(district) cluster(villageid)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender_no_controls.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg intr res_w1 res_w2 res_w1_f1 res_w2_f2   , absorb(district) cluster(villageid)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender_no_controls.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



***NREGWK ********NREGSDAYS

areg nregwks res_w1 res_w2  $xvar  , absorb(district) robust cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Res_nregs_day.xls, replace auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg lnnregdays res_w1 res_w2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Res_nregs_day.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg rationing res_w1 res_w2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Res_nregs_day.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg extr res_w1 res_w2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Res_nregs_day.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg intr res_w1 res_w2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)

test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)
outreg2 using $out\Res_nregs_day.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


**========================================
areg nregwks res_w1 res_w2 res_w1_f1 res_w2_f2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender.xls, replace auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg lnnregdays res_w1 res_w2 res_w1_f1 res_w2_f2  $xvar $pchar $villvar , absorb(district) robust cluster(village)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg rationing res_w1 res_w2 res_w1_f1 res_w2_f2  $xvar $pchar $villvar , absorb(district) robust cluster(village)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons

areg extr res_w1 res_w2 res_w1_f1 res_w2_f2  $xvar $pchar $villvar , absorb(district) robust cluster(village)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg intr res_w1 res_w2 res_w1_f1 res_w2_f2  $xvar $pchar $villvar , absorb(district) robust cluster(village)
estadd ysumm
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)
outreg2 using $out\Res_nregs_day_gender.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd), JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


**meeting related to NREGS etc.


areg gmeet res_w1 res_w2  $xvar  , absorb(district)  cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)
test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)

outreg2 using $out\Governance_all.xls, replace auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg gmeetno res_w1 res_w2  $xvar  , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)
test res_w1 +res_w2=0
local f1=r(F)
local p1=r(p)

outreg2 using $out\Governance_all.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg solved res_w1 res_w2  $xvar $pchar $villvar , absorb(district) cluster(village)
estadd ysumm
lincom res_w1 +res_w2
local est= r(estimate)
local ese= r(se)
test res_w1+ res_w2=0
local f1=r(F)
local p1=r(p)

outreg2 using $out\Governance_all.xls, append auto(3)  bdec(3) label ///
addstat(Dep. Var Mean, e(ymean),Dep. Var Std, e(ysd),JointEst, `est', JointSE, `ese', JointFtest, `f1', Pval, `p1')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



//by gender

areg gmeet res_w1 res_w1_f1 res_w2  res_w2_f2   $xvar $pchar $villvar  , absorb(district) cluster(village)
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)



outreg2 using $out\Governance_all_gender.xls, replace auto(3)  bdec(3) label ///
addstat(JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons



areg gmeetno res_w1 res_w1_f1 res_w2  res_w2_f2   $xvar $pchar $villvar   , absorb(district) cluster(village)
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)



outreg2 using $out\Governance_all_gender.xls, append auto(3)  bdec(3) label ///
addstat(JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons


areg solved res_w1 res_w1_f1 res_w2  res_w2_f2  $xvar $pchar $villvar , absorb(district) cluster(village)
lincom res_w1 +res_w1_f1
local est1= r(estimate)
local ese1= r(se)

lincom  res_w2+ res_w2_f2
local est2= r(estimate)
local ese2= r(se)


lincom res_w1 +res_w1_f1+ res_w2+ res_w2_f2
local est= r(estimate)
local ese= r(se)


test res_w1 +res_w1_f1=0
local f1=r(F)
local p1=r(p)

test  res_w2+ res_w2_f2=0
local f2=r(F)
local p2=r(p)

test res_w1 +res_w1_f1+ res_w2+ res_w2_f2=0
local f3=r(F)
local p3=r(p)



outreg2 using $out\Governance_all_gender.xls, append auto(3)  bdec(3) label ///
addstat(JointEst1, `est1', JointSE1, `ese1',JointEst2, `est2', JointSE2, `ese2', JointEst, `est', JointSE, `ese', F1, `f1', Pval1, `p1',F2, `f2', Pval2, `p2', F, `f3', Pval, `p3')  ///
drop($xvar $pchar $villvar) ///
addtext(District FE, Yes, Individual Control, Yes, HH Control, Yes,Pradhan Characteristics, Yes) nocons
