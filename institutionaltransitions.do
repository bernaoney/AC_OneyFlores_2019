

/******************************************/
/************** Bring data *****************/
/******************************************/

set seed 7777777

use fh_transitions_data2.dta, clear

/******************************************/
/************** Variables *****************/
/******************************************/

/* 6 CASES AND THEORY: REGTYPE: free(1), partly free(2), not free(3)*/
/* partly free(2)->free(1)...destination==1*/
/* partly free(2)->not free(3)...destination==4*/

/* free(1)->partly free(2)...destination==3*/
/* free(1)->not free(3)...destination==5*/

/* not free(3)->partly free(2)...destination==2*/
/* not free(3)->free(1)...destination==? No cases recorded! */


/*New total duration*/
encode iso3,gen(ctry_code)
sort ctry_code regtype year
by ctry_code regtype: gen tot_dur=_n
label variable tot_dur "Duration of the country-spell"

//Unit of analysis is the country-spell
egen ctry_spell=group(ctry_code regtype)
order country ctry_code iso3c year regtype ctry_spell tot_dur failfr regchangefr durationfr destination tot_dur

sort ctry_spell year
by ctry_spell: gen interptn_length=year-year[_n-1]
order interptn_length, after(ctry_spell)
replace interptn_length=0 if interptn_length==1
label variable interptn_length "Number of years this country-spell has been interrupted"
sort ctry_spell year
by ctry_spell: gen interptn_bin=(interptn_length!=0) if interptn_length!=.
by ctry_spell: egen interptn_num=total(interptn_bin)
order interptn_bin interptn_num, after(interptn_length)
label variable interptn_bin "Was there an interruption of this country-spell? Y/N"
label variable interptn_num "Total number of interruptions for this country-spell: measure of instit weakness"
//Clearly, if interptn_num==2, this means this is the 3rd spell of regtype because the regime has been interupted twice
sort ctry_spell year
sort ctry_code year 

/******************************************/
//Number of countries and years
preserve
duplicates drop ctry_code, force
list ctry_code
restore

sum year

//Number if country-spells (ie unit if analysis)
tab ctry_spell

//Number if country-spells (ie unit if analysis) by country
preserve
sort ctry_code ctry_spell
by ctry_code ctry_spell: gen obs=_n
keep if obs==1
tab ctry_code
sort ctry_code
by ctry_code: egen countregimes=count(obs)
order obs countregimes
sum countregimes,detail
egen mode=mode(countregimes)
sum mode
drop obs countregimes mode
restore

/******************************************/
/************ Liberalization ***************/
/******************************************/

/******************************************/
/************ Destination 1 ***************/
/******************************************/

sort ctry_code year

/* partly free(2)->free(1)...destination==1. Lesotho, Brazil, Indonesia*/

//* DESTINATION 1 - Partly Free - Free*/

//total number of this type of transitions:19
gen dest1=1 if destination==1 & failfr==1
tab dest1

//Gen new failure (this is the destination)
gen fail1=0
replace fail1=1 if failfr[_n+1]==1 & destination[_n+1]==1 & ctry_code==ctry_code[_n+1]
tab fail1

//Drop regtype==3. Not the transition (3->1) we are interested in
//drop if regtype==3
//Drop regtype==1. Not the transition (1->1) we are interested in
//drop if regtype==1

//Unit of analysis is the country-spell. Ignore regtype==3 and regtype==1 as above
//Ghana/Croatia/Mexico/Suriname has dest==1 in 1st obs. Accounts for difference in 19 vs 15 failures
stset tot_dur if regtype==2, failure(fail1==1) id(ctry_spell) exit(time .) 
stdes
order country ctry_code iso3c year regtype ctry_spell tot_dur destination _st _d _t _t0 dest1 fail1 failfr

//Use of interptn_length is problematic, possibly endogenous. Ignore.

//Highly non-prop
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
estat phtest, detail

gen ln_time=ln(tot_dur+1)
gen ln_time_itu=ln_time*itu
gen ln_time_diffusion=ln_time*diffusion
gen ln_time_fpress=ln_time*fpress
gen ln_time_loggdp=ln_time*loggdp
gen ln_time_gdp_annual=ln_time*gdp_annual
gen ln_time_coercivecap=ln_time*coercivecap
gen ln_time_interptn_num=ln_time*interptn_num

stcox itu ln_time_itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num ln_time_diffusion ln_time_fpress ///
 ln_time_loggdp ln_time_gdp_annual ln_time_coercivecap ln_time_interptn_num, ///
 nohr vce(cluster ctry_spell)

//Internet significant with mobilization: increase h(t) but fall over time
lincom c.itu+ln_time_itu+c.itu#0.popmob
lincom c.itu+.693*ln_time_itu+c.itu#0.popmob
lincom c.itu+1*ln_time_itu+c.itu#0.popmob
lincom c.itu+1.5*ln_time_itu+c.itu#0.popmob
lincom c.itu+2.833*ln_time_itu+c.itu#0.popmob

lincom c.itu+ln_time_itu+c.itu#1.popmob
lincom c.itu+.693*ln_time_itu+c.itu#1.popmob
lincom c.itu+1*ln_time_itu+c.itu#1.popmob
lincom c.itu+1.5*ln_time_itu+c.itu#1.popmob
lincom c.itu+2.833*ln_time_itu+c.itu#1.popmob

//Pop mobilization and internet does: increase h(t) then insignificant
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob
 
//No difference
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system, nohr shared(ctry_spell) forceshared
 
//* Generating lagged covariates

sort country year
by country: generate itu_L1=itu[_n-1]
by country: generate popmob_L1=popmob[_n-1]

//Lags. Fails: does not converge.
stcox l_itu i.class i.l_popmob i.class##i.l_popmob i.l_popmob##c.l_itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
 
stset, clear


***********************/// MARKOV TRANSITION///**************************
tsset ctry_code year

*Gen new dummy variable for regime: 0 for partly free(2) and 1 for free(1)...destination==1
*Previously, we did not have enough obs because we were focusing on transitions, and there
*were very few of them (2->1), but this is not the correct coding. The correct one
*is 0s for partly free and 1s for free and not for the transitions between them
gen reg1=0 if regtypefr==2
replace reg1=1 if regtypefr==1
order reg1, after(regtypefr)
tab reg1

*First observation is deleted; this could happen in 2000 or anytime between 2000-2017 (eg new regime)
gen first1=0 if reg1!=.
replace first1=1 if year==2000 & reg1!=.
replace first1=1 if reg1!=. & l.reg1==. & ctry_code==l.ctry_code
order first1,after(reg1)

*AQF: not enough transitions
tab reg1
probit reg1 if 1.reg1==1 & first1!=1
probit reg1 if 1.reg1==0 & first1!=1

*This approach may work:
gen lreg1_itu=1.reg1* itu
probit reg1 itu 1.reg1 lreg1_itu if first1!=1

tsset,clear
**************************************************************************

/******************************************/
/************ Destination 2 ***************/
/******************************************/

/* not free(3)->partly free(2)...destination==2*/

//* DESTINATION 2 - Not Free - Party Free 

//total number of this type of transitions:30
gen dest2=1 if destination==2 & failfr==1
tab dest2

//Gen new failure (this is the destination)
gen fail2=0
replace fail2=1 if failfr[_n+1]==1 & destination[_n+1]==2 & ctry_code==ctry_code[_n+1]
tab fail2

//Drop regtype==3. Not the transition (3->1) we are interested in
//drop if regtype==3
//Drop regtype==1. Not the transition (1->1) we are interested in
//drop if regtype==1

//Unit of analysis is the country-spell. Ignore regtype==3 and regtype==1 as above
//Congo Braz has dest==2 in 1st obs, Venezula has typo in failfr==1? Accounts for difference in 30 vs 28 failures
stset tot_dur if regtype==3, failure(fail2==1) id(ctry_spell) exit(time .) 
stdes
order country ctry_code iso3c year regtype ctry_spell tot_dur destination _st _d _t _t0 dest2 fail2 failfr

stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
estat phtest, detail

//Internet not significant
lincom c.itu+c.itu#0.popmob
lincom c.itu+c.itu#1.popmob

//Pop mobilization not significant
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob

//NA
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system, nohr shared(ctry_spell) forceshared

//Lags. Fails: missing SEs.
stcox l_itu i.class i.l_popmob i.class##i.l_popmob i.l_popmob##c.l_itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
 
stset, clear


***********************/// MARKOV TRANSITION///**************************
tsset ctry_code year

*Gen new dummy variable for regime: 0 for (3) and 1 for free(2)...destination==2

gen reg2=0 if regtypefr==3
replace reg2=1 if regtypefr==2
order reg2, after(regtypefr)
tab reg2

*First observation is deleted; this could happen in 2000 or anytime between 2000-2017 (eg new regime)
gen first2=0 if reg2!=.
replace first2=1 if year==2000 & reg2!=.
replace first2=1 if reg2!=. & l.reg2==. & ctry_code==l.ctry_code
order first2,after(reg2)

*AQF: not enough transitions
tab reg2
probit reg2 itu if 1.reg2==1 & first2!=1
probit reg2 itu if 1.reg2==0 & first2!=1

*This approach may work:
gen lreg2_itu=1.reg2* itu
probit reg2 itu 1.reg2 lreg2_itu if first2!=1


tsset,clear
**************************************************************************

/******************************************/
/************ Backsliding ***************/
/******************************************/

/******************************************/
/************ Destination 3 ***************/
/******************************************/
/******************************************/

/* free(1)->partly free(2)...destination==3*/

//* DESTINATION 3 - Free - Party Free 

//total number of this type of transitions:18
gen dest3=1  if destination==3 & failfr==1
tab dest3

//Gen new failure (this is the destination)
gen fail3=0
replace fail3=1 if failfr[_n+1]==1 & destination[_n+1]==3 & ctry_code==ctry_code[_n+1]
tab fail3

//Drop regtype==3. Not the transition (3->1) we are interested in
//drop if regtype==3
//Drop regtype==1. Not the transition (1->1) we are interested in
//drop if regtype==1

//Unit of analysis is the country-spell. Ignore regtype==3 and regtype==1 as above
//Ecuador/Fiji/Solomon has dest==3 in 1st obs. Accounts for difference in 18 vs 15 failures
stset tot_dur if regtype==1, failure(fail3==1) id(ctry_spell) exit(time .) 
stdes
order country ctry_code iso3c year regtype ctry_spell tot_dur destination _st _d _t _t0 dest3 fail3 failfr

stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
estat phtest, detail

//Internet significant with mobilization: decrease h(t) 
lincom c.itu+c.itu#0.popmob
lincom c.itu+c.itu#1.popmob

//Pop mobilization and internet does: from positive to negative coefficient
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob
 
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
scalar itu1=r(estimate)
scalar itu1se=r(se)
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
scalar itu2=r(estimate)
scalar itu2se=r(se)
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
scalar itu3=r(estimate)
scalar itu3se=r(se)
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
scalar itu4=r(estimate)
scalar itu4se=r(se)
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob
scalar itu5=r(estimate)
scalar itu5se=r(se)

gen est=.
gen se=.
gen valueitu=_n if _n<=5
replace est = itu1 in 1
replace est = itu2 in 2
replace est = itu3 in 3
replace est = itu4 in 4
replace est = itu5 in 5
replace se = itu1se in 1
replace se = itu2se in 2
replace se = itu3se in 3
replace se = itu4se in 4
replace se = itu5se in 5

gen est_round=round(est,0.01)

serrbar est se valueitu if valueitu<=5, mvopts(mcolor(pink) ///
mlabel(est_round))  plotr(m(vlarge)) lcolor(pink) lwidth(medthick) ///
ytitle("Linear Combination of Coefficients") xtitle(Internet Usage) ///
title("Transition from Free Regime to Partly Free Regime")  ///
subtitle("Effect of Mobilization as a Function of Internet Usage")  ///
scale(1.960) xlabel(1(1)5) yline(0, lwidth(medthick) lcolor(blue) ///
lpattern(dash)) note(95% CIs)

scalar drop _all
drop est se valueitu est_round

//NA
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system, nohr shared(ctry_spell) forceshared

//Lags. Fails: does not converge.
stcox l_itu i.class i.l_popmob i.class##i.l_popmob i.l_popmob##c.l_itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)

 
stset,clear




/******************************************/
/************ Destination 4 ***************/
/******************************************/

/* partly free(2)->not free(3)...destination==4*/

//* DESTINATION 4 - Partly Free - Not Free 

//total number of this type of transitions:19
gen dest4=1  if destination==4 & failfr==1
tab dest4

//Gen new failure (this is the destination)
gen fail4=0
replace fail4=1 if failfr[_n+1]==1 & destination[_n+1]==4 & ctry_code==ctry_code[_n+1]
tab fail4

//Drop regtype==3. Not the transition (3->1) we are interested in
//drop if regtype==3
//Drop regtype==1. Not the transition (1->1) we are interested in
//drop if regtype==1

//Unit of analysis is the country-spell. Ignore regtype==3 and regtype==1 as above
//Haiti/Kyrgyzstan has dest==4 in 1st obs. Accounts for difference in 30 vs 28 failures
stset tot_dur if regtype==2, failure(fail4==1) id(ctry_spell) exit(time .) 
stdes
order country ctry_code iso3c year regtype ctry_spell tot_dur destination _st _d _t _t0 dest4 fail4 failfr

stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
estat phtest, detail

//Lags. Works.
stcox l_itu i.class i.l_popmob i.class##i.l_popmob i.l_popmob##c.l_itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)

//Internet significant with mobilization: decrease h(t) 
lincom c.itu+c.itu#0.popmob
lincom c.itu+c.itu#1.popmob

//Pop mobilization not significant
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob

lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
scalar itu1=r(estimate)
scalar itu1se=r(se)
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
scalar itu2=r(estimate)
scalar itu2se=r(se)
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
scalar itu3=r(estimate)
scalar itu3se=r(se)
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
scalar itu4=r(estimate)
scalar itu4se=r(se)
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob
scalar itu5=r(estimate)
scalar itu5se=r(se)

gen est=.
gen se=.
gen valueitu=_n if _n<=5
replace est = itu1 in 1
replace est = itu2 in 2
replace est = itu3 in 3
replace est = itu4 in 4
replace est = itu5 in 5
replace se = itu1se in 1
replace se = itu2se in 2
replace se = itu3se in 3
replace se = itu4se in 4
replace se = itu5se in 5

gen est_round=round(est,0.01)

serrbar est se valueitu if valueitu<=5, mvopts(mcolor(pink) ///
mlabel(est_round))  plotr(m(vlarge)) lcolor(pink) lwidth(medthick) ///
ytitle("Linear Combination of Coefficients") xtitle(Internet Usage) ///
title("Transition from Partly Free Regime to Not Free Regime")  ///
subtitle("Effect of Mobilization as a Function of Internet Usage")  ///
scale(1.960) xlabel(1(1)5) yline(0, lwidth(medthick) lcolor(blue) ///
lpattern(dash)) note(95% CIs)

scalar drop _all
drop est se valueitu est_round


//No difference
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system, nohr shared(ctry_spell) forceshared
 

 
 
stset, clear



/******************************************/
/************ Destination 5 ***************/
/******************************************/

/* free(1)->not free(3)...destination==5*/
/* Only one observation: Mali (2012-2013)*/

//total number of this type of transitions:1
gen dest5=1  if destination==5 & failfr==1
tab dest5

//Gen new failure (this is the destination)
gen fail5=0
replace fail5=1 if failfr[_n+1]==1 & destination[_n+1]==5 & ctry_code==ctry_code[_n+1]
tab fail5

//Drop regtype==3. Not the transition (3->1) we are interested in
//drop if regtype==3
//Drop regtype==1. Not the transition (1->1) we are interested in
//drop if regtype==1

//Unit of analysis is the country-spell. Ignore regtype==3 and regtype==1 as above
stset tot_dur if regtype==1, failure(fail5==1) id(ctry_spell) exit(time .) 
stdes
order country ctry_code iso3c year regtype ctry_spell tot_dur destination _st _d _t _t0 dest5 fail5 failfr

stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system interptn_num , nohr vce(cluster ctry_spell)
estat phtest, detail

//Internet significant with mobilization: increase h(t) 
lincom c.itu+c.itu#0.popmob
lincom c.itu+c.itu#1.popmob

//Pop mobilization and internet significant: increase h(t)
lincom 1.popmob+c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+5*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+25*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+50*c.itu#1.popmob+1.class#1.popmob
lincom 1.popmob+75*c.itu#1.popmob+1.class#1.popmob

 
//No difference
stcox itu i.class i.popmob i.class##i.popmob i.popmob##c.itu logunemp ///
 i.priorlib diffusion i.GWmember fpress loggdp gdp_annual coercivecap ///
 i.catcpi i.system, nohr shared(ctry_spell) forceshared
 
stset,clear












