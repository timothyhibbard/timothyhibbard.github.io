clear
use /Users/timothyhibbard/Desktop/Research/JudgesData/Panel.dta

*****************************************************
****** Generate, Rename, and Recode Variiables ******
******************************************************
gen casetype = dataset
egen ds =max(dataset), by(dataset)
recode ds 10=8 11=9 12=10 13=11 14=12 15=13 16=14
by casetype, sort: gen case1 = _n==1
gen caseU = casetype if case1==1
* WEIRDNESS
*list caseU dataset ds if caseU!=.
*recode dataset 10=8 11=9
*list caseU dataset ds if caseU!=.

gen sd = ds==11
gen num = 1
gen co = case_outcome
recode co 0=1 1=0
rename party_pres pnp
gen party = pnp
recode party 0=1 1=0
encode cite, gen(pan)
by pan, sort: gen panU = _n==1

label drop judge_number
label drop sex
label drop pan

rename conserve_vote cv
rename judge_name judge
rename judge_number jn
rename sex female
egen fempan=max(female), by(pan)
rename lc_conserve lc_cv
rename Minority min
rename Majority maj
rename dec_year year
rename birth_yr age
*label define party_pres 0 "Democrat", modify
*label define party_pres 1 "Republican", modify

by jn, sort: gen jnU = _n==1
bysort pan: egen numjudge=count(_n)
bysort pan: egen numfem =sum(female)
bysort jn: egen numcase=sum(num)
bysort jn: egen meancv=mean(cv)

bysort jn: egen conIDvote=max(1) if cv==1 &jcs>0
bysort jn: egen libIDvote=max(1) if cv==0 &jcs<0
recode conIDvote .=0 if cv!=.
recode libIDvote .=0 if cv!=.
gen IDvote=conIDvote+libIDvote

***********************************************
****** persuasive index (PI) using loops ******
***********************************************
*****				 *****
*** Create denomenator ***
*****				 *****
set more off
forvalues i = 1/14 {
	gen CONovOP_`i' = 1 ==(jcs>0 &lc_cv ==0 &ds !=`i')
	gen LIBovOP_`i' = 1 ==(jcs<0 &lc_cv ==1 &ds !=`i')
	gen ovOP_`i' = CONovOP_`i' + LIBovOP_`i'
	gen MINovOP_`i' = 1 ==(ovOP_`i' == 1 & min == 1)
	egen sumMINovOP_`i' = total(MINovOP_`i'), by(jn)
*****		       *****
*** Create numerator ***
*****		       *****
	gen CONpsit_`i' = 1 ==(co==1 &lc_cv==0 &cv==1 &jcs>0 &min==1 &ds !=`i')
	gen LIBpsit_`i' = 1 ==(co==0 &lc_cv==1 &cv==0 &jcs<0 &min==1 &ds !=`i')
	gen psit_`i' = CONpsit_`i' + LIBpsit_`i'
	egen sumpsit_`i' = total(psit_`i'), by(jn)
*****		*****
*** Create PI ***
*****		*****
	gen PI_`i' = sumpsit_`i'/sumMINovOP_`i' if sumMINovOP_`i'>0
*****			  *****
*** Create mfPI ***
*****			  *****
	gen femPI_`i' = PI_`i' if female==1
	egen mfPI_`i' = max(femPI_`i'), by(pan)
	
	drop CONovOP_`i' LIBovOP_`i' ovOP_`i' MINovOP_`i' sumMINovOP_`i'
	drop CONpsit_`i' LIBpsit_`i' psit_`i' sumpsit_`i'
	drop femPI_`i'
					}

****************************************************
****** persuadability index (pdi) using loops ******
****************************************************
*****				 *****
*** Create denomenator ***
*****				 *****
set more off
forvalues i = 1/14 {							
	gen CONupOP_`i' = 1 ==(jcs>0 &lc_cv==1 &ds !=`i')
	gen LIBupOP_`i' = 1  ==(jcs<0 &lc_cv==0 &ds !=`i')
	gen upOP_`i' = CONupOP_`i' + LIBupOP_`i'
	gen MAJupOP_`i' = 1 ==(upOP_`i' ==1 &maj==1)
	egen sumMAJupOP_`i' = total(MAJupOP_`i'), by(jn)
*****		       *****
*** Create numerator ***
*****		       *****
	gen CONav_`i' = 1 ==(cv==0 &jcs>0 &maj==1 &lc_cv==1 &ds !=`i')
	gen LIBav_`i' = 1 ==(cv==1 &jcs<0 &maj==1 &lc_cv==0 &ds !=`i')
	gen av_`i' = CONav_`i' + LIBav_`i'
	egen sumav_`i' = total(av_`i'), by(jn)
*****		 *****
*** Create pdi ***
*****		 *****
	gen pdi_`i' = sumav_`i'/sumMAJupOP_`i' if sumMAJupOP_`i'>0
	
	drop CONupOP_`i' LIBupOP_`i' upOP_`i' MAJupOP_`i' sumMAJupOP_`i'			
	drop CONav_`i' LIBav_`i' av_`i' sumav_`i'
					}
					
*************************************************
****** standardizing variables using loops ******
*************************************************
set more off
forvalues i = 1/14 {
*****	   *****
*** mfPI ***
*****	   *****
	summarize mfPI_`i'
	return list
	gen mn_mfPI_`i' = r(mean)
	gen sd2_mfPI_`i' = 2*r(sd)
	gen mfPI_2SD_`i' = (mfPI_`i'-mn_mfPI_`i')/sd2_mfPI_`i'
**********
*** PI ***
**********					
	summarize PI_`i'
	return list
	gen mn_PI_`i' = r(mean)					
	gen sd2_PI_`i' = 2*r(sd)				
	gen PI_2SD_`i' = (PI_`i'-mn_PI_`i')/sd2_PI_`i'			
***** *****
*** pdi ***
***** *****					
	summarize pdi_`i'				
	return list
	gen mn_pdi_`i' = r(mean)					
	gen sd2_pdi_`i' = 2*r(sd)				
	gen pdi_2SD_`i' = (pdi_`i'-mn_pdi_`i')/sd2_pdi_`i'				
					}
forvalues i = 1/14 {					
		drop mn_mfPI_`i' sd2_mfPI_`i' 
		drop mn_PI_`i' sd2_PI_`i'
		drop mn_pdi_`i' sd2_pdi_`i'
				    }
*****  *****
*** year ***
*****  *****
summarize year
return list
gen mn_yr = r(mean)
gen sd2_yr = 2*r(sd)
gen yr_2SD = (year-mn_yr)/sd2_yr
drop mn_yr sd2_yr 

***** *****
*** age ***
***** *****
summarize age
return list
gen mn_age = r(mean)
gen sd2_age = 2*r(sd)
gen age_2SD = (age-mn_age)/sd2_age
drop mn_age sd2_age

***** *****
*** jcs ***
***** *****
summarize jcs
return list
gen mn_jcs = r(mean)
gen sd2_jcs = 2*r(sd)
gen jcs_2SD = (jcs-mn_jcs)/sd2_jcs
drop mn_jcs sd2_jcs 

*********************************************				
****** categorize mfPI using loops ******
*********************************************
set more off
forvalues i = 1/14 {
gen mfPI3_`i' = 0
recode mfPI3_`i' 0=1 if mfPI_2SD_`i' <= 0 & mfPI_2SD_`i' !=.
recode mfPI3_`i' 0=2 if mfPI_2SD_`i' >= 0 & mfPI_2SD_`i' !=.
recode mfPI3_`i' 0=0 if mfPI_`i' ==. 					
	
	gen amp_`i'= mfPI3_`i' if mfPI3_`i' ==0
	recode amp_`i' .=0 if mfPI3_`i' !=.
	gen upw_`i'= mfPI3_`i' if mfPI3_`i' ==1
	recode upw_`i' .=0 if mfPI3_`i' !=.
	gen pw_`i'= mfPI3_`i' if mfPI3_`i' ==2
	recode pw_`i' .=0 if mfPI3_`i' !=.
				    }
set more off
#delimit ;					
********************************************
****** label new variables with loops ******
********************************************
;
label variable sd "1 if sex discrimination case, 0 if not" ;
label variable co "case outcome, 1 is conservative, 0 is liberal" ;
label variable panU "Unique panel" ;
label variable pan "panel";
label variable jnU "Unique judge" ;
label variable numfem "number of female judges on panel" ;
label variable numjudge "number of judges on panel" ;
label variable IDvote
"ideological vote, 1 is vote accoriding to ideology, 0 is vote against" ;
label variable fempan "panel has a female on it" ;
forvalues i = 11/11 { ;
label variable pdi_`i' "persuadable index" ;
label variable PI_`i' "persuasive index" ;
label variable mfPI_`i' "PI of most persuasive female on the panel" ;
					} ;
**************************************
****** drop unnessary variables ******
**************************************
 ;
drop judge_vote rev_lib rev_cons RR DR DD Fr_Rep_Jud No_Ideology Yung def_con 
lct_dir tenure elevateDC mod_con mod_lib def_lib judge_num app_j st_birth race 
religion lawschool jud_exp president pres_cs confirm_yr type_crime criminal 
judge_age sr_judge over65 jcs_circuit jcs_NEW party_pres1 jcs1 jcs_NEW1 
party_pres2 jcs2 jcs_NEW2 party_pres3 jcs3 jcs_NEW3 avgJCS_New avgJCS_NEW_WO 
Mixed judge_no no_R no_D job Act_judges fraction_R Fraction jcs_421_D 
jcs_422_267_D jcs_266_D R_jcs jcs_218_R jcs_218_502_R jcs_503_R sr_yr term_yr 
reverse_lib_dc reverse_con_dc rev_ratio_lc pcon_lib_dct pcon_con_dct pcon 
circuit_rev_ratio _Icircuit_2 _Icircuit_3 _Icircuit_4 _Icircuit_5 _Icircuit_6 
_Icircuit_7 _Icircuit_8 _Icircuit_9 _Icircuit_10 _Icircuit_11 _Icircuit_12 
_Idataset_2 _Idataset_3 _Idataset_4 _Idataset_5 _Idataset_6 _Idataset_7 
_Idataset_10 _Idataset_11 _Idataset_12 _Idataset_13 _Idataset_14 _Idataset_15
_Idataset_16 _Idec_year_1996 _Idec_year_1997 _Idec_year_1998 _Idec_year_1999
_Idec_year_2000 _Idec_year_2001 _Idec_year_2002 _Idec_year_2003
_Idec_year_2004 _Idec_year_2005 _Idec_year_2006 _Idec_year_2007
_Idec_year_2008 gov_atty sum numcase meancv conIDvote 
libIDvote ;



*drop PI_8 mfPI_8 pdi_8 mfPI_2SD_8 PI_2SD_8 pdi_2SD_8 mfPI3_8
*PI_9 mfPI_9 pdi_9 mfPI_2SD_9 PI_2SD_9 pdi_2SD_9 mfPI3_9 ;

save "/Users/timothyhibbard/Desktop/Research/JudgesData/women judges/sd/Panel.dta", replace ;
sort ds ;
list dataset ds if caseU!=. ;
drop dataset caseU case1 ;

*****************************
****** LIST CASE TYPES ******
*****************************
* 1  = abortion 
* 2  = ada
* 3  = affirm_action
* 4  = age
* 5  = campaign_finance	
* 6  = cap_punish
* 7  = contract clause
* 8 = epa
* 9 = federalism
* 10 = pcv (piercing the corporate veil
* 11 = sex_discrimination
* 12 = sex_harassment
* 13 = takings clause
* 14 = title_vii (race-discrimination) ;
