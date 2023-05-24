cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use abadie_bol_eh2012, clear 

drop if pea ==0
gen edad_fb= edad - edadhijo_max
*********************************************************************
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 * estimate e[z|x] ;
*probit samesex edad edad_fb hijo1_sexa eje_region etnia_b hijo2_sexa edu_level [pw=perwt], r 
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b
predict econdz 
gen m_econdz = 1 - econdz
*probit job more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [w=factor_2014], r 
******************************************************************************
*columna 1 tabla 2 y 4 tabla 3 
******************************************************************************
****** Y = hourswm;
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons tothrs if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h=what
replace wstar_h=0 if what<=0
summ wstar_h
 drop zhat what

/***** Y = hours0 
en la eh 2012 no tenemos horas trabajadas cero 
gen hours0 = (tothrs<=0)
probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours0 if morekids==1
predict zhat if morekids==1
replace zhat=0 if morekids==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 
gen wstar_h0=what
replace wstar_h0=0 if what<=0
summ wstar_h
drop zhat what */

 ***** Y = hours1 
gen hours1 = (tothrs>0 & tothrs<=10)
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons hours1 if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h1=what
replace wstar_h1=0 if what<=0
summ wstar_h1
drop zhat what

  ***** Y = hours2 ;
gen hours2 = (tothrs>10 & tothrs<=20)
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons hours2 if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h2=what
replace wstar_h2=0 if what<=0
summ wstar_h2
drop zhat what

 ***** Y = hours3 ;
gen hours3 = (tothrs>20 & tothrs<=30)
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons hours3 if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h3=what
replace wstar_h3=0 if what<=0
summ wstar_h3
drop zhat what

 ***** Y = hours4 ;
gen hours4 = (tothrs>30 & tothrs<=40)

probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons hours4 if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h4=what
replace wstar_h4=0 if what<=0
summ wstar_h4
drop zhat what

 ***** Y = hours5;
gen hours5 = (tothrs>40 & tothrs<=50)
probit samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b more2sons hours5 if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_h5=what
replace wstar_h5=0 if what<=0
summ wstar_h5
drop zhat what


* RUN WEIGHTED REGRESSION, using aweights! tabla 2 col 1 y 3 ;
*reg workedm  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_e] 
reg tothrs  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h] 
reg tothrs  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b 
*regress workedm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem (multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem)
regress tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b)
 
*===============================================================*/
 * DISTRIBUTION OF HOURS ; 
* WEIGHTED REG W/ AWEIGHTS ; table 3 col 4 
/*===============================================================*/
*reg hours0  more2sons more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h0] 
reg hours1  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h1] 
reg hours2  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h2] 
reg hours3  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h3] 
reg hours4  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h4] 
reg hours5  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [aw=wstar_h5]

*****************************************************************************
*********** MODELO NO LINEAL tabla 2 (col 5) y 3 (col 5 o 6 ?)
**************************************************************************
probit hours3 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b
probit hours4 more2sons edad edad_fb /*no sig */ 
******************************************************************************
***************************************************************************
*** Causal QTE col 8 en tabla 3 
**********************************************************************
/*===============================================================*/
/* QTE estimator is based on a model where effect of trtmnt &
   covariates is linear and additive at each quantile, so that a single 
   trtmnt effect is estimated;  it is to quantile regression what
   IV is to OLS 					         */
/* I've estimated k (kappa) above, now use the fitted values khat
   (what) to construct the estimator:
   delta_hat = argmin 1/nSUM(1(khat>=1)*khat*rho(Yi - Wi'delta)
             = argmin 1/n SUM (wstar)*rho(Yi - Wi'delta), 
    where rho(lamda) is check fn  = (theta - 1{lamda<0})*lambda */
/*=============================================================*/
* QUANTILE REGRESSION ;
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.5)
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.6)
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.7)
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.75)
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.8)
qreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b [pw=wstar_h], quant(.9)

**********************************************************************
*** QR estimates --- tabla 3 qreg col 7 en tabla 3 
*********************************************************************
summ tothrs, detail
pctile quantval = tothrs, nquantiles(20) genp(quant)
list quantval quant in 1/20

/*gen byte hours0=(hourswm<=0)
gen byte hours10=(hourswm>0 & hourswm<=10)
gen byte hours20=(hourswm>10 & hourswm<=20)
gen byte hours30=(hourswm>20 & hourswm<=30)
gen byte hours40=(hourswm>30 & hourswm<=40)
gen byte hours50=(hourswm>40 & hourswm<=50)

gen byte weeks0=(weeksm1<=0)
gen byte weeks10=(weeksm1>0 & weeksm1<=10)
gen byte weeks20=(weeksm1>10 & weeksm1<=20)
gen byte weeks30=(weeksm1>20 & weeksm1<=30)
gen byte weeks40=(weeksm1>30 & weeksm1<=40)
gen byte weeks50=(weeksm1>40 & weeksm1<=50)*/

* QUANTILE REGRESSION ;
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.5) 
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.6)
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.7)
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.75)
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.8)
bsqreg tothrs more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, quant(.9)

**************************************************************************
*** Table 3 probit columna 2 en tabla 3 
***********************************************************************
* RECALCULATE THE MARGINAL EFFECTS FOR THE PROBITS OF HOURS DISTRIBUTION
 *(FOR TABLE III) SO IT'S ANALOGOUS TO WHAT WE'VE DONE FOR THE OTHERS;

*gen byte hours0 =(hourswm<=0)
gen byte hours10=(tothrs>0 & tothrs<=10)
gen byte hours20=(tothrs>10 & tothrs<=20)
gen byte hours30=(tothrs>20 & tothrs<=30)
gen byte hours40=(tothrs>30 & tothrs<=40)
gen byte hours50=(tothrs>40 & tothrs<=50)
 
probit hours10 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust 
mfx
probit hours20 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust 
mfx
probit hours30 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust 
mfx
probit hours40 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust 
mfx
probit hours50 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust 
mfx
*****************************************************************************
*** Oprobit col 3 en table 3 
*************************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Angrist_1998datafile\paper_Limited Dependent Variable"
use pums80m, clear 

gen byte hours0=(hourswm<=0)
gen byte hours10=(hourswm>0 & hourswm<=10)
gen byte hours20=(hourswm>10 & hourswm<=20)
gen byte hours30=(hourswm>20 & hourswm<=30)
gen byte hours40=(hourswm>30 & hourswm<=40)
gen byte hours50=(hourswm>40 & hourswm<=50)

gen hrscat = hourswm

/* ESTIMATE ORDERED PROBIT AND MARGINAL EFFECTS */

recode hrscat 0=1 1/10=2 11/20=3 21/30=4 31/40=5 41/max=6 

oprobit hrscat morekids agem1 agefstm boy1st boy2nd blackm hispm othracem

scalar define beta = _b[morekids]
scalar define stdev = _se[morekids]
di beta stdev

predict yhat, xb

gen me_y1 = -normalden(_b[/cut1]-yhat)*beta if morekids==1
gen se_1 = -normalden(_b[/cut1]-yhat)*stdev if morekids==1

gen me_y2 = (normalden(_b[/cut1]-yhat)-normalden(_b[/cut2]-yhat))*beta  if morekids==1
gen se_2 =  (normalden(_b[/cut1]-yhat)-normalden(_b[/cut2]-yhat))*stdev if morekids==1

gen me_y3 = (normalden(_b[/cut2]-yhat)-normalden(_b[/cut3]-yhat))*beta  if morekids==1
gen se_3 =  (normalden(_b[/cut2]-yhat)-normalden(_b[/cut3]-yhat))*stdev if morekids==1

gen me_y4 = (normalden(_b[/cut3]-yhat)-normalden(_b[/cut4]-yhat))*beta  if morekids==1
gen se_4 =  (normalden(_b[/cut3]-yhat)-normalden(_b[/cut4]-yhat))*stdev if morekids==1

gen me_y5 = (normalden(_b[/cut4]-yhat)-normalden(_b[/cut5]-yhat))*beta  if morekids==1
gen se_5 =  (normalden(_b[/cut4]-yhat)-normalden(_b[/cut5]-yhat))*stdev if morekids==1

gen me_y6 = normalden(_b[/cut5]-yhat)*beta if morekids==1
gen se_6 =  normalden(_b[/cut5]-yhat)*stdev if morekids==1

summ me_y1 me_y2 me_y3 me_y4 me_y5 me_y6 
summ se_1 se_2 se_3 se_4 se_5 se_6 

****************************************************************************
*** ols 2sls tabla 3 columnas 
********************************************************************

*gen byte hours0 =(hourswm<=0)
gen byte hours10=(tothrs>0 & tothrs<=10)
gen byte hours20=(tothrs>10 & tothrs<=20)
gen byte hours30=(tothrs>20 & tothrs<=30)
gen byte hours40=(tothrs>30 & tothrs<=40)
gen byte hours50=(tothrs>40 & tothrs<=50)

/*********************************/
/* HOURS DISTRIUBTION            */
/*********************************/
* OLS and RF ;
*regress hours0  agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours10  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust
regress hours20  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust
regress hours30  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust
regress hours40  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust
regress hours50  more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b, robust
* 2SLS USING MULTI2ND ;
regress hours10 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex  edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b), robust
regress hours20 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex  edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b), robust
regress hours30 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex  edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b), robust
regress hours40 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex  edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b), robust
regress hours50 more2sons edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b (samesex  edad edad_fb hijo1_sexa hijo2_sexa region pareja granfb etnia_b), robust







