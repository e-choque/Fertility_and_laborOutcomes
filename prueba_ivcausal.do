cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Angrist_1998datafile\paper_Limited Dependent Variable"
use pums80m, clear 
set dp comma 
*use abadie, clear 
/*===============================================================*/
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 
 * estimate e[z|x] ;
probit multi2nd  agem1 agefstm boy1st boy2nd blackm hispm othracem 
*probit workedm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem 
predict econdz 
gen m_econdz = 1 - econdz

* zhat = E(Z|X,D,Y) =  E(Z|X,D=1,Y)*D + E(Z|D=0,Y)*(1-D);
*probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm
*don't do this - morekids==0 predicts failure perfectly;
***** Y = employment ;
probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm if morekids==1
predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 
gen wstar_e=what
replace wstar_e=0 if what<=0
summ wstar_e
drop zhat what

******************************************************************************
*columna 1 tabla 2 y 4 tabla 3 
******************************************************************************
****** Y = hourswm;
probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hourswm if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h=what

replace wstar_h=0 if what<=0
summ wstar_h

 drop zhat what

***** Y = hours0 
gen hours0 = (hourswm<=0)

probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours0 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h0=what

replace wstar_h0=0 if what<=0

summ wstar_h

drop zhat what 

 ***** Y = hours1 
gen hours1 = (hourswm>0 & hourswm<=10)

probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours1 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h1=what

replace wstar_h1=0 if what<=0

summ wstar_h1

drop zhat what

  ***** Y = hours2 ;
gen hours2 = (hourswm>10 & hourswm<=20)

probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours2 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0
* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h2=what

replace wstar_h2=0 if what<=0

summ wstar_h2

drop zhat what

 ***** Y = hours3 ;
gen hours3 = (hourswm>20 & hourswm<=30)
probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours3 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h3=what

replace wstar_h3=0 if what<=0

summ wstar_h3

drop zhat what

 ***** Y = hours4 ;
gen hours4 = (hourswm>30 & hourswm<=40)

probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours4 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h4=what

replace wstar_h4=0 if what<=0

summ wstar_h4

drop zhat what

 ***** Y = hours5;
gen hours5 = (hourswm>40 & hourswm<=50)

probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids hours5 if morekids==1

predict zhat if morekids==1
replace zhat=0 if morekids==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-morekids))/econdz) - ((morekids*(1-zhat))/m_econdz) 

gen wstar_h5=what

replace wstar_h5=0 if what<=0

summ wstar_h5

drop zhat what

 *save abadie
******************************
use abadie, clear 

* RUN WEIGHTED REGRESSION, using aweights! tabla 2 col 1 y 3 ;
reg workedm  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_e] 
reg hourswm  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h] 

regress workedm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem (multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem)
regress hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem (multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem)
* WITHOUT COVARIATES;
reg workedm  morekids [aw=wstar_e] 
reg hourswm  morekids [aw=wstar_h] 

/*===============================================================*/
 * DISTRIBUTION OF HOURS ; 
* WEIGHTED REG W/ AWEIGHTS ; table 3 col 4
/*===============================================================*/
 
reg hours0  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h0] 
reg hours1  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h1] 
reg hours2  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h2] 
reg hours3  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h3] 
reg hours4  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h4] 
reg hours5  morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [aw=wstar_h5]


*****************************************************************************
*********** MODELO NO LINEAL tabla 2 (col 5) y 3 (col 5 o 6 ?)
**************************************************************************
use abadie, clear 
*probit workedm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem 
probit hours0 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem 

program drop nlphi
program define nlphi
    version 16.0
    if "`1'" == "?" {
                  global S_1 "B0 B1 B2 B3 B4 B5 B6 B7 B8" 
                  global B0=-.1
                  global B1=-.4
                  global B2=.1
                  global B3=-.1
                  global B4=0  
                  global B5=0
                  global B6=.5
                  global B7=-0
                  global B8=.1

                  global S_2 "NLLS, w/ Abadie weight"
                  global S_3 "$S_E_depv = normprob(B'X)"
                  exit
                  }  

                 replace `1' = normprob($B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm+$B7*hispm+$B8*othracem)   

               end

			   
*program drop nlphi 
*** para horas tabla 3 
program define nlphi
    version 16.0
    if "`1'" == "?" {
                  global S_1 "B0 B1 B2 B3 B4 B5 B6 B7 B8" 
                  global B0=.1
                  global B1=-.43
                  global B2=-.1
                  global B3=.1
                  global B4=0  
                  global B5=0
                  global B6=-.5
                  global B7=0
                  global B8=-.1
				  
                  global S_2 "NLLS, w/ Abadie weight"
                  global S_3 "$S_E_depv = normprob(B'X)"
                  exit
                  }  

                 replace `1' = normprob($B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm+$B7*hispm+$B8*othracem)   

               end

			   

/*===============================================================*/
* EMPLOYMENT ;
/*===============================================================*/

nl phi workedm [w=wstar_e] 

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm+$B7*hispm+$B8*othracem

gen phi = normalden(xb) if morekids==1
summ phi
scalar define scale = _result(3)
di scale
gen effect = $B1*scale
summ effect

drop xb phi effect 
scalar drop scale

**********************************************
*** probit table 3 col 5 o  6  ?
/*===============================================================*/
* DISTRIBUTION OF HOURS ;
* Estimate the nlls model - let H(.) = phi;
/*===============================================================*/

* HOURS0;
nl phi hours0 [w=wstar_h0], init(B0=.1,B1=.43,B2=-.1,B3=.1,B4=0,B5=0,B6=-.5,B7=0,B8=-.1)
gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm+$B7*hispm+$B8*othracem
gen phi = normalden(xb) if morekids==1
summ phi
scalar define scale = _result(3)
di scale
gen effect = $B1*scale
summ effect

drop xb phi effect 
scalar drop scale

* HOURS1;
 nl phi hours1 [w=wstar_h1], init(B0=-2.7,B1=0,B2=0,B3=0,B4=0,B5=0,B6=-.1,B7=-.16,B8=-.2);

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm
+$B7*hispm+$B8*othracem;

gen phi = normd(xb) if morekids==1;
summ phi;
scalar define scale = _result(3);
di scale;
gen effect = $B1*scale;
summ effect;

drop xb phi effect ;
scalar drop scale;

* HOURS2;
nl phi hours2 [w=wstar_h2], init(B0=-2,B1=-.1,B2=0,B3=0,B4=0,B5=0,B6=-.5,B7=-.3,B8=-.3);

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm
+$B7*hispm+$B8*othracem;

gen phi = normd(xb) if morekids==1;
summ phi;
scalar define scale = _result(3);
di scale;
gen effect = $B1*scale;
summ effect;

drop xb phi effect ;
scalar drop scale;

* HOURS3;
nl phi hours3 [w=wstar_h3], init(B0=-1.5,B1=-.2,B2=0,B3=0,B4=0,B5=0,B6=-.2,B7=-.2,B8=-.1);

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm
+$B7*hispm+$B8*othracem;

gen phi = normd(xb) if morekids==1;
summ phi;
scalar define scale = _result(3);
di scale;
gen effect = $B1*scale;
summ effect;

drop xb phi effect ;
scalar drop scale;

* HOURS4;
nl phi hours4 [w=wstar_h4], init(B0=-.1,B1=-.4,B2=0,B3=-.1,B4=0,B5=0,B6=.8,B7=.2,B8=.3);

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm
+$B7*hispm+$B8*othracem;

gen phi = normd(xb) if morekids==1;
summ phi;
scalar define scale = _result(3);
di scale;
gen effect = $B1*scale;
summ effect;

drop xb phi effect ;
scalar drop scale;

* HOURS5;
nl phi hours5 [w=wstar_h5], init(B0=-1.7,B1=-.15,B2=0,B3=0,B4=0,B5=0,B6=.1,B7=0,B8=.1);

gen xb = $B0+$B1*morekids+$B2*agem1+$B3*agefstm+$B4*boy1st+$B5*boy2nd+$B6*blackm
+$B7*hispm+$B8*othracem;

gen phi = normd(xb) if morekids==1;
summ phi;
scalar define scale = _result(3);
di scale;
gen effect = $B1*scale;
summ effect;

drop xb phi effect ;
scalar drop scale;

***************************************************************************
*** Causal QTE col 8 en tabla 3 
**********************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Angrist_1998datafile\paper_Limited Dependent Variable"
use abadie, clear 
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
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [pw=wstar_h], quant(.5)
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [pw=wstar_h], quant(.6)
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [w=wstar_h], quant(.7)
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [w=wstar_h], quant(.75)
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [w=wstar_h], quant(.8)
qreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem [w=wstar_h], quant(.9)

**********************************************************************
*** tabla 3 qreg col 7 en tabla 3 
*********************************************************************
use pums80m, clear 
/*=======================================*/
/* program: /data/melissas/ldvtbl2b.do   */
/* by: MS				 */
/* date: 9 July 1999			 */
/*					 */
/* purpose: quantile regression w/ bootstrapped s.e.'s  */
/*					 */
/* data in: pums80m                      */
/* data out: 				 */
/*=======================================*/

summ hourswm, detail
pctile quantval = hourswm, nquantiles(20) genp(quant)
list quantval quant in 1/20

gen byte hours0=(hourswm<=0)
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
gen byte weeks50=(weeksm1>40 & weeksm1<=50)

* QUANTILE REGRESSION ;
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.5)
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.6)
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.7)
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.75)
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.8)
bsqreg hourswm morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, quant(.9)

**************************************************************************
*** Table 3 probit columna 2 en tabla 3 
***********************************************************************
* RECALCULATE THE MARGINAL EFFECTS FOR THE PROBITS OF HOURS DISTRIBUTION
 *(FOR TABLE III) SO IT'S ANALOGOUS TO WHAT WE'VE DONE FOR THE OTHERS;

use pums80m, clear 

gen byte hours0 =(hourswm<=0)
gen byte hours10=(hourswm>0 & hourswm<=10)
gen byte hours20=(hourswm>10 & hourswm<=20)
gen byte hours30=(hourswm>20 & hourswm<=30)
gen byte hours40=(hourswm>30 & hourswm<=40)
gen byte hours50=(hourswm>40 & hourswm<=50)

probit hours0 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust 

gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm + _b[hispm]*hispm + _b[othracem]*othracem 
gen phi1 = normalden(xb) if morekids==1
gen effect1 = _b[morekids]*phi1
summ effect1

drop phi1 effect1 xb

*****
probit hours10 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust ;

gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm
	 + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm +
	 _b[hispm]*hispm + _b[othracem]*othracem ;
gen phi1 = normd(xb) if morekids==1;
gen effect1 = _b[morekids]*phi1;
summ effect1;

drop phi1 effect1 xb;

probit hours20 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust ;

gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm
	 + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm +
	 _b[hispm]*hispm + _b[othracem]*othracem ;
gen phi1 = normd(xb) if morekids==1;
gen effect1 = _b[morekids]*phi1;
summ effect1;

drop phi1 effect1 xb;

probit hours30 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust ;

gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm
	 + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm +
	 _b[hispm]*hispm + _b[othracem]*othracem ;
gen phi1 = normd(xb) if morekids==1;
gen effect1 = _b[morekids]*phi1;
summ effect1;

drop phi1 effect1 xb ;

probit hours40 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust ;

gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm
	 + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm +
	 _b[hispm]*hispm + _b[othracem]*othracem ;
gen phi1 = normd(xb) if morekids==1;
gen effect1 = _b[morekids]*phi1;
summ effect1;

drop phi1 effect1 xb;

probit hours50 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem, robust ;


gen xb = _b[_cons] + _b[morekids]*morekids + _b[agem1]*agem1 + _b[agefstm]*agefstm
	 + _b[boy1st]*boy1st + _b[boy2nd]*boy2nd +_b[blackm]*blackm +
	 _b[hispm]*hispm + _b[othracem]*othracem ;
gen phi1 = normd(xb) if morekids==1;
gen effect1 = _b[morekids]*phi1;
summ effect1;

drop phi1 effect1 xb;

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

use pums80m, clear 

gen byte hours0=(hourswm<=0)
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
gen byte weeks50=(weeksm1>40 & weeksm1<=50)

/*********************************/
/* HOURS DISTRIUBTION            */
/*********************************/
* OLS and RF ;
regress hours0  agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours10 agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours20 agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours30 agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours40 agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
regress hours50 agem1 agefstm boy1st boy2nd blackm hispm othracem morekids, robust
* 2SLS USING MULTI2ND ;
regress hours0 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem (multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust
regress hours10 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem
	(multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust;
regress hours20 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem
	(multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust;
regress hours30 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem
	(multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust;
regress hours40 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem
	(multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust;
regress hours50 morekids agem1 agefstm boy1st boy2nd blackm hispm othracem
	(multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem), robust;











