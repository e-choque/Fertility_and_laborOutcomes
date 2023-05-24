*** Tesis de grado en Economia 
*** Por Edison Choque Sanchez - UMSA 
cd "J:\Mi unidad\Consultorias\Tesis_Edison\Stata"
use abadie_fembol_censo2001, clear 

set dp comma 
*ren bo2012a_resdept depto_reside
ren bo2001a_nonwork2 nowork2 
ren bo2001a_classwk est_empleo
*** generacion de variables 
gen edad_fb= edad - edadhijo_max
*gen urban=(depto_reside==2 | depto_reside==3 | depto_reside==7)
gen urban=(area==1)

drop if nowork2 ==3
drop if edad_fb<18

*recode edad 18/24=1 25/29=2 30/34=3 35/39=4 40/max=5, gen (edad2)
*recode edad_fb 18/25=1 26/30=2 31/35=3 36/max=4 , gen (edad_fb2)
*********************************************************************
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 * estimate e[z|x] ;
*probit samesex edad edad_fb hijo1_sexa urban etnia_b hijo2_sexa edu_level [pw=perwt], r 
probit samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b
predict econdz 
gen m_econdz = 1 - econdz

* zhat = E(Z|X,D,Y) =  E(Z|X,D=1,Y)*D + E(Z|D=0,Y)*(1-D);
*probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm;
*don't do this - morekids==0 predicts failure perfectly;
***** Y = employment ;
*probit samesex more2sons edad edad_fb hijo1_sexa eje etnia_b job  hijo2_sexa edu_level [pw=perwt] if more2sons==1, r
probit samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b more2sons job if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_e=what
replace wstar_e=0 if what<=0
summ wstar_e
drop zhat what 

************************************************************************
program drop nlphi
program define nlphi
 version 16.0
    if "`1'" == "?" {
                  global S_1 "B0 B1 B2 B3 B4 B5 B6 B7 B8" 
                  global B0=-0.84
                  global B1=-.24
                  global B2=.04
                  global B3=-.02
                  global B4=0
                  global B5=0
                  global B6=.15
				  global B7=.05
				  global B8=.05
                
                  global S_2 "NLLS, w/ Abadie weight"
                  global S_3 "$S_E_depv = normprob(B'X)"
                  exit
                  }  

                 replace `1' = normprob($B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*urban+$B7*granfb+$B8*etnia_b)   

               end

/*===============================================================*/
* EMPLOYMENT ;
/*===============================================================*/

nl phi job [w=wstar_e]

cap drop xb
gen xb = $B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*urban+$B7*granfb+$B8*etnia_b
cap drop phi
gen phi = normalden(xb) 
summ phi
scalar define scale = _result(3)
di scale
cap drop effect
gen effect = $B1*scale
summ effect  

drop xb phi effect 
scalar drop scale

***********************************************
*** Lineal model 2sls - abadie - probit 
reg job more2sons edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b [aw=wstar_e] 
outreg2 using regression_result, replace excel dec(2)
estat endogenous 

*regress job more2sons edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b (samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b)

probit job more2sons edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b, r 
probit more2sons samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b, r 
margins, dydx(*)
outreg2 using regression_result, replace excel dec(2)

ivregress gmm job edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b (more2sons = samesex), vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b (more2sons = samesex) if area==2, vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa urban granfb etnia_b (more2sons = twoboys twogirls), vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa granfb etnia_b (more2sons = twoboys twogirls)  if area==2 , vce(boot, reps(500))

ivregress gmm job edad edad_fb hijo1_sexa urban granfb etnia_b (more2sons = samesex), vce(robust)
ivregress gmm job edad edad_fb hijo1_sexa granfb etnia_b (more2sons = samesex) if area==2, vce(robust)

weakivtest 
*estat firststage
estat endegenous
estat overid 

*gmm (eq1: job - {xb: edad edad_fb urban granfb etnia_b more2sons}), instruments(eq1: edad edad_fb urban granfb etnia_b twoboys twogirls) /*el signo de more2sons es positivo*/

*ivregress 2sls job edad edad_fb hijo1_sexa urban granfb etnia_b (more2sons = twoboys twogirls)  if area==1 , vce(boot, reps(100)) sargan not found




**********************************************************************
***********************************************************************
*** MODELO PARA HIJOS MENORES A 12 AÑOS BOLIVIA 2012 CENSO - Urbana
*********************************************************************
use abadie_fembol_censo2001, clear 

set dp comma 
*ren bo2012a_resdept depto_reside
ren bo2001a_nonwork2 nowork2 
ren bo2001a_classwk est_empleo
*** generacion de variables 
gen edad_fb= edad - edadhijo_max
*gen urban=(depto_reside==2 | depto_reside==3 | depto_reside==7)
gen urban=(area==1)
drop if nowork2 ==3
drop if edad_fb<18

keep if edadhijo_max<=12

keep if urban==1
*********************************************************************
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 * estimate e[z|x] ;
*probit samesex edad edad_fb hijo1_sexa urban etnia_b hijo2_sexa edu_level [pw=perwt], r 
probit samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b
predict econdz 
gen m_econdz = 1 - econdz

* zhat = E(Z|X,D,Y) =  E(Z|X,D=1,Y)*D + E(Z|D=0,Y)*(1-D);
*probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm;
*don't do this - morekids==0 predicts failure perfectly;
***** Y = employment ;
*probit samesex more2sons edad edad_fb hijo1_sexa eje etnia_b job  hijo2_sexa edu_level [pw=perwt] if more2sons==1, r
probit samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b more2sons job if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_e=what
replace wstar_e=0 if what<=0
summ wstar_e
drop zhat what 

************************************************************************
program drop nlphi
program define nlphi
 version 16.0
    if "`1'" == "?" {
                  global S_1 "B0 B1 B2 B3 B4 B5 B6 B7" 
                  global B0=-0.90
                  global B1=-.28
                  global B2=.05
                  global B3=-.03
                  global B4=0
                  global B5=0
                  global B6=.07
				  global B7=0
				  
                
                  global S_2 "NLLS, w/ Abadie weight"
                  global S_3 "$S_E_depv = normprob(B'X)"
                  exit
                  }  

                 replace `1' = normprob($B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*granfb+$B7*etnia_b)   

               end

/*===============================================================*/
* EMPLOYMENT ;
/*===============================================================*/

nl phi job [w=wstar_e]

cap drop xb
gen xb = $B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*granfb+$B7*etnia_b
cap drop phi
gen phi = normalden(xb) 
summ phi
scalar define scale = _result(3)
di scale
cap drop effect
gen effect = $B1*scale
summ effect  

drop xb phi effect 
scalar drop scale

***********************************************
*** Lineal model 2sls - abadie - probit 
reg job more2sons edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b [aw=wstar_e] 
outreg2 using regression_result, replace excel dec(2)
estat endogenous 

probit job more2sons edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b, r 
probit more2sons samesex edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b, r 
margins, dydx(*)
outreg2 using regression_result, replace excel dec(2)

ivregress gmm job edad edad_fb hijo1_sexa granfb etnia_b (more2sons = samesex), vce(robust)
weakivtest 

ivregress gmm job edad edad_fb hijo1_sexa granfb etnia_b (more2sons = twoboys twogirls), vce(boot, reps(500))
estat firststage
estat endegenous
estat overid 



