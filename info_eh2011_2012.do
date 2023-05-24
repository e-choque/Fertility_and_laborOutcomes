/// Tesis de grado por Edison Choque Sanchez - Bolivia 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos" 
use abadie_eh2011_2012.dta, clear

// Setting the complex survey desing
*svyset upm [pw=factor], strata(estrato)

/*For categorical and dummy variables
svy: tab firmsize if info==1 , ci  // Informal
svy: tab firmsize if info==0 , ci  // Formal
svy: tab firmsize if hhpo==1 , ci  // Poor
svy: tab firmsize if hhpo==0 , ci  // Non-Poor
*/
set dp comma 
*** generacion de variables 
gen edad_fb= edad - edadhijo_max
gen eje_region=(depto==2 | depto==3 | depto==7)

*drop if pei ==1
drop if edad_fb<18

keep if urban==1 
*recode edad 18/24=1 25/29=2 30/34=3 35/39=4 40/max=5, gen (edad2)
*recode edad_fb 18/25=1 26/30=2 31/35=3 36/max=4 , gen (edad_fb2)
*********************************************************************
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 * estimate e[z|x] ;
*probit samesex edad edad_fb hijo1_sexa eje_region etnia_b hijo2_sexa edu_level [pw=perwt], r 
probit samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b
predict econdz 
gen m_econdz = 1 - econdz

* zhat = E(Z|X,D,Y) =  E(Z|X,D=1,Y)*D + E(Z|D=0,Y)*(1-D);
*probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm;
*don't do this - morekids==0 predicts failure perfectly;
***** Y = employment ;
*probit samesex more2sons edad edad_fb hijo1_sexa eje etnia_b job  hijo2_sexa edu_level [pw=perwt] if more2sons==1, r
probit samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b more2sons info if more2sons==1
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
                  global B0=1
                  global B1=.23
                  global B2=0
                  global B3=-.03
                  global B4=0
                  global B5=0
                  global B6=.12
				  global B7=0
				  global B8=.37
                
                  global S_2 "NLLS, w/ Abadie weight"
                  global S_3 "$S_E_depv = normprob(B'X)"
                  exit
                  }  

                 replace `1' = normprob($B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*eje_region+$B7*granfb+$B8*etnia_b)   

               end

/*===============================================================*/
* EMPLOYMENT ;
/*===============================================================*/

nl phi info [w=wstar_e]

cap drop xb
gen xb = $B0+$B1*more2sons+$B2*edad+$B3*edad_fb+$B4*hijo1_sexa+$B5*hijo2_sexa+$B6*eje_region+$B7*granfb+$B8*etnia_b
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

*** Lineal model 2sls - abadie - probit 
reg info more2sons edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b [aw=wstar_e] 
estat endogenous 

regress job  more2sons edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b (samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b)

probit info more2sons edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b, r 
probit more2sons samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b, r 
margins, dydx(*)
outreg2 using regression_result, replace excel dec(2)