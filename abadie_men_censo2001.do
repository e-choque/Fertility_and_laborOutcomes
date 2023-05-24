/// Tesis de grado Edison Choque Sanchez - Economia UMSA 
*** Tesis de grado en Economia 
*** Por Edison Choque Sanchez - UMSA 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use abadie_menbol_censo2001, clear 

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

********************************
*keep if pareja==1 
*recode edad 18/24=1 25/29=2 30/34=3 35/39=4 40/max=5, gen (edad2)
*recode edad_fb 18/25=1 26/30=2 31/35=3 36/max=4 , gen (edad_fb2)
*********************************************************************
 /* ABADIE'S WEIGHT: 1 - (zhat(1-D)/E(z|X,Y)) - (D(1-zhat)/1-E(Z|X,Y) */
 /*===============================================================*/
 * estimate e[z|x] ;
*probit samesex edad edad_fb hijo1_sexa eje_region etnia_b hijo2_sexa edu_level [pw=perwt], r 
probit samesex edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b
predict econdz 
gen m_econdz = 1 - econdz

* zhat = E(Z|X,D,Y) =  E(Z|X,D=1,Y)*D + E(Z|D=0,Y)*(1-D);
*probit multi2nd agem1 agefstm boy1st boy2nd blackm hispm othracem morekids workedm;
*don't do this - morekids==0 predicts failure perfectly;
***** Y = employment ;
*probit samesex more2sons edad edad_fb hijo1_sexa eje etnia_b job  hijo2_sexa edu_level [pw=perwt] if more2sons==1, r
probit samesex edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b more2sons job if more2sons==1
predict zhat if more2sons==1
replace zhat=0 if more2sons==0

* if D=0, Z=0, so only 1st component of zhat matters;
gen what = 1 - ((zhat*(1-more2sons))/econdz) - ((more2sons*(1-zhat))/m_econdz) 
gen wstar_e=what
replace wstar_e=0 if what<=0
summ wstar_e
drop zhat what 

*** Lineal model 2sls - abadie - probit 
reg job more2sons edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b [aw=wstar_e] 
reg job more2sons edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b [aw=wstar_e] if area==2
* estat endogenous 

*regress job  more2sons edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b (samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b)

ivregress gmm job edad edad_fb hijo1_sexa  urban granfb etnia_b (more2sons = samesex), vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa  granfb etnia_b (more2sons = samesex) if area==2, vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa urban granfb etnia_b (more2sons = twoboys twogirls), vce(boot, reps(500))
ivregress gmm job edad edad_fb hijo1_sexa  granfb etnia_b (more2sons = twoboys twogirls) if area==2, vce(boot, reps(500))

probit job more2sons edad edad_fb hijo1_sexa hijo2_sexa urban granfb etnia_b, r 
probit job more2sons edad edad_fb hijo1_sexa hijo2_sexa granfb etnia_b if area==2, r 

probit more2sons samesex edad edad_fb hijo1_sexa hijo2_sexa eje_region granfb etnia_b, r 
margins, dydx(*)
mfx
outreg2 using regression_result, replace mfx excel dec(2)
outreg2 using regression_result, replace  excel dec(2)
*******************
ivregress gmm job edad edad_fb hijo1_sexa  urban granfb etnia_b (more2sons = samesex), vce(robust)
ivregress gmm job edad edad_fb hijo1_sexa  granfb etnia_b (more2sons = samesex) if area==2, vce(robust)
weakivtest, level (0.1)
*******************
estat firststage
estat endegenous
estat overid 
*