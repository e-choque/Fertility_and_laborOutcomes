/// EH 2019
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2019_Persona.dta, clear 

*drop if area ==2

ren s02a_05 parentesco 
ren s02a_03 edad 
ren s02a_02 sexo 

* eliminar hogares que tienen hijos adoptados 
gen adopt = (parentesco==4) 
	egen adopta=total(adopt), by (folio)
	gen adoptb= (adopta >0)
	drop if adoptb==1
* Variable sons
	gen sons = (parentesco==3) 
	egen sonsa=total(sons), by (folio)
	label var sonsa "Número de hijos en los hogares"
	gen sonsb= (sonsa >0)
	label var sonsb "Presencia de hijos en el hogar" 
	label define sonsb  1 "Con hijos"  0 "Sin hijos" 
	   label values sonsb sonsb   

gen more2sons=(sonsa>2)
drop if sonsa<=1
*** samesex variable con restriciones de edadhijo_max y sons>=2 
keep if parentesco==3
egen edadhijo_max=max(edad), by (folio)
drop if edadhijo_max>18
*** Para hijo1 mayor 
sort folio
by folio: gen hijo1_sex=sexo if edad==edadhijo_max
egen hijo1_sexa=mean(hijo1_sex), by(folio)

keep if hijo1_sexa==1 | hijo1_sexa==2 

drop if edadhijo_max==edad 

egen edadhijo_maxb=max(edad), by (folio)
by folio: gen hijo2_sex=sexo if edad==edadhijo_maxb
egen hijo2_sexa=mean(hijo2_sex), by(folio)

keep if hijo2_sexa==1 | hijo2_sexa==2 

gen samesex=1 if hijo1_sexa==hijo2_sexa
replace samesex=0 if samesex==.

by folio: keep if _n==1 
save "samesex_ind_2019", replace 
*************************************************************************
*2
use EH2019_Persona, clear 
*drop if area ==2
ren s02a_05 parentesco 
ren s02a_03 edad 
ren s02a_02 sexo 

ren s02a_10 est_civil
ren s03a_04 etnia
ren s06g_48 mashrs_trabajo
ren s06g_47 desea_mashrs_sem_pasada
ren ocupado job 
ren s04b_13 hijos_vivos
ren s04b_12 hijosnac_vivos

/// Seleccion de la muestra 
** variable samesex
merge m:1 folio using "samesex_ind_2019", keepusing (more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex)
keep if _merge==3
drop _merge

label var  samesex "mismo sexo"
label define samesex 1 "dos primeros hijos del mismo sexo" 0 "dos primeros hijos no tienen mismo sexo"
label values samesex samesex

/// otros instrumentos 

gen twoboys=1 if hijo1_sexa==1 & hijo2_sexa==1
replace twoboys=0 if twoboys==.

gen twogirls=1 if hijo1_sexa==2 & hijo2_sexa==2
replace twogirls=0 if twogirls==.

gen pareja=(est_civil==2 | est_civil==3)

gen etnia_b=(etnia==1) 
label define etnia_b 1 "si pertenece" 0 "no pertenece"
label values etnia_b etnia_b

gen edu_level=0 if niv_ed_g  ==0
replace edu_level=1 if niv_ed_g ==1
replace edu_level=2 if niv_ed_g ==2
replace edu_level=3 if niv_ed_g ==3

egen hhsize = count(nro), by(folio)
gen region = (depto==2 | depto==3 | depto==7) if !missing(depto)

/// Seleccion de la muestra 
keep if parentesco==1 | parentesco==2 
drop if sexo ==1
drop if edad>=45
drop if edad<=19
save "abadie_bol_eh2019", replace 
******************************************************************************

gen lnylab=ln(ylab)
gen lnyhog= ln(yhog)
gen edad2=edad*edad
gen exp = edad - aestudio - 6
gen exp2=exp*exp

drop if p0==.

****************************************************************************
/// Analisis descriptivo 
tab job [w=factor]
tab sonsa [w=factor]

/// Analisis Econometrico

*** Modelo probit

probit more2sons samesex edad  etnia_b pareja, r 
probit more2sons twoboys twogirls edad  etnia_b pareja, r 

probit job edad exp exp2 etnia_b pareja more2sons, r
ivprobit job edad exp exp2 etnia_b pareja (more2sons=samesex)  
margins, dydx(*) predict(pr)

reg lnylab edad exp exp2 etnia_b pareja more2sons, r
ivregress 2sls lnylab (more2sons = samesex) edad exp exp2 etnia_b pareja, first
ivregress 2sls lnylab (more2sons = samesex twoboys twogirls) edad exp exp2 etnia_b pareja, first
estat firststage

reg tothrs edad exp exp2 etnia_b pareja more2sons, r
ivregress 2sls tothrs (more2sons = samesex) edad exp exp2 etnia_b pareja, first

reg lnyyhog edad exp exp2 etnia_b pareja more2sons, r
ivregress 2sls lnyyhog (more2sons = samesex) edad exp exp2 etnia_b pareja, first


*** Regresion cuantilica 
bsqreg tothrs morekids edad exp etnia_b pareja boy1st boy2nd,
bsqreg tothrs more2sons edad exp etnia_b pareja,
 quant(.9)

 *Simultaneous Maximum Likelihood Probit Models: All workers
 cmp setup
 
 cmp (p0 = more2sons# pareja sexo hhsize edu_level region) (more2sons = p0# pareja sexo hhsize  edu_level region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 20, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr qui  tech(dfp nr)
 
 cmp (p0 = more2sons# pareja sexo hhsize edu_level region) (more2sons = p0# pareja sexo hhsize  edu_level region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 20, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr nonrtolerance
 
 cmp (p0 = more1sons# pareja sexo hhsize edu_level region) (more1sons = p0# pareja sexo hhsize  edu_level region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 20, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr nonrtolerance
 
predict info_hat if e(sample), pr eq(#1)
predict hhpo_hat if e(sample), pr eq(#2)
 
 ** probit bivariado 
 
 probit p0 more2sons pareja sexo hhsize  edu_level region [pw=factor] if parentesco == 1 & area == 1  & edad >= 20  , r
 probit more2sons p0 pareja sexo hhsize  edu_level region [pw=factor] if parentesco == 1 & area == 1  & edad >= 20  , r
 
****************************************************************************
****************************************************************************
*3
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2019_Persona, clear 

ren s02a_05 parentesco 
ren s02a_03 edad 
ren s02a_02 sexo 

ren s02a_10 est_civil
ren s03a_04 etnia
ren s06g_48 mashrs_trabajo
ren s06g_47 desea_mashrs_sem_pasada
ren ocupado job 
ren s04b_13 hijos_vivos
ren s04b_12 hijosnac_vivos
ren s02a_04b mes_nac


 *eliminar hogares que tienen hijos adoptados 
gen adopt = (parentesco==4) 
	egen adopta=total(adopt), by (folio)
	gen adoptb= (adopta >0)
	drop if adoptb==1

* Variable sons
	gen sons = (parentesco==3) /*variable que tiene el valor de 1 para los miembros que sean menores o iguales a 13 años*/
	egen sonsa=total(sons), by (folio)
	label var sonsa "Número de hijos en los hogares"
	gen sonsb= (sonsa >0)
	label var sonsb "Presencia de hijos en el hogar" 
	label define sonsb  1 "Con hijos"  0 "Sin hijos" 
	   label values sonsb sonsb   

/// generacion de variables 
egen edadhijo_max=max(edad) if parentesco==3, by (folio)
egen edadhijo_maxa=mean(edadhijo_max), by (folio)


/// generacion de variables 
gen pareja=(est_civil==2 | est_civil==3)

gen etnia_b=(etnia==1) 
label define etnia_b 1 "si pertenece" 0 "no pertenece"
label values etnia_b etnia_b

gen edu_level=0 if niv_ed_g  ==0
replace edu_level=1 if niv_ed_g ==1
replace edu_level=2 if niv_ed_g ==2
replace edu_level=3 if niv_ed_g ==3

gen edu_sup=(edu_level==3)

gen edu_9=(aestudio>9)


egen hhsize = count(nro), by(folio)
gen region = (depto==2 | depto==3 | depto==7) if !missing(depto)
gen urban=(area==1)
gen firsthalf=(mes_nac<=6)

gen lnylab=ln(ylab)
gen lnyhog= ln(yhog)
gen edad2=edad*edad
gen exp = edad - aestudio - 6
gen exp2=exp*exp

gen kids=0 if sonsa==0
replace kids=1 if sonsa==1 
replace kids=2 if sonsa==2 
replace kids=3 if sonsa==3
replace kids=4 if sonsa>3

gen more2sons=(sonsa>2)

/// Seleccion de la muestra 
drop if edadhijo_maxa>18 & sonsb==1
 drop if p0==.
*keep if parentesco==1 | parentesco==2 
*drop if sexo ==1
*drop if edad>=45
*drop if edad<=19

****************************************************************************
/// Analisis descriptivo 
tab job [w=factor]
tab sonsa [w=factor]
 
 
** oprobit 
oprobit kids lnylab pareja sexo hhsize aestudio region if parentesco == 1 & area == 1  & edad >= 18
margins, dydx(*) predict(outcome(#2))

cmp setup
cmp (kids = lnylab pareja sexo hhsize aestudio region)  if parentesco == 1 & area == 1  & edad >= 18, indicators($cmp_oprobit) quietly 
margins, dydx(*) predict(pr outcome(#2))


*Simultaneous Maximum Likelihood Probit Models: All workers
cmp setup
cmp (p0 = sonsb# pareja sexo hhsize informalhhmembers remitt eduyears tenure firmsize ib3.ecsector ib4.occupation region) (hhpo = info# maritalstatus gender ethnic hhsize hhmemberswork informalhhmembers remitt eduyears tenure ib3.ecsector ib4.occupation region) [pw=factor] if hhhead == 1 & area == 1 & priv == 1 & employed == 1 & age >= 10, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr qui 
predict info_hat if e(sample), pr eq(#1)
predict hhpo_hat if e(sample), pr eq(#2)

biprobit (p0 = more2sons# pareja sexo hhsize aestudio region)(more2sons = p0# pareja sexo hhsize  aestudio region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 18

biprobit (p0 = more2sons pareja sexo hhsize aestudio region)(more2sons = p0 pareja sexo hhsize  aestudio region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 18 



cmp (p0 = sonsb# pareja sexo hhsize aestudio region) (sonsb = p0# pareja sexo hhsize  aestudio region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 18, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr qui tech(dfp nr) 
predict info_hat if e(sample), pr eq(#1)
predict hhpo_hat if e(sample), pr eq(#2)

cmp (p0 = sonsb# pareja sexo hhsize  edu_level region) (sonsb = p0# pareja sexo hhsize  edu_level region) [pw=factor] if parentesco == 1 & area == 1  & edad >= 20, ind($cmp_probit $cmp_probit) vce(cluster upm) nolr qui nonrtolerance
predict info_hat if e(sample), pr eq(#1)
predict hhpo_hat if e(sample), pr eq(#2)

*** binary treatmentmodel
regress sonsa aestudio [pw=facto]if area==1 
ivtreatreg sonsa edu_sup edad edad2 exp  urban region, hetero(edad edad2 urban) iv(firsthalf) model(modeltype) graphic
ivtreatreg sonsa edu_sup edad edad2 exp  urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls) graphic
ivtreatreg sonsa edu_9 edad edad2 region if area==1, hetero(edad edad2 ) iv(firsthalf) model(probit-2sls)  
ivtreatreg sonsa edu_9 edad edad2 region if area==1, hetero(edad edad2 ) iv(firsthalf) model(probit-ols)  
bootstrap atet=e(atet) atent=e(atent), rep(100): ivtreatreg sonsa edu_9 edad edad2 urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls)
bootstrap atet=e(atet) atent=e(atent), rep(100): ivtreatreg sonsa aestudio edad edad2 urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls)
ereturn list
estimates store probit_2sls
estimates table  probit_2sls, keep(edu_9) star

