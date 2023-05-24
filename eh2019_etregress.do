////Tesis de Grado por Edison Choque Sanchez Agosto de 2021 - UMSA 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
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

gen more2sons=(sonsa>2)
gen more1sons=(sonsa>1)

/// Seleccion de la muestra 
drop if sonsa<=1
*drop if sonsa<1
drop if edadhijo_maxa>18 

* variable samesex
merge m:1 folio using "samesex_ind_2019", keepusing (hijo1_sexa hijo2_sexa samesex)
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

************************
/// Seleccion de la muestra 
keep if parentesco==1 | parentesco==2 
drop if sexo ==1
drop if edad>=45
drop if edad<=19

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
etregress lnylab edad exp exp2 aestudio etnia_b pareja if area==1, treat(more2sons = samesex twoboys twogirls) vce(robust)
*** el efecto de more2sons es positivo y sig pero los instrimentos no son significativos 
 
 