
**************************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2011_persona.dta, clear 

ren s1_08 parentesco 
ren s1_04 edad 
ren s1_03 sexo 
ren ecivil est_civil
ren s5_54 disponible_mashrs_trabajo
ren s5_01 job 
ren s3_16 hijos_vivos_hoy

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
save "samesex_ind_2011", replace 
*************************************************************************
*2
use EH2011_persona, clear 

ren s1_08 parentesco 
ren s1_04 edad 
ren s1_03 sexo 
ren s1_13 est_civil
ren s5_54 disponible_mashrs_trabajo
ren s5_01 job 
ren s3_15 hijosnac_vivos
ren s3_16 hijos_vivos_hoy
ren ident  etnia 

	gen granf = (parentesco==6 | parentesco==7) 
	egen granfa=total(granf), by (folio)
	gen granfb= (granfa >0)
/// Seleccion de la muestra 
** variable samesex
merge m:1 folio using "samesex_ind_2011", keepusing (more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex)
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

gen etnia_b=(etnia==1 | etnia==2 | etnia==3) 
label define etnia_b 1 "si pertenece" 0 "no pertenece"
label values etnia_b etnia_b

gen edu_level=0 if niv_ed_g  ==0
replace edu_level=1 if niv_ed_g ==1
replace edu_level=2 if niv_ed_g ==2
replace edu_level=3 if niv_ed_g ==3

egen hhsize = count(nro), by(folio)
gen region = (depto==2 | depto==3 | depto==7) if !missing(depto)

gen ylabr=ylab/0.766400922268947 /*ipc 2016*/

/// Seleccion de la muestra 
keep if parentesco==1 | parentesco==2 
drop if sexo ==1
drop if edad>=45
drop if edad<=18
gen year=2011
save "abadie_bol_eh2011", replace 
*********************************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2012_persona.dta, clear 

ren s1_08 parentesco 
ren s1_04 edad 
ren s1_03 sexo 
ren s1_13 est_civil
ren s5_54 disponible_mashrs_trabajo
ren s5_01 job 
ren s3_14 hijosnac_vivos

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
save "samesex_ind_2012", replace 
*************************************************************************
*2
use EH2012_persona, clear 

ren s1_08 parentesco 
ren s1_04 edad 
ren s1_03 sexo 
ren s1_13 est_civil
ren s5_54 disponible_mashrs_trabajo
ren s5_01 job 
ren s3_14 hijosnac_vivos
ren s2_05a etnia 

	gen granf = (parentesco==6 | parentesco==7) 
	egen granfa=total(granf), by (folio)
	gen granfb= (granfa >0)
/// Seleccion de la muestra 
** variable samesex
merge m:1 folio using "samesex_ind_2012", keepusing (more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex)
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

gen  ylabr=ylab/0.801021859287506 /*ipc 2016*/
/// Seleccion de la muestra 
keep if parentesco==1 | parentesco==2 
drop if sexo ==1
drop if edad>=45
drop if edad<=18
gen year=2012
save "abadie_bol_eh2012", replace 
*********************************************************************************
/*use EH2013_persona, clear 

ren s2a_05 parentesco 
ren s2a_03 edad 
ren s2a_02 sexo 
ren s2a_10 est_civil
ren s6g_47 disponible_mashrs_trabajo
ren s6a_01 job 
ren s4c_12 hijos_vivos_hoy

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
save "samesex_ind_2013", replace 
*************************************************************************
*2
/*use EH2013_persona, clear 

ren s2a_05 parentesco 
ren s2a_03 edad 
ren s2a_02 sexo 
ren s2a_10 est_civil
ren s6g_47 disponible_mashrs_trabajo
ren s6a_01 job 
ren s4c_12 hijos_vivos_hoy
ren s3a_02a  etnia 

	gen granf = (parentesco==6 | parentesco==7) 
	egen granfa=total(granf), by (folio)
	gen granfb= (granfa >0)
/// Seleccion de la muestra 
** variable samesex
merge m:1 folio using "samesex_ind_2013", keepusing (more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex)
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
gen year=2013
save "abadie_bol_eh2013", replace */
*/
*********************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"

*use abadie_bol_eh2012, clear 
use abadie_bol_eh2011, clear 

keep more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex  parentesco edad sexo est_civil disponible_mashrs_trabajo job hijos_vivos_hoy hijosnac_vivos year  hijo1_sexa hijo2_sexa region pareja granfb etnia_b  tothrs twoboys twogirls area edad edadhijo_max pei pet ocupado condact mt phrs shrs  pext0 p0  edu_level upm pea factor ylabr  s5_21  s5_27 s5_22 s5_25 estrato s5_59b depto  
 
 
append using abadie_bol_eh2012, keep(more2sons sonsa edadhijo_max sonsb hijo1_sexa hijo2_sexa samesex  parentesco edad sexo est_civil disponible_mashrs_trabajo hijosnac_vivos job  year  hijo1_sexa hijo2_sexa region pareja granfb etnia_b  tothrs twoboys twogirls area edad edadhijo_max pei pet ocupado condact mt phrs shrs  pext0 p0  edu_level upm factor_2014 pea ylabr s5_21  s5_27 s5_22 s5_25 estrato s5_59b depto)

ren s5_21  categor 
ren s5_27  nro_trab 
ren s5_22  priv_a 
ren s5_25 rec_a

*keep if area==1
*save "abadie_eh2011_2012", replace 
*****************************************************************************
*use abadie_eh2011_2012, clear 
// creando la variable informal 
gen employed = (condact == 1)
*gen employed = 1 if condact == 2 ojo no es 2  
*gen employed = (condact==2) if !missing(condact)
*replace employed = . if (condact==5 | condact==6)
*gen unemployed = (condact==3 | condact ==4) if !missing(condact)
*replace unemployed = . if (condact==5 | condact==6)

gen urban = (area==1) if !missing(area)
*ren s06b_16 categor
ren nro_trab s06b_21
replace s06b_21=. if s06b_21>80
gen firmsize = 0 if s06b_21 < 6
replace firmsize =1 if s06b_21 >= 6 & s06b_21 <10
replace firmsize =2 if s06b_21 >= 10 & s06b_21 <50
replace firmsize =3 if s06b_21 >= 50 & s06b_21 <.
label define firmsize 0 "1 – 5 workers - small" 1 "6 – 9 workers- small medium" 2 "10 – 49 workers - medium" 3 "50 - workers - large" 
label values firmsize firmsize
label variable firmsize "Firm Size"

/*ren priv_a s06b_18 
gen priv = (s06b_18 == 3 | s06b_18 == 4) if !missing(s06b_18)   /*dummy sector privado*/
replace priv = 1 if s06b_18 >= . & categor != . */

gen priv =(priv_a == 2) if !missing(priv_a)

ren rec_a s06b_19
gen rec = (s06b_19 == 3 | s06b_19==4 | s06b_19==5) if !missing(s06b_19)  /* dummy si la empresa no tiene registro*/

/*La tasa de empleo informal se define según los siguientes criterios representados en las variables info"j" y los criterios se unen en la variable info:*/
gen info1=0 if (categor == 7)
replace info1 = 1 if (categor == 7 & employed == 1)  

gen info2=0 if categor == 8
replace info2 = 1 if (categor == 8 & employed == 1)  

gen info3=0 if categor == 6
replace info3 = 1 if (categor == 6 & employed == 1) 
 
gen info4=0 if categor == 3
replace  info4 = 1 if (categor == 3 & employed == 1 & (rec == 1 | s06b_19 == 6))

gen info5=0 if categor <= 2
replace info5=1 if (categor <= 2 & employed == 1 & priv == 1 & firmsize == 0  & (rec == 1 | (s06b_19 == 6 & s5_59b == 2)))

gen info6=0 if categor == 4 | categor == 5
replace info6= 1 if ((categor == 4 | categor == 5) & employed == 1  & firmsize == 0 & (rec == 1 | s06b_19 == 6))

gen info = 0
replace info = 1 if (info1 == 1 | info2 == 1 | info3 == 1 | info4 == 1 | info5 == 1 | info6 == 1)
replace info = . if (info1 >= . & info2 >= . & info3 >= . & info4 >= . & info5 >= . & info6 >= .)
gen info_self = (info4==1) if !missing(info4)   // Self-employed informals

****************************
/*
gen info1 = 1 if (categor == 7 & employed == 1)  //*trabajador familiar o aprendiz*//
gen  info2 = (categor == 8 & employed == 1)  //*Empleada del hogar*/
gen  info3 = (categor == 6 & employed == 1 )  //*Cooperativista*/
gen  info4 = (categor == 3 & employed == 1 & (rec == 1 | s06b_19 == 6))   //*Trabajador por cuenta propia sin registro o que no sabe si tiene registro*/
gen info5 = (categor <= 2 & employed == 1 & firmsize == 0  & (rec == 1 | (s06b_19 == 6 & s5_59b == 2))) //*Obrero o empleado en empresa con 5 o menos trabjadores y que no tiene registro o que si no sabe que tiene registro no aporta a las AFP*/
gen info6 = ((categor == 4 | categor == 5) & employed == 1  & firmsize == 0 & (rec == 1 | s06b_19 == 6))  //*Empleador con 5 o menos trabajadores, sin registro o no sabe si tiene registro*/
gen info = 0
replace info = 1 if (info1 == 1 | info2 == 1 | info3 == 1 | info4 == 1 | info5 == 1 | info6 == 1)
replace info = . if (info1 >= . & info2 >= . & info3 >= . & info4 >= . & info5 >= . & info6 >= .)
gen info_self = (info4==1) if !missing(info4)   // Self-employed informals*/
*******************************

save "abadie_eh2011_2012", replace 