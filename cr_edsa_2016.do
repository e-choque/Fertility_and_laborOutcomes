/// EDSA 2016 mujeres antecedentes 

cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EDSA16_mujer_historia_nacimientos, clear 
*drop if area==2
// export variables to data hogar 
*ponderadorm multiplea edad_hijo live_athome hijo_vivo 

ren ms02_0217 multiple 
ren ms02_0215 nro2
ren ms02_0221 edad_hijo
ren ms02_0222 live_athome
ren ms02_0220 hijo_vivo

*egen multiplea=total(multiple==2), by (id)

// seleccionando la muestra 
gen multi=(multiple==2)
egen multia=total(multi==1), by (id)
gen multib=(multia>0) 
drop if multib==1 
drop if edad_hijo>18 
drop if live_athome==2
drop if hijo_vivo==2
egen nro_hijo=total(multiple), by (id)
/// generacion de variables 
egen edadhijo_max=max(edad_hijo), by (id)
drop if nro >2
sort id
by id: keep if _n==1

save "mujer_historia_export", replace 

/* analizando twinsmethod 
egen nro_max=max(nro2), by (id)

br id nro nro2 multiple  multiplea  edad_hijo live_athome hijo_vivo
br id multiple  multiplea  edad_hijo live_athome hijo_vivo nro_max if nro_max==2 & multiplea ==2 /* la muestra para multiples es menor a 30 */
br id multiple  multiplea  edad_hijo live_athome hijo_vivo nro_max if nro_max==3 & multiplea ==2 /* la muestra para multiples es menor a 30 */
 */

/// Nota: la muestra es muy pequeña para el twins method 
/// intentaremos probar con infertility method following to Aguero 

*********************************************************************************
use EDSA16_mujer_calendario, clear 


*********************************************************************************
use EDSA16_hogar, clear  

*drop if area == 2  

ren hs03_0004 parentesco 
ren hs03_0003_1 edad 
ren hs03_0005 sexo 
/// generacion de variables 
**Variable sons
	gen sons = (parentesco==3 | parentesco==4) /*variable que tiene el valor de 1 para los miembros que sean menores o iguales a 13 años*/
	egen sonsa=total(sons), by (id)
	label var sonsa "Número de hijos en los hogares"
	gen sonsb= (sonsa >0)
	label var sonsb "Presencia de hijos en el hogar" 
	label define sonsb  1 "Con hijos"  0 "Sin hijos" 
	   label values sonsb sonsb 

sort id
by id: keep if _n==1 	   

save "hogar_export", replace
**********************************
use EDSA16_mujeres_antecedentes, clear 
*drop if area==2
drop if nro>2
sort id
by id: keep if _n==1 

save "mujer_antecedente_master", replace 
*****************************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use mujer_antecedente_master, clear 

merge 1:1 id using "hogar_export", keepusing(sexo sonsa sonsb)
keep if _merge==3
drop _merge

merge 1:1 id using "mujer_historia_export", keepusing(edadhijo_max nro_hijo live_athome hijo_vivo)
*keep if _merge==3
*drop _merge

ren ms06_0610_1 sex_first /*insrumento*/ 
ren ms07_0702_1  infertility2 /*insrumento*/
ren ms07_0707_1e infertility1 /*insrumento*/
ren ms07_0714_1_1 sons_past 
ren ms07_0715 sexo_sons /* preferencias del sexo de los hijos en el pasado */ 
ren ms03_0309 hijos_antes_metodos /*cuando hijos tenia cuando aplico algun metodo para no quedar embarazada*/ 
ren ms03_0312 pareja_metodo /*su pareja hace algo para postergar un embarazo*/
ren ms03_0313a esterelizacion_f /*metodo a: esterelizacion femenina ver mas metodos*/ 
ren ms03_0318_2 año_esterelizacion 
ren ms03_0319_2 año_usocontinuo_metodoi
ren ms06_0601 casada_union

ren ms02_0241 aborto_espontaneo /*para ver 0 vs 1 hijo*/ 
ren ms02_0253 aborto_espontaneo_antes2011 
ren ms06_0610 have_sex 
ren ms01_0110 edad_mujer 

ren ms08_0805 ocupado_esposo 
ren ms08_0810cod ocu_principal_mujer  
ren ms08_0809 estudiante_ama 
ren ms08_0808 ocupado_mujer
ren ms07_0703_1  tiempo_otrohijo 


recode edad_mujer 18/25=1 26/30=2 31/35=3 36/40=4 41/44=5, gen(edad_mujer2)
gen ocu_mujer=(ocupado_mujer==1)
gen infertil1=(infertility1==1)
gen infertil2=(infertility2==3)
gen infertil=(infertil1==1 | infertil2==1)
gen married_union=(casada_union==1 | casada_union==2)
gen edu_pareja=0
replace edu_pareja=1 if ms08_0804_1==4
replace edu_pareja=2 if ms08_0804_1==5
replace edu_pareja=3 if ms08_0804_1==6 | ms08_0804_1==7 | ms08_0804_1==8 | ms08_0804_1==9 | ms08_0804_1==10
recode sonsa 0/0=0 1/1=1 2/2=2 3/3=3 4/4=4 5/max=5, gen (sons_a)
/// sample selection 
gen sons_b=(nro_hijo ==. & sonsa>0)
drop if sons_b==1 
gen sons_c=0 if nro_hijo==.
replace sons_c=1 if nro_hijo!=.
drop if estudiante_ama==1
drop if have_sex==0 
keep if edad_mujer>=18 & edad_mujer<=44

save "edsa_2016_abadie", replace 
/// ////////////////descrition of the data ///////////////////////
svyset upm [pweight=ponderadorm], strata(estrato)
table infertil edad_mujer2 [aw=ponderadorm], c(mean nro_hijo)
egen mean_hijo=mean(sonsa), by (edad_mujer2)
twoway (line mean_hijo edad_mujer2, sort), by(infertil)
twoway (connected mean_hijo edad_mujer2)
*** formas de exportar tablas de excel a stata o word 
asdoc 
tab2xl 
putexcel 
xtable foreign, cont(mean mpg sd mpg) row
estout 

tabout occupation collgrad using resultados.xls, cells (col)
tabout occupation race using resultados.xls, cells (col) append
tabout occupation using resultados.xls, cells (col) replace
*** para exportar estimaciones 
ssc install estout

quietly regress wage grade, beta
eststo model1
quietly regress  wage grade ttl_exp
eststo model2

esttab using prueba.doc, beta not label nonumber append
esttab using prueba.xls, beta not label nonumber replace

ssc install outreg2
help outreg2

outreg2 [model1 model2] using prueba2.doc, replace see beta

quietly logit  collgrad i.race if race!=3
eststo model3

outreg2 [model1 model2 model3] using prueba3.doc, replace see
 
/// ////////////////// estimation /////////////////////////////////////////
set dp comma 
set dp period 
*** modelo OLS 
** ols model 1 
probit ocu_mujer nro_hijo i.edad_mujer2 if sons_c==1 [pweight = ponderadorm], vce(robust)
probit ocu_mujer nro_hijo i.edad_mujer2 if sons_c==1 & edadhijo_max<=10 [pweight = ponderadorm], vce(robust)
probit ocu_mujer sons_c i.edad_mujer2 [pweight = ponderadorm], vce(robust)
margins, dydx(*)

eststo mfx1
esttab using prueba.xls, beta not label nonumber replace
outreg2 [mfx1] using prueba2.xls, replace see
outreg2 using regression_result, replace excel dec(2)
eststo: mfx 
estout
esttab 
esttab, r2 ar2 p 
esttab using name.doc, r2 ar2 p 
** ols model 2 
probit ocu_mujer nro_hijo i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja if sons_c==1 [pweight = ponderadorm], vce(robust)
probit ocu_mujer nro_hijo i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja if sons_c==1 & edadhijo_max<=10  [pweight = ponderadorm], vce(robust)
probit ocu_mujer sons_c i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja [pweight = ponderadorm], vce(robust)
margins, dydx(*)


*** iv model 1 
ivprobit ocu_mujer i.edad_mujer2  (sonsa=infertil) , first twostep
ivprobit ocu_mujer i.edad_mujer2   (nro_hijo=infertil) if sons_c==1 , first twostep
ivprobit ocu_mujer i.edad_mujer2   (sonsa=infertil) if sons_c==1, twostep
ivprobit ocu_mujer i.edad_mujer2   (sonsa=infertil) if sons_c==1 & edadhijo_max<=10 , first twostep
ivprobit ocu_mujer i.edad_mujer2  (sons_c=infertil)  , first twostep

ivprobit ocu_mujer i.edad_mujer2   (nro_hijo=infertil) if sons_c==1 [pweight = ponderadorm]
ivprobit ocu_mujer i.edad_mujer2  (nro_hijo=infertil) if sons_c==1 & edadhijo_max<=10 [pweight = ponderadorm]
ivprobit ocu_mujer i.edad_mujer2  (sons_c=infertil)  [pweight = ponderadorm]
margins, dydx(*) predict(pr)
outreg2 using regression_result, append excel dec(2)
*** iv model 2 
ivprobit ocu_mujer i.edad_mujer2  i.nivedum sex_first married_union i.edu_pareja (nro_hijo=infertil) if sons_c==1, first twostep 
ivprobit ocu_mujer i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja (sonsa=infertil) if sons_c==1 & edadhijo_max<=10, first twostep
ivprobit ocu_mujer i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja (sons_c=infertil) , first twostep

ivprobit ocu_mujer i.edad_mujer2  i.nivedum sex_first married_union i.edu_pareja (nro_hijo=infertil) [pweight = ponderadorm] if sons_c==1 
ivprobit ocu_mujer i.edad_mujer2  i.nivedum sex_first married_union i.edu_pareja (nro_hijo=infertil) [pweight = ponderadorm] if sons_c==1  & edadhijo_max<=10
ivprobit ocu_mujer i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja (sons_c=infertil)  [pweight = ponderadorm]
margins, dydx(*) predict(pr)


** postestimacion comands 
estat correlation
estat covariance
lroc
lsens
estat ic
estat sum
estat vce
estimates
margins
marginsplot
margins, dydx(*)


