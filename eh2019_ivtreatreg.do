////Tesis de Grado por Edison Choque Sanchez Agosto de 2021 - UMSA 
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
keep if parentesco==1 | parentesco==2 
drop if sexo ==1
*drop if edad>=45
*drop if edad<=19

****************************************************************************
/// Analisis descriptivo 
tab job [w=factor]
tab sonsa [w=factor]

/// Analisis Econometrico 
*** binary treatmentmodel
regress sonsa aestudio [pw=factor] if area==1 

ivtreatreg sonsa edu_sup edad edad2 exp  urban region, hetero(edad edad2 urban) iv(firsthalf) model(modeltype) graphic
ivtreatreg sonsa edu_sup edad edad2 exp  urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls) graphic
ivtreatreg sonsa edu_9 edad edad2 region if area==1, hetero(edad edad2 ) iv(firsthalf) model(probit-2sls)  
ivtreatreg sonsa edu_9 edad edad2 region if area==1, hetero(edad edad2 ) iv(firsthalf) model(probit-ols)  

bootstrap atet=e(atet) atent=e(atent), rep(100): ivtreatreg sonsa edu_9 edad edad2 urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls)
bootstrap atet=e(atet) atent=e(atent), rep(100): ivtreatreg sonsa aestudio edad edad2 urban region, hetero(edad edad2 urban) iv(firsthalf) model(probit-2sls)
ereturn list
estimates store probit_2sls
estimates table  probit_2sls, keep(edu_9) star

etregress sonsa edad exp edad2 region etnia_b pareja if area==1 & edad >= 18, treat(edu_9 = firsthalf) vce(robust) /*wald test no*/

** oprobit 
oprobit kids lnylab aestudio edad edad2 region etnia_b pareja if area == 1  & edad >= 18
margins aestudio
marginsplot
test
predict kids_hat if e(sample), pr 
margins, dydx(*) predict(outcome(#2))







