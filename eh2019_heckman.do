///Tesis de Grado por Edison Choque Sanchez Agosto de 2021 - UMSA 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2019_Persona, clear 

ren s02a_05 parentesco 
ren s02a_03 edad 
ren s02a_02 sexo 
ren s02a_10 est_civil
ren s03a_04 etnia
ren s06g_48 mashrs_trabajo
ren s06g_47 desea_mashrs_sem_pasada
ren ocupado pob_ocu 
ren s04b_13 hijos_vivos
ren s04b_12 hijosnac_vivos
ren s02a_04b mes_nac
ren s06a_01 job  

 *eliminar hogares que tienen hijos adoptados 
gen adopt = (parentesco==4) 
	egen adopta=total(adopt), by (folio)
	gen adoptb= (adopta >0)
	drop if adoptb==1
/// generacion de variables
* Variable sons
	gen sons = (parentesco==3) /*variable que tiene el valor de 1 para los miembros que sean menores o iguales a 13 años*/
	egen sonsa=total(sons), by (folio)
	label var sonsa "Número de hijos en los hogares"
	gen sonsb= (sonsa >0)
	label var sonsb "Presencia de hijos en el hogar" 
	label define sonsb  1 "Con hijos"  0 "Sin hijos" 
	   label values sonsb sonsb     
** Edad del hijo mayor 
egen edadhijo_max=max(edad) if parentesco==3, by (folio)
egen edadhijo_maxa=mean(edadhijo_max), by (folio)

*Edad al cuadrado
cap drop edad2
gen edad2=edad*edad

* Estado civil
cap drop married
gen married=(est_civil==2)
cap drop single
gen single=(est_civil==1) 
cap drop divorced
gen divorced=(est_civil==5)
 
* Hijos por edades
cap drop kids6
gen kids6=(parentesco==3 & edad<=6) 
egen kids6a=total(kids6), by (folio)
gen kids6b=(kids6a>0)

cap drop kids714
gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio)
gen kids714b=(kids714a>0)

* Pertenencia etnica 
gen etnia_b=(etnia==1) 
label define etnia_b 1 "si pertenece" 0 "no pertenece"
label values etnia_b etnia_b

* Nivel de educacion 
gen edu_level=0 if niv_ed_g  ==0
replace edu_level=1 if niv_ed_g ==1
replace edu_level=2 if niv_ed_g ==2
replace edu_level=3 if niv_ed_g ==3

gen edu_sup=(edu_level==3)
gen edu_9=(aestudio>9)

*Experiencia 
cap drop exp
gen exp=edad-aestudio-6
drop if exp<0
replace exp=0 if exp<0

cap drop expsq
gen exp2=exp*exp

egen hhsize = count(nro), by(folio)
gen region = (depto==2 | depto==3 | depto==7) if !missing(depto)
gen urban=(area==1)
gen firsthalf=(mes_nac<=6)

*Log ingreso
drop if ylab==0
cap drop lnlab
gen lnylab=ln(ylab)
cap drop lnyhog
gen lnyhog=ln(yhog)

gen more2sons=(sonsa>2)

* numero de niños 

gen kids=0 if sonsa==0
replace kids=1 if sonsa==1 
replace kids=2 if sonsa==2 
replace kids=3 if sonsa==3
replace kids=4 if sonsa==4
replace kids=5 if sonsa>4


/// Seleccion de la muestra 
*drop if edadhijo_maxa>18 & sonsb==1
keep if parentesco ==1 | parentesco==2
keep if sexo == 2 
keep if edad>18 &  edad<=44
drop if edadhijo_maxa>12 & sonsb==1
****************************************************************************
/// Analisis descriptivo 
tab job [w=factor]
tab sonsa [w=factor]
 
/// Modelo Econometrico 
** Heckman 

*tasa de empleo
cap drop templeo
gen templeo=pob_ocu/pea

probit pea edad edad2 married single divorced kids6b kids714b [pw=factor] if sexo==2 & edad>18 & edad<=44 & urban==1
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1 

probit pea edad edad2 married single divorced kids714b [pw=factor] if sexo==2 & edad>18 & edad<=44 
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1 

/*MINCER
reg lnylab exp exp2 aestudio [pw=factor] if sexo==2 & edad>18 & urban==1, r
reg lnylab exp exp2 aestudio aux [pw=factor] if sexo==2 & edad>18 & urban==1, r
heckman lnylab exp exp2 aestudio [pw=factor] if sexo==2 & edad>18 & urban==1, select(married single divorced kids6b kids714b) twostep

reg lnylab exp exp2 aestudio etnia_b kids6b if sexo==2 & edad>18 &  edad<=44  & urban==1, r
reg lnylab exp exp2 aestudio etnia_b kids6b if sexo==2 & edad>18 &  edad<=44  & urban==0, r

reg lnylab exp exp2 aestudio etnia_b kids714b if sexo==2 & edad>18 &  edad<=44  & urban==1, r
reg lnylab exp exp2 aestudio etnia_b kids714b if sexo==2 & edad>18 &  edad<=44  & urban==0, r

reg lnylab exp exp2 aestudio etnia_b aux if sexo==2 & edad>18 &  edad<=44  & urban==1, r


heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==1, select(married single divorced kids6b) twostep
heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==0, select(married single divorced kids6b) twostep

heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==1, select(married single divorced kids714b) twostep
heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==0, select(married single divorced kids714b) twostep

heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==1, select(married single divorced kids6b kids714b) twostep


heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44  &  urban==0, select(married single divorced kids6b kids714b) twostep

* var dep : ingreso del hogar 
reg lnyhog exp exp2 aestudio etnia_b aux if sexo==2 & edad>18 &  edad<=44 & urban==1, r
heckman lnyhog exp exp2 aestudio etnia_b  if sexo==2 & edad>18 & urban==1, select(married single divorced kids6b kids714b) twostep*/
****************************************************************************
* MINCER 2 
probit pea edad edad2 married single divorced kids [pw=factor] if urban==1 
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1 
reg lnylab exp exp2 aestudio etnia_b aux if  urban==1, r
heckman lnylab exp exp2 aestudio etnia_b  if urban==1, select(married single divorced i.kids) twostep

reg lnylab exp exp2 aestudio etnia_b married single divorced i.kids, vce(robust)
heckman lnylab exp exp2 aestudio etnia_b , select(pea= married single divorced i.kids) vce(cluster upm)
heckman lnylab exp exp2 aestudio etnia_b  if urban==1, select(pea= married single divorced i.kids) vce(cluster upm)
heckman lnylab exp exp2 aestudio etnia_b  if urban==0, select(pea= married single divorced i.kids) vce(cluster upm)

/* MINCER 3
probit pea edad edad2 married single divorced more2sons [pw=factor] if sexo==2 & edad>18 & urban==1 & sonsb==1
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1

reg lnylab exp exp2 aestudio etnia_b aux if sexo==2 & edad>18 &  edad<=44 & urban==1 & sonsb==1, r
heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44 & urban==1 & sonsb==1, select(married single divorced more2sons) twostep*/

******
heckman wage educ age, select(married children educ age) vce(cluster county)
** postestimacion 
predict varols 
predict varheckman
test educacion 

