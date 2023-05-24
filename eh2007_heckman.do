//// eh 2007
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use EH2007_Poblacion, clear 

ren s1_06 parentesco 
ren s1_04 edad 
ren s1_03 sexo 
ren s1_12 est_civil
ren s5_52 mashrs_trabajo
ren s5_01 job 
ren s3_11 hijos_vivos
ren s3_10 hijosnac_vivos
ren auto_ind etnia 

///generacion de variables
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
 
/*Hijos por edades
cap drop kids6
gen kids6=(parentesco==3 & edad<=6) 
egen kids6a=total(kids6), by (folio)
gen kids6b=(kids6a>0)

cap drop kids714
gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio)
gen kids714b=(kids714a>0)*/

* Pertenencia etnica 
gen etnia_b=(etnia==2 | etnia==3 | etnia==4) 
label define etnia_b 1 "si pertenece" 0 "no pertenece"
label values etnia_b etnia_b

* Nivel de educacion 
gen edu_level=0 if niv_ed_g  ==0
replace edu_level=1 if niv_ed_g ==1
replace edu_level=2 if niv_ed_g ==2
replace edu_level=3 if niv_ed_g ==3

gen edu_sup=(edu_level==3)


*Experiencia 
cap drop exp
gen exp=edad-a_oe-6
drop if exp<0
replace exp=0 if exp<0

cap drop expsq
gen exp2=exp*exp

egen hhsize = count(nro), by(folio)
gen ejeregion = (depto==2 | depto==3 | depto==7) if !missing(depto)
gen urban=(urb_rur==1)
*Log ingreso
drop if ylabf==0
gen ylabf_r= ylabf/0.981363004510625
cap drop lnlab
gen lnylabr=ln(ylabf_r)
*cap drop lnyhog
*gen lnyhog=ln(yhog)

*gen more2sons=(sonsa>2)

* numero de niños 

gen kids=0 if sonsa==0
replace kids=1 if sonsa==1 
replace kids=2 if sonsa==2 
replace kids=3 if sonsa>2

gen kids_a=(kids>0)
********************************************
/// Seleccion de la muestra 
*drop if edadhijo_maxa>18 & sonsb==1
keep if parentesco ==1 | parentesco==2
keep if sexo == 2 
keep if edad>18 &  edad<=44
drop if edadhijo_maxa>12 & sonsb==1
** pea= p_ocupada + p_cesante + p_asirante 
gen pea = (condact==1 | condact==2 | condact==3)
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

*heckman lnylab exp exp2 a_oe etnia_b ejeregion , select(pea= married single divorced i.kids) vce(cluster upm)

**************************************
probit pea edad edad2 married single divorced i.kids [pw=factor] if urban==1 
probit pea married single  i.kids if urban==1, vce(cluster upm) 
margins, dydx(*)

reg lnylabr exp exp2 a_oe etnia_b ejeregion married single i.kids [w=factor] if urban==1 , vce(robust)

heckman lnylabr exp exp2 a_oe etnia_b ejeregion if urban==1, select(pea= married single i.kids) vce(cluster upm)

margins,

heckman lnylabr exp exp2 a_oe etnia_b ejeregion i.kids if urban==1, select(pea= married single ) vce(cluster upm)


heckman lnylabr exp exp2 a_oe etnia_b ejeregion  if urban==1, select(pea= married single i.kids) twostep

outreg2 using regression_result, replace excel dec(2)


heckman lnylabr exp exp2 a_oe etnia_b ejeregion if urban==0, select(pea= married single divorced i.kids) vce(cluster upm)


/* MINCER 3
probit pea edad edad2 married single divorced more2sons [pw=factor] if sexo==2 & edad>18 & urban==1 & sonsb==1
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1

reg lnylab exp exp2 aestudio etnia_b aux if sexo==2 & edad>18 &  edad<=44 & urban==1 & sonsb==1, r
heckman lnylab exp exp2 aestudio etnia_b  if sexo==2 & edad>18 &  edad<=44 & urban==1 & sonsb==1, select(married single divorced more2sons) twostep*/

******
*heckman wage educ age, select(married children educ age) vce(cluster county)
** postestimacion 
predict varols 
predict varheckman
test educacion 