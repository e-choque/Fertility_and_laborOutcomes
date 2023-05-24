cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use censo_2001, clear 

ren bo2001a_urban area 
ren bo2001a_sex sexo
 ren bo2001a_age edad
ren bo2001a_relate parentesco 

/// generacion de variables 
gen adopt = (parentesco==4)
	egen adopta=total(adopt), by (serial)
	gen adoptb= (adopta >0)
drop if adoptb>0 
* Variable sons
	gen sons = (parentesco==3)
	egen sonsa=total(sons), by (serial)
	label var sonsa "Número de hijos en los hogares"
	gen sonsb= (sonsa >0)
	label var sonsb "Presencia de hijos en el hogar" 
	label define sonsb  1 "Con hijos"  0 "Sin hijos" 
	   label values sonsb sonsb  

keep if parentesco==3  
	      
egen edadhijo_max=max(edad), by (serial)
gen more2sons=(sonsa>2)

drop if sonsa<=1
drop if edad==0 
drop if edadhijo_max>18

*** Para hijo1 mayor 
sort serial
by serial: gen hijo1_sex=sexo if edad==edadhijo_max
egen hijo1_sexa=mean(hijo1_sex), by(serial)

keep if hijo1_sexa==1 | hijo1_sexa==2 

/// comprobando 
*gen ch=1 if edadhijo_maxa==edad  --- si se comprobo 

drop if edadhijo_max==edad 

egen edadhijo_maxb=max(edad), by (serial)

by serial: gen hijo2_sex=sexo if edad==edadhijo_maxb
egen hijo2_sexa=mean(hijo2_sex), by(serial)

keep if hijo2_sexa==1 | hijo2_sexa==2 

gen samesex=(hijo1_sexa==hijo2_sexa)
sort serial
by serial: keep if _n==1 

save "cr_samesex_2001", replace
*************************************************************************
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use censo_2001, clear 

ren bo2001a_urban area 
ren bo2001a_sex sexo
ren bo2001a_age edad
ren bo2001a_relate parentesco 

ren bo2001a_marst est_civil
ren bo2001a_indig   etnia
*ren bo2001a_pernum dentro_hog
*ren bo2001a_ndeaths  nro_muertes
ren bo2001a_chsurv niños_vivos
ren bo2001a_chborn niños_nac
*ren bo2001a_disab existe_inhabil
ren bo2001a_levcomp edu_level_comp
ren bo2001a_wkrlstwk job_lastweek
ren perwt w_person

/// variable samesex
merge m:1 serial using "cr_samesex_2001", keepusing (more2sons sonsa sonsb edadhijo_max hijo1_sexa hijo2_sexa samesex) /*not match parecen haber hijos con la misma edad y otras incoherencias */
keep if _merge==3
drop _merge
label var  samesex "mismo sexo"
label define samesex 1 "dos primeros hijos tienen el mismo sexo" 0 "dos primeros hijos no tienen mismo sexo"
label values samesex samesex

*** abuelos y bisabuelos de los hijos 
gen granf = (parentesco==5)
	egen granfa=total(granf), by (serial)
	label var granfa "Numero de abuelos o bisabuelos de los hijos"
	gen granfb= (granfa >0)
	label var granfb "Presencia de abuelos o bisabuelos de los hijos" 
	label define granfb  1 "Con granf"  0 "Sin granf" 
	   label values granfb granfb  
/// otros instrumentos 
gen twoboys=1 if hijo1_sexa==1 & hijo2_sexa==1
replace twoboys=0 if twoboys==.
gen twogirls=1 if hijo1_sexa==2 & hijo2_sexa==2
replace twogirls=0 if twogirls==.

/// generacion de variables 
gen pareja=(est_civil==2 | est_civil==3)
label var  pareja "Estado civil"
label define pareja 1 "Conviviendo o casado (a)" 0 "otro"
label values pareja pareja

gen etnia_b=(etnia==1 | etnia==2 | etnia==3 | etnia==4 | etnia==5 | etnia==6)
label var  etnia_b "Indigena"
label define etnia_b 1 "Sí pertenece" 0 "No pertenece"
label values etnia_b etnia_b

*gen existe_inhabila=(existe_inhabil==1)

/*gen edu_level=0 
replace edu_level=1 if edu_level_aprob==2 | edu_level_aprob==3 | edu_level_aprob==4 | edu_level_aprob==5 | edu_level_aprob==6 | edu_level_aprob==7 | edu_level_aprob==9 
replace edu_level=2 if edu_level_aprob==8 | edu_level_aprob==10
replace edu_level=3 if edu_level_aprob==11 | edu_level_aprob==12 | edu_level_aprob==13 | edu_level_aprob==14 | edu_level_aprob==15 | edu_level_aprob==16 | edu_level_aprob==17 

label var  edu_level "Nivel educativo"
label define edu_level 0 "Nulo o na" 1 "Primaria" 2 "Secundaria" 3 "Tecnico o Educación superior"
label values edu_level edu_level*/

gen head_hombre=(parentesco==1 & sexo==1)
gen head_mujer=(parentesco==1 & sexo==2)

gen job=(job_lastweek==1)
label var  job "Trabajo la anterior semana "
label define job 0 "No trabajo" 1 "Sí trabajo"
label values job job

/// otras restriciones para la muestra 
** no se puede comprobar si todos los niños o hijos estan vivos ya que has varios valores niu (not in universe)
keep if parentesco==1 | parentesco==2 

/*sort serial
by serial: gen edu_esposo=edu_level[_n] if head_hombre==1
egen edu_esposoa=mean(edu_esposo), by(serial)

by serial: gen edu_esposob=edu_level[_n+1] if head_mujer==1

egen edu_esposoc=rowtotal(edu_esposoa edu_esposob)*/

keep if sexo==1
*keep if sexo==2 /*2 es mujer*/
*keep if edad >18 & edad<45
keep if edad >18 & edad<45
save "abadie_menbol_censo2001", replace 
*save "abadie_fembol_censo2001", replace
