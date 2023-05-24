****** Tesis Edison Choque Sanchez 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use abadie_fembol_censo2001, clear 

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

*keep if urban==1

********************
tab job  [w=w_person]
tab job pareja [w=w_person], col

tab more2sons  [w=w_person]
tab more2sons pareja [w=w_person], col

tab samesex  [w=w_person]
tab samesex pareja [w=w_person], col

tab twoboys  [w=w_person]
tab twoboys pareja [w=w_person], col

tab twogirls  [w=w_person]
tab twogirls pareja [w=w_person], col

tab hijo1_sexa  [w=w_person]
tab hijo1_sexa pareja [w=w_person], col

tab hijo2_sexa  [w=w_person]
tab hijo2_sexa pareja [w=w_person], col

tab etnia_b  [w=w_person]
tab etnia_b pareja [w=w_person], col

/*tab eje_region  [w=w_person]
tab eje_region pareja [w=w_person], col*/

sum sonsa  [w=w_person]
sum sonsa  [w=w_person] if pareja==1 

sum sonsa  [w=w_person] if edadhijo_max<=12
sum sonsa  [w=w_person] if pareja==1 & edadhijo_max<=12 

sum edad  [w=w_person]
sum edad  [w=w_person] if pareja==1 

sum edad_fb  [w=w_person]
sum edad_fb  [w=w_person] if pareja==1 

tab job more2sons if urban==0, row
tab job more2sons if urban==1, row
 

**The process is reapeated for the following list of categorical variables (because svy)
*gender ethnic maritalstatus remitt eduattainment firmsize ecsector occupation depto urban 
**For continious variables
svy: mean tenure if info==1  // Formal
svy: mean tenure if info==0  // Informal
svy: mean tenure if hhpo==1  // Poor
svy: mean tenure if hhpo==0  // Non-Poor