****** Tesis Edison Choque Sanchez 
cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Ipums_data"
use abadie_bol_censo, clear 

set dp comma 
ren bo2012a_resdept depto_reside
ren bo2012a_inact pei 
ren bo2012a_empstat est_empleo
*** generacion de variables 
gen edad_fb= edad - edadhijo_max
gen eje_region=(depto_reside==2 | depto_reside==3 | depto_reside==7)

drop if pei ==1
drop if edad_fb<18

*svyset upm [pw=factor], strata(estrato)
keep if area==2

********************
tab job  [aw=factor]
tab job pareja [aw=factor], col

tab more2sons  [aw=factor]
tab more2sons pareja [aw=factor], col

tab samesex  [aw=factor]
tab samesex pareja [aw=factor], col

tab twoboys  [aw=factor]
tab twoboys pareja [aw=factor], col

tab twogirls  [aw=factor]
tab twogirls pareja [aw=factor], col

tab hijo1_sexa  [aw=factor]
tab hijo1_sexa pareja [aw=factor], col

tab hijo2_sexa  [aw=factor]
tab hijo2_sexa pareja [aw=factor], col

tab etnia_b  [aw=factor]
tab etnia_b pareja [aw=factor], col

tab eje_region  [aw=factor]
tab eje_region pareja [aw=factor], col

sum sonsa  [aw=factor]
sum sonsa  [aw=factor] if pareja==1 

sum sonsa  [aw=factor] if edadhijo_max<=12
sum sonsa  [aw=factor] if pareja==1 & edadhijo_max<=12 

sum edad  [aw=factor]
sum edad  [aw=factor] if pareja==1 

sum edad_fb  [aw=factor]
sum edad_fb  [aw=factor] if pareja==1 

**The process is reapeated for the following list of categorical variables (because svy)
*gender ethnic maritalstatus remitt eduattainment firmsize ecsector occupation depto urban 
**For continious variables
svy: mean tenure if info==1  // Formal
svy: mean tenure if info==0  // Informal
svy: mean tenure if hhpo==1  // Poor
svy: mean tenure if hhpo==0  // Non-Poor