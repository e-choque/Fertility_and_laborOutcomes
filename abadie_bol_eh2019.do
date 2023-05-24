cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use abadie_bol_eh2019.dta, clear 

drop if pea==0
drop if edad >35