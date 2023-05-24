cd "G:\Consultorias\IIE-UMSA\Tesis_LicenciaturaEconomia_Edison\Stata_basesDatos"
use abadie_mcv20002001, clear

*drop if pea == 0
gen edad_fb = edad - edadhijo_max
drop if edad_fb<18

keep if area==1

*****
*tab info more2sons, col
tab samesex more2sons, col

tab etnia_b more2sons , col
tab region more2sons , col
tab pareja more2sons , col
tab granfb more2sons , col

table more2sons, c (mean tothrs sd tothrs)
table more2sons, c (mean ylabr sd ylabr)
table more2sons , c (mean edad sd edad)
table more2sons , c (mean edad_fb sd edad_fb)