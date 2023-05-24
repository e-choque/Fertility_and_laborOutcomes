/// Tesis de grado Edison Choque Sanchez - Economia UMSA 
/// Replica de resultados Agrist (2001) u guia para tesis 
*** mas comandos que explorar 
testnl /*para evaluar las diferencias del existo laboral entre dos grupos */ 
tobit /* explorar tobot as a corner solution pag 2822  tobit usa una distribucion censurada **/
truncreg /* pero se comporta menos bien que tobit ver ej psg 2868 ej 1  */ 

****************************************************************** 
**************************************************************************
cd "H:\Mi unidad\Consultorias\Tesis_Edison\Stata"
use edsa_2016_abadie, clear 

set dp comma
ren ms01_0108_1 pert_etnia 
*** generacion de variables 
gen etnia=(pert_etnia==1)
gen edu=0 if nivedum ==1 
replace edu=1 if nivedum==2 | nivedum==3 | nivedum==4

gen capital=(pobmun==4 | pobmun==5)

gen nro_hijo_a=0 if (nro_hijo==.)
replace nro_hijo_a=1 if nro_hijo==1
replace nro_hijo_a=2 if nro_hijo==2
replace nro_hijo_a=3 if nro_hijo==3
replace nro_hijo_a=4 if nro_hijo==4
replace nro_hijo_a=5 if nro_hijo==5
replace nro_hijo_a=6 if nro_hijo==6
replace nro_hijo_a=7 if nro_hijo==7
replace nro_hijo_a=8 if nro_hijo==8
replace nro_hijo_a=9 if nro_hijo==9

gen nro_hijo_b=(nro_hijo_a>0)

gen edad_fb=edad_mujer - edadhijo_max if nro_hijo_b==1 
drop if edad_fb<18 & nro_hijo_b==1 

* no estudiantes y quienen no tuvieron un encuentro sexual

*drop if edadhijo_max>=13 & nro_hijo_b==1 
***************************************************
*ivprobit ocu_mujer i.edad_mujer2 i.nivedum sex_first married_union i.edu_pareja (sons_c=infertil)  [pweight = ponderadorm]
*margins, dydx(*) predict(pr)
*reg ocu_mujer edad_mujer edu region g_pob etnia nro_hijo_a if area==1, vce(boot, reps(100))

* Columna A
probit ocu_mujer nro_hijo_a edad_mujer nivedum etnia married_union capital edu_pareja [pweight = ponderadorm], vce(robust) 

probit ocu_mujer nro_hijo_a edad_mujer nivedum etnia married_union capital edu_pareja [pweight = ponderadorm]  if area==1, vce(robust) 
mfx

outreg2 using regression_result, replace mfx see excel dec(2)
* Columna B y D
ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil) [pweight = ponderadorm], vce(robust)

ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil1 infertil2), vce(robust)

*ivreg2 ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil1 infertil2) ---> te muestra test de yogo (2005) y otros test
 
*estat firststage
weakivtest 
estat overid 

ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil) [pweight = ponderadorm] if area==1, r 

ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil) if area==1, r 
ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil1 infertil2)  if area==2, vce(robust)
weakivtest 
outreg2 using regression_result, replace excel dec(2)
outreg2 using regression_result, replace excel see dec(2)

* Columna C

ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja (nro_hijo_a= infertil), vce(boot, reps(500))

*estat firststage
weakivtest /*not allowed*/ 

ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja  (nro_hijo_a= infertil) if area==1, vce(boot, reps(500))


outreg2 using regression_result, replace excel see dec(2)
*** mas instrumentos 
*ivprobit ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja  (nro_hijo_a= sex_first)  [pweight = ponderadorm] 
*ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja  (nro_hijo_a= sex_first) [pweight = ponderadorm] if area==1, r 
*ivregress gmm ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja  (nro_hijo_a= infertil sex_first) if area==1, r 
estat firststage
estat endegenous
estat overid 

********************************************************
/*keep if married_union==1

probit ocu_mujer nro_hijo_a edad_mujer nivedum etnia  capital edu_pareja [pweight = ponderadorm]  if area==1, vce(robust) 
ivregress gmm ocu_mujer edad_mujer nivedum etnia  capital edu_pareja  (nro_hijo_a= infertil) [pweight = ponderadorm] if area==1, r 
ivregress gmm ocu_mujer edad_mujer nivedum etnia  capital edu_pareja  (nro_hijo_a= infertil) if area==1, vce(boot, reps(500))

estat firststage
estat endegenous
estat overid 
*/



