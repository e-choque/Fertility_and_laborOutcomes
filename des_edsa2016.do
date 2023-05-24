*** Tabla descriptiva EDSA 2016
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

gen kid=0 if (nro_hijo_a==0)
replace kid= 1 if (nro_hijo_a==1 | nro_hijo_a==2)
replace kid= 2 if (nro_hijo_a==3 | nro_hijo_a==4)
replace kid= 3 if nro_hijo_a>4

gen edu_sup=(nivedum==4)

gen edu_sup_pareja=(edu_pareja==3)

* no estudiantes y quienen no tuvieron un encuentro sexual
*drop if edadhijo_max>12 & nro_hijo_b==1 
gen edad_fb=edad_mujer - edadhijo_max if nro_hijo_b==1 
drop if edad_fb<18 & nro_hijo_b==1 

*****
* variables ---> ocu_mujer edad_mujer nivedum etnia married_union capital edu_pareja

tabout ocu_mujer married_union edu_sup edu_sup_pareja capital etnia kid using  des_edsa.xls [aweight = ponderadorm], c(col) f(1)  dpcomma replace 

tabout  edu_sup_pareja  kid if married_union==1 using  des_edsa.xls [aweight = ponderadorm], c(col) f(1)  dpcomma replace 

tabout kid using des_edsa.xls [aw = ponderadorm] , oneway cells(mean edad_mujer sd edad_mujer mean edad_fb sd edad_fb) f(1) sum  clab(1 2 3 4) dpcomma replace

tabout edad_mujer2 if infertil ==1 using des_edsa.xls [aw = ponderadorm] , oneway cells(mean nro_hijo_a) f(1) sum  clab(mean sd) dpcomma replace

svyset [w = ponderadorm]
tabout edad_mujer2 if infertil ==0 using des_edsa.xls, oneway cells(mean nro_hijo_a lb ub) f(2) sum svy level (95) dpcomma replace 




