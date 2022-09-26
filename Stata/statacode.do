****Data preparation for "Measurement Equivalence in Sequential Mixed-Mode Surveys" https://doi.org/10.18148/srm/2022.v16i1.7811*********

FOLDER NAMES BLINDED


use "FOLDER\ns8_2015_self_completion.dta", clear
rename *, lower

replace w8audit1 = . if inlist(w8audit1,-9,-8,-1)

replace w8audit2 = . if inlist(w8audit2,-9,-8)
replace w8audit2 = 0 if w8audit2 == -1

replace w8audit6 = . if inlist(w8audit6,-9,-8)
replace w8audit6 = 0 if w8audit6 == -1

ren w8audit6 w8audit3

keep nsid w8casistart w8ghq12_1 w8ghq12_2 w8ghq12_3 w8ghq12_4 w8ghq12_5 w8ghq12_6 w8ghq12_7 w8ghq12_8 w8ghq12_9 w8ghq12_10 w8ghq12_11 w8ghq12_12 w8locus0a w8locus0b w8locus0c w8locus0d w8audit1 w8audit2 w8audit3

save "FOLDER\w8_self_completion_vars.dta", replace


use "FOLDER\MOLS2file_BDedit_5March19.dta", clear

rename _all, lower

merge 1:1 nsid using "FOLDER\weights_w8.dta"

drop _merge
merge 1:1 nsid using "FOLDER\W1 extra variables.dta"

drop _merge
merge 1:1 nsid using "FOLDER\W8 extra variables.dta"

drop _merge
merge 1:1 nsid using "FOLDER\w8_self_completion_vars.dta"


replace w8ghq12_1 = . if inlist(w8ghq12_1,-9,-8,-1)
replace w8ghq12_2 = . if inlist(w8ghq12_2,-9,-8,-1)
replace w8ghq12_3 = . if inlist(w8ghq12_3,-9,-8,-1)
replace w8ghq12_4 = . if inlist(w8ghq12_4,-9,-8,-1)
replace w8ghq12_5 = . if inlist(w8ghq12_5,-9,-8,-1)
replace w8ghq12_6 = . if inlist(w8ghq12_6,-9,-8,-1)
replace w8ghq12_7 = . if inlist(w8ghq12_7,-9,-8,-1)
replace w8ghq12_8 = . if inlist(w8ghq12_8,-9,-8,-1)
replace w8ghq12_9 = . if inlist(w8ghq12_9,-9,-8,-1)
replace w8ghq12_10 = . if inlist(w8ghq12_10,-9,-8,-1)
replace w8ghq12_11 = . if inlist(w8ghq12_11,-9,-8,-1)
replace w8ghq12_12 = . if inlist(w8ghq12_12,-9,-8,-1)


replace w8locus0a = . if inlist(w8locus0a,-9,-8,-1)
replace w8locus0b = . if inlist(w8locus0b,-9,-8,-1)
replace w8locus0c = . if inlist(w8locus0c,-9,-8,-1)
replace w8locus0d = . if inlist(w8locus0d,-9,-8,-1)



gen designweight_combin = designweight_web if w8mode == 1
replace designweight_combin = designweight_webtel if w8mode == 2
replace designweight_combin = designweight_webtelftf if w8mode == 3

drop if designweight_combin == .


keep designweight_combin w8mode w8ghq12_1-w8ghq12_12 samppsu sampstratum w8adult0a w8adult0b w8adult0c w8locus0a w8locus0b w8locus0c w8locus0d ///
w8leisurea0a w8leisurea0b w8leisurea0c w8leisurea0d w8leisurea0e w8leisurea0f w8leisureb0a w8leisureb0b w8leisureb0c w8leisureb0d ///
w8bullytype0a w8bullytype0b w8bullytype0c w8bullytype0d w8bullytype0e w8bullytype0f w8bullytype0g w8audit1 w8audit2 w8audit3 ///
w8dghqsc

ren w8ghq12_1 ghq_1
ren w8ghq12_2 ghq_2
ren w8ghq12_3 ghq_3
ren w8ghq12_4 ghq_4
ren w8ghq12_5 ghq_5
ren w8ghq12_6 ghq_6
ren w8ghq12_7 ghq_7
ren w8ghq12_8 ghq_8
ren w8ghq12_9 ghq_9
ren w8ghq12_10 ghq_10
ren w8ghq12_11 ghq_11
ren w8ghq12_12 ghq_12

ren w8adult0a adult0a
ren w8adult0b adult0b
ren w8adult0c adult0c

ren w8locus0a locus0a
ren w8locus0b locus0b 
ren w8locus0c locus0c
ren w8locus0d locus0d

ren w8leisurea0a leisa0a
ren w8leisurea0b leisa0b
ren w8leisurea0c leisa0c
ren w8leisurea0d leisa0d
ren w8leisurea0e leisa0e
ren w8leisurea0f leisa0f

ren w8leisureb0a leisb0a
ren w8leisureb0b leisb0b
ren w8leisureb0c leisb0c
ren w8leisureb0d leisb0d


ren w8bullytype0a bully0a
ren w8bullytype0b bully0b
ren w8bullytype0c bully0c
ren w8bullytype0d bully0d
ren w8bullytype0e bully0e
ren w8bullytype0f bully0f
ren w8bullytype0g bully0g


ren w8audit1 audit1
ren w8audit2 audit2
ren w8audit3 audit3




keep designweight_combin w8mode ghq_1-ghq_12 samppsu sampstratum adult0a adult0b adult0c locus0a locus0b locus0c locus0d ///
leisa0a leisa0b leisa0c leisa0d leisa0e leisa0f leisb0a leisb0b leisb0c leisb0d ///
bully0a bully0b bully0c bully0d bully0e bully0f bully0g audit1 audit2 audit3 ///
w8dghqsc


mean ghq_1, over(w8mode)
mean ghq_2, over(w8mode)
mean ghq_3, over(w8mode)
mean ghq_4, over(w8mode)
mean ghq_5, over(w8mode)
mean ghq_6, over(w8mode)
mean ghq_7, over(w8mode)
mean ghq_8, over(w8mode)
mean ghq_9, over(w8mode)
mean ghq_10, over(w8mode)
mean ghq_11, over(w8mode)
mean ghq_12, over(w8mode)

mean w8dghqsc, over(w8mode)




svyset samppsu [pweight=designweight_combin], strata(sampstratum)

svy: mean ghq_1, over(w8mode)
svy: mean ghq_2, over(w8mode)
svy: mean ghq_3, over(w8mode)
svy: mean ghq_4, over(w8mode)
svy: mean ghq_5, over(w8mode)
svy: mean ghq_6, over(w8mode)
svy: mean ghq_7, over(w8mode)
svy: mean ghq_8, over(w8mode)
svy: mean ghq_9, over(w8mode)
svy: mean ghq_10, over(w8mode)
svy: mean ghq_11, over(w8mode)
svy: mean ghq_12, over(w8mode)

svy: mean w8dghqsc, over(w8mode)

svy: mean ghq_1
svy: mean ghq_2
svy: mean ghq_3
svy: mean ghq_4
svy: mean ghq_5
svy: mean ghq_6
svy: mean ghq_7
svy: mean ghq_8
svy: mean ghq_9
svy: mean ghq_10
svy: mean ghq_11
svy: mean ghq_12

***EXPORT TO MPLUS

stata2mplus using "FOLDER\mplusdata", replace

save "FOLDER\paper4data.dta", replace

