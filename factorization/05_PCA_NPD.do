clear all
run "...path\fre.ado"
run "...path\mdesc.ado"

******************************************************************************
* FACTORIZATION SCHOOL-RELATED DIFFICULTIES (MAIN SAMPLE)
******************************************************************************
*** School-Related Difficulties ***
use "...path\clean_data.dta", clear
summ id_dfe share_absences share_uabsences absences_exc num_absences num_uabsences num_sessions num_fexc sessions_fexc perm_exc num_schools ks1_score ks2_score nomschool sen excluded missing_vars

keep if missing_vars == 0

*** Keep pupils with full EYFSP information
merge 1:1 id_dfe using "...path\eyfsp_clean.dta", keepusing(score_eyfsp_overall) 
keep if _merge == 3
drop _merge

count if score_eyfsp_overall == 0 //no evaluated
drop if score_eyfsp_overall == 0


*** Cleaning school related difficulties
tab num_fexc
tab perm_exc

summ num_fexc if num_fexc !=0, d
recode num_fexc (2/6 = 2) (7/43 = 3), gen(num_fex_cat)
replace num_fex_cat = 4 if perm_exc == 1
tab num_fex_cat

tab sessions_fexc
*Children have 380 sessions (190 x 2 sessions morning and afternoon) in an acdemic year
*1 	no exlusions
*2	less than a week
*3	less than a month
*4	more than a month
recode sessions_fexc (0 = 1) (1/9 = 2) (10/39 = 3) (40/192 = 4), gen(sessions_fexc_cat)
tab sessions_fexc_cat

egen ks1_score_std = std(-ks1_score)

gen persisten_absent = (share_absences >= 0.15)
tab persisten_absent

gen ks1_nolevel = (ks1_score < 13)
replace ks1_nolevel = . if ks1_score == .
tab ks1_nolevel

*** PCA for School Difficulties
corr persisten_absent sessions_fexc_cat perm_exc num_schools ks1_score_std ///
ks1_nolevel share_absences share_uabsences absences_exc num_absences num_uabsences num_sessions num_fexc sessions_fexc perm_exc num_schools nomschool sen excluded

tab excluded
tab persisten_absent
gen excluded_absent = (excluded == 1 | persisten_absent == 1)
tab excluded_absent

*Sebba (2015) uses: share_uabsences num_fexc perm_exc nomschool 
polychoric persisten_absent num_schools sessions_fexc_cat perm_exc
display r(sum_w)
global N = r(sum_w)
matrix r = r(R)
pcamat r, n($N) 
predict school_diff
label var school_diff "school_diff"

summ school_diff
_pctile school_diff, p(99)
local p99 =  r(r1)
replace school_diff = `p99' if school_diff > `p99'

egen school_diff_std = std(school_diff)
kdensity school_diff_std, bw(0.4)
summ school_diff_std

**** Continuous ***
foreach var of varlist share_absences num_schools num_fexc {
	summ `var'
	replace `var' = (`var' - r(mean))/r(sd)
}
pca share_absences num_schools num_fexc 
predict school_diff_alt

summ school_diff_alt, d
_pctile school_diff_alt, p(99)
local p99 =  r(r1)
replace school_diff_alt = `p99' if school_diff_alt > `p99'

egen school_diff_alt_std = std(school_diff_alt)
kdensity school_diff_alt_std, bw(0.4)
summ school_diff_alt_std

******************************************************************************
* FACTORIZATION SOCIAL CARE 
******************************************************************************
summ id_dfe cin age_start_cin episodes_cin cpp num_cpp previous_cpp 
summ cla num_placement placement_changes placement_lenght 

*** CIN ***
*Reverse age 
tab age_start_cin
*4	before EYFSP
*3	during EYFSP
*2 	after EYFSP and before KS2
*1	during KS2
recode age_start_cin (0/4 = 4) (5 =3) (6/10 = 2) (11/12 = 1), gen(age_start_cin_cat)
tab age_start_cin_cat

tab episodes_cin //from 1-4(4 or more episodes)
recode episodes_cin (4/7 = 4), gen(num_episodes_cin)
tab num_episodes_cin

tab num_cpp
recode num_cpp (3/15 = 3), gen(num_cpp_cin)
tab num_cpp_cin

corr age_start_cin_cat num_episodes_cin num_cpp_cin 

*** CLA ***
tab num_placement
recode num_placement (4/7 = 4) (0 = .), gen(num_placement_cla)
tab num_placement_cla

tab placement_lenght
*1	less than 1 year
*2	between 1 and 2 years
*3 	between 2 and 3 years
*4 	between 3 and 4 years
*5  between 4 and 5 years
*6  more than 6 years
recode placement_lenght (1/11 = 1) (12/24 = 2) (25/36 = 3) (37/48 = 4) (48/60 = 5) (61/138 = 6) (0 = .), gen(placement_lenght_cla)
tab placement_lenght_cla

corr num_placement_cla placement_lenght_cla 
replace num_placement_cla = 0 if num_placement_cla == . & cin == 1
replace placement_lenght_cla = 0 if placement_lenght_cla == . & cin == 1

*Sebba (2015) uses: placement change after KS2, mean placement length KS2, number of residential placements after KS2, whether placement residential or other care, lenght of latest placement
polychoric age_start_cin_cat num_episodes_cin num_cpp_cin cla
display r(sum_w)
global N = r(sum_w)
matrix r = r(R)
pcamat r, n($N) //51% total variation captured by the variables in the PCA
predict care_diff
label var care_diff "Social care difficulties"

egen care_diff_std = std(care_diff)
summ care_diff_std
kdensity care_diff_std, bw(0.4)

**** Continuous ***
replace age_start_cin = - age_start_cin
foreach var of varlist age_start_cin num_episodes num_cpp_cin {
	summ `var'
	replace `var' = (`var' - r(mean))/r(sd)
}
summ age_start_cin num_episodes num_cpp_cin cla
pca age_start_cin num_episodes num_cpp_cin cla //41% total variation captured by the variables in the PCA 
predict care_diff_alt

egen care_diff_alt_std = std(care_diff_alt)
summ care_diff_alt_std, d
kdensity care_diff_alt_std, bw(0.4)

keep id_dfe school_diff school_diff_alt care_diff care_diff_alt excluded_absent
save "...path\pca_scores.dta", replace