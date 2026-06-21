********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 05_PCA_absences.do
* Purpose : Construct composite indices of school-related difficulties and
*           social care difficulties using polychoric PCA (categorical
*           variables) and standard PCA (continuous variables).
*
* Inputs  : clean_data.dta
*           eyfsp_clean.dta    (score_eyfsp_overall)
*
* Output  : pca_scores.dta     (school_diff, school_diff_alt,
*                                care_diff, care_diff_alt, excluded_absent)
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOAD AND RESTRICT SAMPLE
********************************************************************************

use "file_path\clean_data.dta", clear

summ id_dfe share_absences share_uabsences absences_exc num_absences ///
     num_uabsences num_sessions num_fexc sessions_fexc perm_exc ///
     num_schools ks1_score nomschool sen excluded missing_vars

keep if missing_vars == 0

merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(score_eyfsp_overall)
keep if _merge == 3
drop _merge

count if score_eyfsp_overall == 0
drop  if score_eyfsp_overall == 0


********************************************************************************
* SECTION 2 — SCHOOL-RELATED DIFFICULTIES
********************************************************************************

*------------------------------------------------------------------------------
* 2.1  Categorical exclusion variables
*------------------------------------------------------------------------------
tab num_fexc
tab perm_exc
summ num_fexc if num_fexc != 0, d

* Number of fixed exclusions (categorical)
*   0        = no exclusions
*   1        = one exclusion
*   2        = two to six exclusions
*   3        = seven or more exclusions
*   4        = permanently excluded (overrides fixed count)
recode num_fexc (2/6 = 2) (7/43 = 3), gen(num_fex_cat)
replace num_fex_cat = 4 if perm_exc == 1
tab num_fex_cat

* Sessions missed due to fixed exclusions (categorical)
*   1 = no exclusions
*   2 = less than one week   (1–9 sessions)
*   3 = less than one month  (10–39 sessions)
*   4 = more than one month  (40–192 sessions)
* NOTE: 380 sessions per academic year (190 days × 2 sessions).
tab sessions_fexc
recode sessions_fexc (0 = 1) (1/9 = 2) (10/39 = 3) (40/192 = 4), gen(sessions_fexc_cat)
tab sessions_fexc_cat

*------------------------------------------------------------------------------
* 2.2  Other school difficulty indicators
*------------------------------------------------------------------------------
egen ks1_score_std = std(-ks1_score)

* Persistent absence (>= 15% of possible sessions)
gen persistent_absent = (share_absences >= 0.15)
tab persistent_absent

* Below expected KS1 attainment (average point score < 13)
gen ks1_nolevel = (ks1_score < 13)
replace ks1_nolevel = . if ks1_score == .
tab ks1_nolevel

* Ever excluded or persistently absent
gen excluded_absent = (excluded == 1 | persistent_absent == 1)
tab excluded_absent

*------------------------------------------------------------------------------
* 2.3  Correlation inspection
*------------------------------------------------------------------------------
corr persistent_absent sessions_fexc_cat perm_exc num_schools ks1_score_std ///
     ks1_nolevel share_absences share_uabsences absences_exc ///
     num_absences num_uabsences num_sessions num_fexc sessions_fexc ///
     nomschool sen excluded

tab excluded
tab persistent_absent

*------------------------------------------------------------------------------
* 2.4  Polychoric PCA — categorical school difficulties
*      Following Sebba (2015): persistent_absent num_schools
*                               sessions_fexc_cat perm_exc
*------------------------------------------------------------------------------
polychoric persistent_absent num_schools sessions_fexc_cat perm_exc
display r(sum_w)
global N = r(sum_w)
matrix r = r(R)

pcamat r, n($N)
predict school_diff
label var school_diff "School difficulties (polychoric PCA)"

summ school_diff
_pctile school_diff, p(99)
local p99 = r(r1)
replace school_diff = `p99' if school_diff > `p99'

egen school_diff_std = std(school_diff)
kdensity school_diff_std, bw(0.4)
summ school_diff_std

*------------------------------------------------------------------------------
* 2.5  Standard PCA — continuous school difficulties
*      Variables: share_absences num_schools num_fexc (standardised)
*------------------------------------------------------------------------------
foreach var of varlist share_absences num_schools num_fexc {
	summ `var'
	replace `var' = (`var' - r(mean)) / r(sd)
}

pca share_absences num_schools num_fexc
predict absences_score
label var absences_score "School difficulties (continuous PCA)"

summ absences_score, d
_pctile absences_score, p(99)
local p99 = r(r1)
replace absences_score = `p99' if absences_score > `p99'

egen absences_score_std = std(absences_score)
kdensity absences_score_std, bw(0.4)
summ absences_score_std


********************************************************************************
* SECTION 3 — SOCIAL CARE DIFFICULTIES
********************************************************************************

summ id_dfe cin age_start_cin episodes_cin cpp num_cpp previous_cpp
summ cla num_placement placement_changes placement_length

*------------------------------------------------------------------------------
* 3.1  CIN categorical variables
*      Age at first referral (reversed: earlier = higher severity):
*        4 = age 0–4   (before EYFSP)
*        3 = age 5     (during EYFSP)
*        2 = age 6–10  (after EYFSP, before KS2)
*        1 = age 11–12 (during KS2)
*------------------------------------------------------------------------------
tab age_start_cin
recode age_start_cin (0/4 = 4) (5 = 3) (6/10 = 2) (11/12 = 1), gen(age_start_cin_cat)
tab age_start_cin_cat

* Number of CIN episodes (top-coded at 4+)
tab episodes_cin
recode episodes_cin (4/7 = 4), gen(num_episodes_cin)
tab num_episodes_cin
* BUG FIX: original used num_episodes (without _cin suffix) in the continuous
* PCA loop below — undefined variable. Corrected to num_episodes_cin.

* Number of CPP plans (top-coded at 3+)
tab num_cpp
recode num_cpp (3/15 = 3), gen(num_cpp_cin)
tab num_cpp_cin

corr age_start_cin_cat num_episodes_cin num_cpp_cin

*------------------------------------------------------------------------------
* 3.2  CLA categorical variables
*      Number of placements (top-coded at 4+; 0 coded as missing)
*      Placement length in months (categorised; 0 coded as missing)
*------------------------------------------------------------------------------
tab num_placement
recode num_placement (0 = .) (4/7 = 4), gen(num_placement_cla)
tab num_placement_cla

tab placement_length
* Categories (months):
*   1 = < 12 months
*   2 = 12–24 months
*   3 = 25–36 months
*   4 = 37–48 months
*   5 = 49–60 months
*   6 = 61+ months
recode placement_length (0 = .) (1/11 = 1) (12/24 = 2) (25/36 = 3) ///
                         (37/48 = 4) (49/60 = 5) (61/138 = 6), gen(placement_length_cla)
tab placement_length_cla
* BUG FIX: original recode had overlapping boundaries: (37/48 = 4) (48/60 = 5)
* — value 48 mapped to both categories 4 and 5 (Stata keeps the last match,
* so 48 → 5). Corrected to (37/48 = 4) (49/60 = 5) to make boundaries
* mutually exclusive.

corr num_placement_cla placement_length_cla

* Zero-fill CLA variables for CIN pupils who were never CLA
replace num_placement_cla  = 0 if num_placement_cla  == . & cin == 1
replace placement_length_cla = 0 if placement_length_cla == . & cin == 1

*------------------------------------------------------------------------------
* 3.3  Polychoric PCA — categorical social care difficulties
*      Following Sebba (2015): age_start_cin_cat num_episodes_cin
*                               num_cpp_cin cla
*------------------------------------------------------------------------------
polychoric age_start_cin_cat num_episodes_cin num_cpp_cin cla
display r(sum_w)
global N = r(sum_w)
matrix r = r(R)

pcamat r, n($N)
predict care_diff
label var care_diff "Social care difficulties (polychoric PCA)"

egen care_diff_std = std(care_diff)
summ care_diff_std
kdensity care_diff_std, bw(0.4)

*------------------------------------------------------------------------------
* 3.4  Standard PCA — continuous social care difficulties
*      Age reversed (earlier referral = higher score) then standardised.
*      Variables: age_start_cin num_episodes_cin num_cpp_cin cla
*------------------------------------------------------------------------------
replace age_start_cin = -age_start_cin

foreach var of varlist age_start_cin num_episodes_cin num_cpp_cin {
	summ `var'
	replace `var' = (`var' - r(mean)) / r(sd)
}

summ age_start_cin num_episodes_cin num_cpp_cin cla
pca age_start_cin num_episodes_cin num_cpp_cin cla
predict care_diff_alt
label var care_diff_alt "Social care difficulties (continuous PCA)"

egen care_diff_alt_std = std(care_diff_alt)
summ care_diff_alt_std, d
kdensity care_diff_alt_std, bw(0.4)


********************************************************************************
* SECTION 4 — SAVE
********************************************************************************

keep id_dfe school_diff absences_score care_diff care_diff_alt excluded_absent
mdesc
compress

save "file_path\pca_scores.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
