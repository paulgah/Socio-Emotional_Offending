********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 01_descriptives.do
* Purpose : Load analysis sample; merge factor scores; construct derived
*           variables; produce summary statistics tables for the full sample
*           and vulnerable subgroups.
*
* Inputs  : clean_data.dta
*           fs_esem.dta        (ESEM factor scores)
*           fs_bifactor.dta    (bifactor scores)
*           pca_scores.dta     (PCA scores)
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOAD SAMPLE AND MERGE FACTOR SCORES
********************************************************************************

use "file_path\clean_data.dta", clear
keep if missing_vars == 0
drop if score_eyfsp_overall == 0

merge 1:1 id_dfe using "file_path\fs_esem.dta"
drop _merge

merge 1:1 id_dfe using "file_path\fs_bifactor.dta"
drop _merge
rename (gend_R cogn_R semo_R) (gend_bfs cogn_bfs semo_bfs)

merge 1:1 id_dfe using "file_path\pca_scores.dta"
drop _merge


********************************************************************************
* SECTION 2 — SEN AND CIN RECLASSIFICATION
*
* Pupils with SEN or CIN onset before school age (age <= 4) are reclassified
* as prior cases and excluded from the school-age SEN/CIN indicators.
* Pupils with CIN onset at age >= 11 are reclassified as post-KS2.
********************************************************************************

* SEN
bys sen: tab sen_age
replace sen = 0 if sen_age < 4 & sen_age != .
gen sen_prior = (sen_age < 4 & sen_age != .)
tab sen_prior
tab sen
count if sen_plan != 0 & sen_prior == 1

* CIN
bys cin: tab age_start_cin
replace cin = 0 if age_start_cin >= 11 & age_start_cin != .
replace cin = 0 if age_start_cin < 4  & age_start_cin != .
gen cin_prior = (age_start_cin < 4 & age_start_cin != .)
tab cin_prior
tab cin


********************************************************************************
* SECTION 3 — EXCLUSIONS CHECK
********************************************************************************

tab excluded

********************************************************************************
* SECTION 4 — STANDARDISE AND REVERSE-SCORE FACTOR SCORES
*
* All factor scores are standardised to mean 0, SD 1, then sign-reversed so
* that higher values indicate better outcomes (lower risk).
********************************************************************************

foreach var of varlist *fs *esem {
	summ `var'
	replace `var' = (`var' - r(mean)) / r(sd)
	replace `var' = -`var'
}
summ *fs


********************************************************************************
* SECTION 5 — KS2 SCORES
*
* Standardise test marks and composite scores.
* Construct composite KS2 score as rowmean of GPS, reading, and maths,
* then standardise.
********************************************************************************

summ ks2_testmark_gps ks2_testmark_read ks2_testmark_math

foreach var of varlist *diff school_diff_alt ks1_score ///
                       ks2_testmark_gps ks2_testmark_read ks2_testmark_math ks2_score_ta {
	summ `var'
	replace `var' = (`var' - r(mean)) / r(sd)
}

egen ks2_total       = rowmean(ks2_testmark_gps ks2_testmark_read ks2_testmark_math)
egen ks2_score       = std(ks2_total)
summ ks2_score

egen ks2_total_points = rowmean(ks2_points_gps ks2_points_math ks2_points_read)
egen ks2_score_points = std(ks2_total_points)
summ ks2_score_points
kdensity ks2_score_points, bw(0.4)

tab ks2_level4
bys ks2_level4: summ ks2_score


********************************************************************************
* SECTION 6 — YEAR OF BIRTH RESTRICTIONS AND GEOGRAPHY
********************************************************************************

tab ybirth
replace ybirth = . if inlist(ybirth, 2000, 2005, 2008)
drop if ybirth == .

tab mbirth

* School and LA identifiers at KS2
gen school_KS2 = id_school_yr6
encode LA_school_code_yr6, gen(LA_KS2)


********************************************************************************
* SECTION 7 — TABLE: SAMPLE STATISTICS BY OFFENDING STATUS
********************************************************************************

summ offending if offending == 1
summ offending if offending == 0

foreach stub in sen cin cla excluded {
	tab `stub' if serious_violence == 1
	tab `stub' if offending == 1
	tab `stub' if offending == 0
}


********************************************************************************
* SECTION 8 — TABLE: SUMMARY STATISTICS MAIN VARIABLES
********************************************************************************

*------------------------------------------------------------------------------
* 8.1  Demographic characteristics
*------------------------------------------------------------------------------
foreach var in female nowhite lang_neng {
	summ `var' if offending == 1
	summ `var' if offending == 0
	prtest `var', by(offending)
	tab `var' offending, chi2 row
}

*------------------------------------------------------------------------------
* 8.2  SES characteristics
*------------------------------------------------------------------------------

* FSM
summ fsm if offending == 1
summ fsm if offending == 0
prtest fsm, by(offending)
tab fsm offending, chi2 row

* CIN
summ cin if offending == 1
summ cin if offending == 0
prtest cin, by(offending)
tab cin offending, chi2 row

* IMD deprivation decile
summ imd_decile if offending == 1
summ imd_decile if offending == 0
ttest imd_decile, by(offending)
tab imd_decile offending, chi2 row
bys imd_decile: tab offending

* Number of siblings
summ nsiblings if offending == 1
summ nsiblings if offending == 0
ttest nsiblings, by(offending)
tab nsiblings offending, chi2 row
_pctile nsiblings if offending == 0, p(97)
display r(r1)
_pctile nsiblings if offending == 1, p(97)
display r(r1)
bys nsiblings: tab offending

* First born
summ first_born if offending == 1
summ first_born if offending == 0
prtest first_born, by(offending)
tab first_born offending, chi2 row

*------------------------------------------------------------------------------
* 8.3  School characteristics
*------------------------------------------------------------------------------

* KS2 score
gen ks2_score_rounded = round(ks2_score, 0.1)
summ ks2_score_rounded if offending == 1
summ ks2_score_rounded if offending == 0
ttest ks2_score_rounded, by(offending)
_pctile ks2_score_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile ks2_score_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab ks2_score_rounded if offending == 1
tab ks2_score_rounded if offending == 0

gen ks2_score_rounded_string = string(ks2_score_rounded)
count if ks2_score_rounded_string == "-3.7" & offending == 1
count if ks2_score_rounded_string ==  "1.6" & offending == 1
count if ks2_score_rounded_string == "-3.7" & offending == 0
count if ks2_score_rounded_string ==  "1.6" & offending == 0

* KS2 level 4
summ ks2_level4 if offending == 1
summ ks2_level4 if offending == 0
prtest ks2_level4, by(offending)
tab ks2_level4 offending, chi2 row

* Persistent absence (>= 15% of sessions)
gen persistent_absent = (share_absences >= 0.15)
tab persistent_absent
summ persistent_absent if offending == 1
summ persistent_absent if offending == 0
prtest persistent_absent, by(offending)
tab persistent_absent offending, chi2 row

* Ever excluded
summ excluded if offending == 1
summ excluded if offending == 0
prtest excluded, by(offending)
tab excluded offending, chi2 row

* SEN
summ sen if offending == 1
summ sen if offending == 0
prtest sen, by(offending)
tab sen offending, chi2 row

* Number of school changes
gen change_school = (num_schools > 1)
tab change_school
summ num_schools if offending == 1
summ num_schools if offending == 0
ttest num_schools, by(offending)
tab num_schools offending, chi2 row
_pctile num_schools if offending == 0, p(97)
display r(r1)
_pctile num_schools if offending == 1, p(97)
display r(r1)

* Part-time enrolment
summ part_time if offending == 1
summ part_time if offending == 0
prtest part_time, by(offending)
tab part_time offending, chi2 row

* School difficulties score
gen school_diff_rounded = round(school_diff_alt, 0.1)
summ school_diff_alt if offending == 1
summ school_diff_alt if offending == 0
ttest school_diff_rounded, by(offending)
_pctile school_diff_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile school_diff_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab school_diff_rounded if offending == 1
tab school_diff_rounded if offending == 0

* Care difficulties score
summ care_diff if offending == 1
summ care_diff if offending == 0
ttest care_diff, by(offending)

*------------------------------------------------------------------------------
* 8.4  EYFSP factor scores
*------------------------------------------------------------------------------
summ *fs

* Cognitive (ESEM)
gen cogn_esem_rounded = round(cogn_esem, 0.2)
summ cogn_esem if offending == 1
summ cogn_esem if offending == 0
ttest cogn_esem_rounded, by(offending)
_pctile cogn_esem_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile cogn_esem_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab cogn_esem_rounded if offending == 1
tab cogn_esem_rounded if offending == 0

* Socio-emotional (ESEM)
gen semo_esem_rounded = round(semo_esem, 0.1)
summ semo_esem if offending == 1
summ semo_esem if offending == 0
ttest semo_esem_rounded, by(offending)
_pctile semo_esem_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile semo_esem_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab semo_esem_rounded if offending == 1
tab semo_esem_rounded if offending == 0

* General factor (bifactor)
gen gend_bfs_rounded = round(gend_bfs, 0.1)
summ gend_bfs_rounded if offending == 1
summ gend_bfs_rounded if offending == 0
ttest gend_bfs_rounded, by(offending)
_pctile gend_bfs_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile gend_bfs_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab gend_bfs_rounded if offending == 1
tab gend_bfs_rounded if offending == 0

* Cognitive (bifactor)
gen cogn_bfs_rounded = round(cogn_bfs, 0.1)
summ cogn_bfs_rounded if offending == 1
summ cogn_bfs_rounded if offending == 0
ttest cogn_bfs_rounded, by(offending)
_pctile cogn_bfs_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile cogn_bfs_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab cogn_bfs_rounded if offending == 1
tab cogn_bfs_rounded if offending == 0

* Socio-emotional (bifactor)
gen semo_bfs_rounded = round(semo_bfs, 0.1)
summ semo_bfs_rounded if offending == 1
summ semo_bfs_rounded if offending == 0
ttest semo_bfs_rounded, by(offending)
_pctile semo_bfs_rounded if offending == 0, p(3 97)
display r(r1)
display r(r2)
_pctile semo_bfs_rounded if offending == 1, p(3 97)
display r(r1)
display r(r2)
tab semo_bfs_rounded if offending == 1
tab semo_bfs_rounded if offending == 0

* Correlation table
corr offending ks2_score school_diff_alt excluded cin sen *esem *bfs


********************************************************************************
* SECTION 9 — TABLE: SUMMARY STATISTICS VULNERABLE SUBGROUPS
********************************************************************************

*------------------------------------------------------------------------------
* 9.1  SEN subgroup
*------------------------------------------------------------------------------
foreach stub in sen_besd sen_ld sen_pd sen_other {
	summ `stub' if serious_violence == 1 & sen == 1
	summ `stub' if offending == 1        & sen == 1
	summ `stub' if offending == 0        & sen == 1
}

*------------------------------------------------------------------------------
* 9.2  CIN/CLA subgroup
*------------------------------------------------------------------------------
gen cin_noinfo = (age_start_cin == .) if cin == 1
tab cin_noinfo

foreach cond in "serious_violence == 1 & cin == 1" "offending == 1 & cin == 1" "offending == 0 & cin == 1" {
	summ cin_noinfo   if `cond'
	summ age_start_cin if `cond'
	summ episodes_cin  if `cond'
	summ cpp           if `cond'
}

foreach cond in "serious_violence == 1 & cpp == 1" "offending == 1 & cpp == 1" "offending == 0 & cpp == 1" {
	summ num_cpp      if `cond'
	summ cpp_emotion  if `cond'
	summ cpp_neglect  if `cond'
	summ cpp_physical if `cond'
	summ cpp_mult     if `cond'
}

*------------------------------------------------------------------------------
* 9.3  CLA subgroup
*------------------------------------------------------------------------------
foreach cond in "serious_violence == 1 & cla == 1" "offending == 1 & cla == 1" "offending == 0 & cla == 1" {
	summ num_placement      if `cond'
	summ placement_length   if `cond'
	summ abuse_cla          if `cond'
	summ fam_prob_cla       if `cond'
	summ soc_misbehave_cla  if `cond'
	summ other_prob_cla     if `cond'
}

*------------------------------------------------------------------------------
* 9.4  Exclusions subgroup
*------------------------------------------------------------------------------
foreach cond in "serious_violence == 1 & excluded == 1" "offending == 1 & excluded == 1" "offending == 0 & excluded == 1" {
	summ num_fexc             if `cond'
	summ perm_exc             if `cond'
	summ physical_assault_exc if `cond'
	summ verbal_abuse_exc     if `cond'
	summ misbehaviour_exc     if `cond'
	summ other_reason_exc     if `cond'
}

********************************************************************************
* END OF FILE
********************************************************************************
