********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 10_ipw.do
* Purpose : Estimate inverse probability weights (IPW) to correct for sample
*           selection into the EYFSP analytical sample. Weights are stabilised
*           and winsorised at the 1st and 99th percentiles.
*           A Mundlak adjustment accounts for school-level sorting into the
*           sample by including school means of pupil-level covariates.
*
* Inputs  : clean_data.dta
*           eyfsp_clean.dta    (sc_* subscale scores, score_eyfsp_overall)
*
* Output  : ipw.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOAD AND MERGE
********************************************************************************

use "file_path\clean_data.dta", clear

merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing( ///
	sc_attitude sc_social sc_emotion sc_language sc_link ///
	sc_read sc_write sc_numbers sc_calculating)
keep if _merge == 3
drop _merge

drop if score_eyfsp_overall == 0
drop if score_eyfsp_overall == .

* Reset missing_vars for pupils with EYFSP missingness flag
* (these are included in the selection model as the "not selected" group)
replace missing_vars = 0 if missing_EYFSP != 0
tab missing_vars

tab ybirth
tab mbirth

* Geography
gen school_KS2 = id_school_yr6
encode LA_school_code_yr6, gen(LA_KS2)


********************************************************************************
* SECTION 2 — KEEP VARIABLES NEEDED FOR SELECTION MODEL
********************************************************************************

keep id_dfe id_moj cohort ///
     ks1_score ks2_testmark* ks2_lev* ks2_points* ks2_score_ta ks2_level4 ///
     offending serious_violence violent_offence property_offence ///
     drugs_offence summary_offence num_offences ///
     female fsm nowhite lang_neng part_time ///
     nsiblings first_born ybirth mbirth imd_decile ///
     school_KS2 groupid_sib ///
     age_start_cin episodes_cin cpp num_cpp ///
     cpp_emotion cpp_neglect cpp_physical cpp_mult ///
     cla num_placement placement_length *cla ///
     *_exc sen* cin excluded ///
     missing_EYFSP sc_* score_eyfsp_overall ///
     share_absences num_schools num_sessions ///
     id_school_yr0 id_la_home id_la_school

********************************************************************************
* SECTION 3 — SEN AND CIN RECLASSIFICATION
*
* Consistent with 08_descriptives.do.
********************************************************************************

bys sen: tab sen_age
replace sen = 0 if sen_age < 4 & sen_age != .
gen sen_prior = (sen_age < 4 & sen_age != .)
tab sen_prior
tab sen
count if sen_plan != 0 & sen_prior == 1

bys cin: tab age_start_cin
replace cin = 0 if age_start_cin >= 11 & age_start_cin != .
replace cin = 0 if age_start_cin < 4  & age_start_cin != .
gen cin_prior = (age_start_cin < 4 & age_start_cin != .)
tab cin_prior
tab cin

tab excluded


********************************************************************************
* SECTION 4 — STANDARDISE KS2 SCORES
********************************************************************************

summ ks2_testmark_gps ks2_testmark_read ks2_testmark_math

foreach var of varlist ks1_score ks2_testmark_gps ks2_testmark_read ks2_testmark_math ks2_score_ta {
	summ `var'
	replace `var' = (`var' - r(mean)) / r(sd)
}

egen ks2_total = rowmean(ks2_testmark_gps ks2_testmark_read ks2_testmark_math)
egen ks2_score = std(ks2_total)
summ ks2_score


********************************************************************************
* SECTION 5 — ANALYSIS SAMPLE AND SELECTION INDICATOR
********************************************************************************

* Complete case restriction for selection model covariates
egen missing = rowmiss( ///
	offending ks2_score sen cin excluded ///
	share_absences num_schools num_sessions score_eyfsp_overall ///
	female nsiblings first_born nowhite fsm imd_decile ///
	ybirth mbirth lang_neng part_time ///
	sen_prior cin_prior id_school_yr0 ///
	sc_social sc_numbers sc_attitude sc_emotion sc_language ///
	sc_link sc_read sc_write sc_calculating id_la_home)
tab missing
keep if missing == 0

* Sample indicator: 1 = in EYFSP analytical sample, 0 = missing EYFSP
gen sample = (missing_EYFSP == 0)
tab sample


********************************************************************************
* SECTION 6 — SELECTION MODEL
********************************************************************************

global census ///
	1.female nsiblings 1.first_born 1.nowhite 1.fsm ///
	ib10.imd_decile i.ybirth i.mbirth 1.lang_neng ///
	1.sen_prior 1.cin_prior

*------------------------------------------------------------------------------
* 6.1  Baseline logit (no Mundlak adjustment)
*------------------------------------------------------------------------------
logit sample $census i.id_la_home, or vce(cluster id_school_yr0)
predict pw, pr

* School-level variation in sample membership
bys id_school_yr0: egen VarSchool  = sd(sample)
bys id_school_yr0: egen MeanSchool = mean(sample)
summ VarSchool MeanSchool
count if VarSchool  == 0
count if MeanSchool == 0
count if MeanSchool == 1
codebook id_school_yr0
codebook id_school_yr0 if MeanSchool == 0
codebook id_school_yr0 if MeanSchool == 1

*------------------------------------------------------------------------------
* 6.2  Mundlak-adjusted logit
*      School means of pupil-level covariates added to account for
*      non-random sorting of pupils into schools.
*------------------------------------------------------------------------------
foreach var of varlist female nsiblings first_born nowhite fsm ///
                       imd_decile ybirth mbirth lang_neng part_time ///
                       sen_prior cin_prior {
	bys id_school_yr0: egen mean_`var' = mean(`var')
}

summ sample
logit sample $census i.id_la_home mean_*, or vce(cluster id_school_yr0)
predict pw_school, pr

outreg2 using "file_path\table_ipw", ///
	alpha(0.01, 0.05, 0.10) bracket nocons excel dec(3) replace eform


********************************************************************************
* SECTION 7 — STABILISED IPW AND WINSORISATION
********************************************************************************

* Stabilised weights: numerator = school-level sample rate
bys id_school_yr0: egen mean_school = mean(sample)

gen ipw        = mean_school / pw        if sample == 1
gen ipw_school = 1 / pw_school           if sample == 1
* ipw uses a stabilised form (school-level sample rate / propensity score)
* to reduce weight variance in the baseline model, where school-level
* variation in sample membership is not otherwise accounted for.
* ipw_school uses the unstabilised form (1 / propensity score) because the
* Mundlak adjustment already absorbs school-level sorting via the school
* means of covariates, making explicit stabilisation redundant.

* Winsorise at 1st and 99th percentiles
winsor2 ipw        if sample == 1, cuts(1 99) replace
winsor2 ipw_school if sample == 1, cuts(1 99) replace

summ ipw ipw_school


********************************************************************************
* SECTION 8 — SAVE
********************************************************************************

keep if sample == 1
keep id_dfe ipw*

save "file_path\ipw.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
