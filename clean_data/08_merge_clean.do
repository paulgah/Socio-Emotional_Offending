********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 08_merge_clean.do
* Purpose : Merge all cleaned data sources into a single cross-sectional
*           analysis file; construct derived variables for offending,
*           exclusions, CIN/CLA status, and missing data indicators.
*
* Inputs  : eyfsp_clean.dta
*           crime_clean.dta
*           census_clean.dta
*           ks2_clean.dta
*           absences_clean.dta
*           exclusions_clean.dta
*           cin_clean.dta
*           cla_clean.dta
*
* Output  : clean_data.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — EYFSP BASE FILE
********************************************************************************

use "file_path\eyfsp_clean.dta", clear
keep id_dfe cohort missing_EYFSP score_eyfsp_overall


********************************************************************************
* SECTION 2 — PNC OFFENDING
********************************************************************************

merge 1:1 id_dfe using "file_path\crime_clean.dta"

* Non-matched EYFSP pupils have no criminal record — set offending to zero
replace offending = 0 if _merge == 1
tab offending
drop _merge

* Drop offenders outside the adolescent window (age < 10 or age >= 18)
count if offending == 1 & flag_offending_noteen == 1
drop  if offending == 1 & flag_offending_noteen == 1

tab serious_violence

*------------------------------------------------------------------------------
* 2.1  Winsorise number of offences at the 99th percentile
*------------------------------------------------------------------------------
tab num_offences
summ num_offences, d

xtile p99 = num_offences if num_offences != ., nq(100)
summ num_offences if p99 == 99

gen num_offences_cens = num_offences
replace num_offences_cens = 14 if p99 >= 99 & p99 != . & num_offences_cens >= 14
tab num_offences_cens
drop p99

*------------------------------------------------------------------------------
* 2.2  Winsorise number of serious offences at the 97th percentile
*------------------------------------------------------------------------------
tab num_serious_offences
summ num_serious_offences, d

xtile p97 = num_serious_offences if num_serious_offences != ., nq(100)
summ num_serious_offences if p97 == 97
replace num_serious_offences = 3 if p97 >= 98 & p97 != .
tab num_serious_offences
drop p97


********************************************************************************
* SECTION 3 — CENSUS
********************************************************************************

merge 1:1 id_dfe using "file_path\census_clean.dta", keepusing( ///
	id_dfe female ybirth mbirth nowhite lang_neng imd_decile ///
	sen_age sen_plan_age nsiblings first_born groupid_sib ///
	fsm part_time sen sen_plan ///
	sen_besd sen_ld sen_pd sen_other ///
	sen_besd2 sen_ld2 sen_pd2 sen_other2 ///
	num_schools id_school_yr* LA_home_code_yr* LA_school_code_yr* ///
	id_la_home id_la_school)
drop _merge

*------------------------------------------------------------------------------
* 3.1  Year of birth — set implausible values to missing
*------------------------------------------------------------------------------
tab ybirth
bys cohort: tab ybirth
replace ybirth = . if ybirth <= 2000 | ybirth >= 2005

*------------------------------------------------------------------------------
* 3.2  SEN type indicators — set to 0 within SEN pupils where type is missing
*      (implies SEN provision recorded but type unclassified)
*------------------------------------------------------------------------------
tab sen
tab sen_plan

foreach var of varlist sen_besd sen_ld sen_pd sen_other sen_besd2 sen_ld2 sen_pd2 sen_other2 {
	replace `var' = 0 if sen == 1 & `var' == .
	tab `var'
}

* Geography at KS2
codebook LA_home_code_yr6 LA_school_code_yr6 id_school_yr6


********************************************************************************
* SECTION 4 — KS2
********************************************************************************

merge 1:1 id_dfe using "file_path\ks2_clean.dta", keepusing( ///
	mschool ks1_score ks2_score_ta ///
	ks2_read4 ks2_maths4 ks2_gps4 ks2_level4 ks2_acadyr ///
	ks2_testmark* ks2_lev* ks2_points*)
drop _merge

mdesc mschool ks1_score ks2_score_ta ks2_read4 ks2_maths4 ks2_gps4 ///
      ks2_level4 ks2_acadyr ks2_testmark* ks2_lev* ks2_points*

gen nomschool = (mschool == 0)
tab nomschool


********************************************************************************
* SECTION 5 — ABSENCES
********************************************************************************

merge 1:1 id_dfe using "file_path\absences_clean.dta", keepusing( ///
	share_absences share_uabsences absences_exc ///
	num_absences num_uabsences num_sessions)
drop _merge

mdesc share_absences share_uabsences absences_exc
tab absences_exc
count if share_absences == 0
count if share_absences == 1


********************************************************************************
* SECTION 6 — EXCLUSIONS
********************************************************************************

merge 1:1 id_dfe using "file_path\exclusions_clean.dta", keepusing( ///
	num_fexc sessions_fexc perm_exc ///
	physical_assault_exc verbal_abuse_exc misbehaviour_exc other_reason_exc)
drop _merge

mdesc num_fexc sessions_fexc perm_exc ///
      physical_assault_exc verbal_abuse_exc misbehaviour_exc other_reason_exc

summ share_absences share_uabsences num_fexc nomschool sen

*------------------------------------------------------------------------------
* 6.1  Ever-excluded indicator
*------------------------------------------------------------------------------
count if physical_assault_exc == 1 & num_fexc == 0
count if verbal_abuse_exc     == 1 & num_fexc == 0
count if misbehaviour_exc     == 1 & num_fexc == 0
count if other_reason_exc     == 1 & num_fexc == 0

gen excluded = 0
replace excluded = 1 if num_fexc != 0 | perm_exc == 1 | sessions_fexc != 0
replace excluded = 1 if (physical_assault_exc == 1 | verbal_abuse_exc == 1 ///
                       | misbehaviour_exc == 1 | other_reason_exc == 1) ///
                       & excluded != 1
tab excluded

* Reason indicators are only meaningful for excluded pupils
replace physical_assault_exc = . if excluded == 0
replace verbal_abuse_exc     = . if excluded == 0
replace misbehaviour_exc     = . if excluded == 0
replace other_reason_exc     = . if excluded == 0


********************************************************************************
* SECTION 7 — CIN AND CLA
********************************************************************************

merge 1:1 id_dfe using "file_path\cin_clean.dta", keepusing( ///
	cin previous_cpp episodes_cin ///
	cpp_emotion cpp_neglect cpp_physical cpp_mult age_start_cin)
drop _merge

mdesc cin previous_cpp episodes_cin cpp_emotion cpp_neglect cpp_physical cpp_mult age_start_cin

merge 1:1 id_dfe using "file_path\cla_clean.dta", keepusing( ///
	cla placement_changes placement_length ///
	abuse_cla fam_prob_cla soc_misbehave_cla other_prob_cla)
drop _merge

mdesc cla placement_changes placement_length ///
      abuse_cla fam_prob_cla soc_misbehave_cla other_prob_cla

*------------------------------------------------------------------------------
* 7.1  CIN/CLA overlap — any CLA child is also CIN
*------------------------------------------------------------------------------
count if cin == 1 & cla == 1
count if cin == 0 & cla == 1
count if cin == 1 & cla == 0
replace cin = 1 if cla == 1

*------------------------------------------------------------------------------
* 7.2  Child Protection Plan indicator and count
*------------------------------------------------------------------------------
tab cin

gen cpp = 0 if cin == 1
replace cpp = 1 if previous_cpp != .
replace cpp = 1 if cpp == . & (cpp_emotion == 1 | cpp_neglect == 1 | cpp_physical == 1 | cpp_mult == 1)
tab cpp

gen num_cpp = 0 if cin == 1
replace num_cpp = previous_cpp + 1 if cpp == 1
tab num_cpp

*------------------------------------------------------------------------------
* 7.3  CPP abuse category indicators
*      Within CIN: missing implies no CPP (set to 0).
*      Within CPP: missing implies category not recorded (retain as missing).
*------------------------------------------------------------------------------
foreach stub in cpp_emotion cpp_neglect cpp_physical cpp_mult {
	replace `stub' = 0 if cin == 1 & `stub' == .
	replace `stub' = . if cpp == 1 & `stub' == .
	tab `stub'
}

tab episodes_cin
tab age_start_cin

*------------------------------------------------------------------------------
* 7.4  CLA variables — zero-fill for CIN pupils who were never CLA
*------------------------------------------------------------------------------
tab cla
replace cla = . if cin == 0

gen num_placement = 0 if cin == 1
replace num_placement = placement_changes + 1 if cla == 1
tab num_placement

* Zero-fill for CIN non-CLA only.
foreach stub in placement_length abuse_cla fam_prob_cla soc_misbehave_cla other_prob_cla {
	replace `stub' = 0 if cin == 1 & cla == 0
	tab `stub'
}


********************************************************************************
* SECTION 8 — MISSING DATA INDICATOR
********************************************************************************

egen missing_vars = rowmiss( ///
	id_dfe share_absences sessions_fexc num_schools perm_exc ///
	ks2_level4 ks2_testmark_gps ks2_testmark_math ks2_testmark_read ///
	offending cin sen female fsm nowhite lang_neng part_time ///
	nsiblings first_born ybirth mbirth imd_decile ///
	LA_school_code_yr6 id_school_yr6)

replace missing_vars = 1 if missing_EYFSP != 0
tab missing_vars


********************************************************************************
* SECTION 9 — SAVE
********************************************************************************

mdesc
compress

save "file_path\clean_data.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
