********************************************************************************
* Project : Socio-Emotional Characteristics in Early Childhood and
*           Offending Behaviour in Adolescence
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 01_census_clean.do
* Purpose : Extract and clean pupil-level demographic, SES, school, and SEN
*           variables from the Spring School Census (2007–2020); merge with the
*           EYFSP analytical sample; collapse to one observation per pupil;
*           construct cohort-aligned time-varying school/LSOA indicators.
*
* Inputs  : Spring_Census_20YY.csv  (YY = 07 … 20)
*           eyfsp_clean.dta
*
* Output  : census_clean.dta
*
* Notes   : Intermediate per-year .dta files are created and deleted at the end.
*           The merge step retains only children present in eyfsp_clean.dta
*           (_merge == 3).  All file paths use the placeholder "file_path\"
*           and should be updated to absolute paths before running.
********************************************************************************

clear all
set more off

********************************************************************************
* SECTION 1 — LOOP OVER ANNUAL CENSUS FILES (Spring 2007–2020)
*             Extract, rename, and clean one file per iteration; save to disk.
********************************************************************************

foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {

	import delimited "file_path\Spring_Census_20`yr'.csv", clear

	*--------------------------------------------------------------------------
	* 1.1  Pupil identifier
	*--------------------------------------------------------------------------
	rename pupilmatchingrefanonymous_spr`yr' id_dfe

	*--------------------------------------------------------------------------
	* 1.2  Restrict to children with EYFSP data
	*      Merge on id_dfe; keep only matched observations (_merge == 3)
	*--------------------------------------------------------------------------
	* Confirm uniqueness before merge
	bys id_dfe: gen n = _n
	tab n
	drop n

	merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(id_dfe cohort)
	keep if _merge == 3
	drop _merge

	*--------------------------------------------------------------------------
	* 1.3  Local Authority codes
	*      Available from 2011 onwards (la_9code prefix); captured conditionally
	*--------------------------------------------------------------------------
	capture confirm variable la_9code_spr`yr'
	if _rc == 0 {
		rename la_9code_spr`yr'      LA_school_code`yr'
		rename homela_9code_spr`yr'  LA_home_code`yr'
	}

	*--------------------------------------------------------------------------
	* 1.4  Demographics (all years: 2007–2020)
	*--------------------------------------------------------------------------

	* Gender — recode string "F"/"M" to binary; missing if blank
	gen female`yr' = (gender_spr`yr' == "F")
	replace female`yr' = . if gender_spr`yr' == ""
	label var female`yr' "Whether pupil is female"

	* Age at start of academic year
	rename ageatstartofacademicyear_spr`yr' age`yr'
	label var age`yr' "Age at start of academic year"

	* Year of birth
	rename yearofbirth_spr`yr' ybirth`yr'
	label var ybirth`yr' "Year of birth"

	* Month of birth
	rename monthofbirth_spr`yr' mbirth`yr'
	label var mbirth`yr' "Month of birth"

	* Ethnicity — indicator for non-White; missing if unclassified or blank
	gen nowhite`yr' = (ethnicgroupmajor_spr`yr' != "WHIT")
	replace nowhite`yr' = . if inlist(ethnicgroupmajor_spr`yr', "UNCL", "")
	label var nowhite`yr' "Whether pupil is non-White"

	* Home language — indicator for non-English; missing if unclassified/blank
	gen lang_neng`yr' = (languagegroupmajor_spr`yr' != "1_ENG")
	replace lang_neng`yr' = . if inlist(languagegroupmajor_spr`yr', "3_UNCL", "")
	label var lang_neng`yr' "Whether pupil's first language is not English"

	*--------------------------------------------------------------------------
	* 1.5  Family SES
	*--------------------------------------------------------------------------

	* Free School Meal eligibility
	rename fsmeligible_spr`yr' fsm`yr'
	label var fsm`yr' "Whether pupil is FSM eligible"

	* IDACI deprivation decile (1 = most deprived, 10 = least deprived)
	* Variable name changed across census waves; capture both variants
	capture confirm variable idacidecile_spr`yr'
	if _rc == 0 {
		rename idacidecile_spr`yr' imd_decile`yr'
		label var imd_decile`yr' "IDACI deprivation decile (1=most deprived)"
	}
	capture confirm variable idacidecile_15_spr`yr'
	if _rc == 0 {
		rename idacidecile_15_spr`yr' imd_decile`yr'
		label var imd_decile`yr' "IDACI deprivation decile (1=most deprived)"
	}

	* Siblings and birth order — available only in waves 2008, 2013, 2016
	if inlist("`yr'", "08", "13", "16") {
		rename nsiblings_sgas_spr`yr' nsiblings`yr'

		gen first_born`yr' = (birthorder_sgas_spr`yr' == 1)
		replace first_born`yr' = . if birthorder_sgas_spr`yr' == .

		rename groupid_sgas_spr`yr' groupid_sib`yr'
	}

	*--------------------------------------------------------------------------
	* 1.6  School characteristics
	*--------------------------------------------------------------------------

	* Part-time enrolment
	rename parttime_spr`yr' part_time`yr'
	label var part_time`yr' "Whether pupil is enrolled part-time"

	* SEN provision — any provision vs none
	gen sen`yr' = (senprovision_spr`yr' != "N")
	replace sen`yr' = . if senprovision_spr`yr' == ""
	label var sen`yr' "Whether pupil has any SEN provision"

	* SEN statement / EHC plan
	* Variable format differs pre/post 2009
	if inlist("`yr'", "07", "08") {
		gen sen_plan`yr' = (senprovision_spr`yr' == "S")
	}
	else {
		gen sen_plan`yr' = (senprovisionmajor_spr`yr' == "3_SS")
		replace sen_plan`yr' = . if inlist(senprovisionmajor_spr`yr', "", "4_UNCL")
	}
	label var sen_plan`yr' "Whether pupil has SEN statement or EHC plan"

	* SEN type indicators (primary and secondary need)
	* Note: BESD (pre-2015) and SEMH (post-2015) refer to the same broad category

	* Behavioural, Emotional and Social Difficulties / SEMH
	gen sen_besd`yr'  = 0 if primarysentype_spr`yr'   != ""
	gen sen_besd2`yr' = 0 if secondarysentype_spr`yr' != ""
	replace sen_besd`yr'  = 1 if inlist(primarysentype_spr`yr',   "BESD", "SEMH")
	replace sen_besd2`yr' = 1 if inlist(secondarysentype_spr`yr', "BESD", "SEMH")
	label var sen_besd`yr'  "Primary need: behavioural, emotional or social difficulties"
	label var sen_besd2`yr' "Secondary need: behavioural, emotional or social difficulties"

	* Learning Difficulties (SpLD, MLD, SLD, PMLD)
	gen sen_ld`yr'  = 0 if primarysentype_spr`yr'   != ""
	gen sen_ld2`yr' = 0 if secondarysentype_spr`yr' != ""
	foreach stub in "" "2" {
		local src = cond("`stub'" == "", "primarysentype_spr`yr'", "secondarysentype_spr`yr'")
		replace sen_ld`stub'`yr' = 1 if inlist(`src', "SPLD", "MLD", "SLD", "PMLD")
	}
	label var sen_ld`yr'  "Primary need: learning difficulties"
	label var sen_ld2`yr' "Secondary need: learning difficulties"

	* Physical / Sensory Disabilities (SLCN, HI, VI, MSI, PD)
	gen sen_pd`yr'  = 0 if primarysentype_spr`yr'   != ""
	gen sen_pd2`yr' = 0 if secondarysentype_spr`yr' != ""
	foreach stub in "" "2" {
		local src = cond("`stub'" == "", "primarysentype_spr`yr'", "secondarysentype_spr`yr'")
		replace sen_pd`stub'`yr' = 1 if inlist(`src', "SLCN", "HI", "VI", "MSI", "PD")
	}
	label var sen_pd`yr'  "Primary need: physical or sensory disability"
	label var sen_pd2`yr' "Secondary need: physical or sensory disability"

	* Other (ASD, OTH, NSA)
	gen sen_other`yr'  = 0 if primarysentype_spr`yr'   != ""
	gen sen_other2`yr' = 0 if secondarysentype_spr`yr' != ""
	foreach stub in "" "2" {
		local src = cond("`stub'" == "", "primarysentype_spr`yr'", "secondarysentype_spr`yr'")
		replace sen_other`stub'`yr' = 1 if inlist(`src', "ASD", "OTH", "NSA")
	}
	label var sen_other`yr'  "Primary need: other difficulty (ASD/OTH/NSA)"
	label var sen_other2`yr' "Secondary need: other difficulty (ASD/OTH/NSA)"

	*--------------------------------------------------------------------------
	* 1.7  Geographic identifiers
	*--------------------------------------------------------------------------

	rename la_spr`yr'     id_la_school`yr'
	rename homela_spr`yr' id_la_home`yr'
	rename urn_spr`yr'    id_school`yr'

	* LSOA code — variable name changed from llsoa to lsoa11 from 2013 onward
	gen lsoa_code`yr' = ""
	if inlist("`yr'", "07", "08", "09", "10", "11", "12") {
		replace lsoa_code`yr' = llsoa_spr`yr'
	}
	else {
		replace lsoa_code`yr' = lsoa11_spr`yr'
	}

	*--------------------------------------------------------------------------
	* 1.8  Keep only variables that exist in this wave, then save
	*--------------------------------------------------------------------------
	local varlist ///
		id_dfe age`yr' female`yr' fsm`yr' ybirth`yr' mbirth`yr' ///
		nowhite`yr' lang_neng`yr' imd_decile`yr' part_time`yr' ///
		sen`yr' sen_plan`yr' sen_besd`yr' sen_ld`yr' sen_pd`yr' sen_other`yr' ///
		sen_besd2`yr' sen_ld2`yr' sen_pd2`yr' sen_other2`yr' ///
		LA_home_code`yr' LA_school_code`yr' ///
		id_la_school`yr' id_la_home`yr' id_school`yr' ///
		nsiblings`yr' first_born`yr' groupid_sib`yr' lsoa_code`yr'

	local keeplist
	foreach var of local varlist {
		capture confirm variable `var'
		if !_rc local keeplist `keeplist' `var'
	}

	keep `keeplist'
	mdesc
	compress
	save "file_path\census_20`yr'.dta", replace

} // end year loop


********************************************************************************
* SECTION 2 — MERGE ANNUAL FILES INTO A SINGLE WIDE PANEL
*             Start from eyfsp_clean.dta and merge in each census wave.
********************************************************************************

use "file_path\eyfsp_clean.dta", clear
keep id_dfe cohort age_EYFSP

* Merge one year at a time; drop non-EYFSP observations (_merge == 2)
foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {
	merge 1:1 id_dfe using "file_path\census_20`yr'.dta"
	drop if _merge == 2
	drop _merge
}


********************************************************************************
* SECTION 3 — SCHOOL MOBILITY (number of school transitions, by cohort)
*
* Logic: dschoolN = 1 if school changed between consecutive census years.
* Each cohort spans 5 years of primary school; the relevant year range shifts
* by one census wave per cohort.
* Cohort 1: EYFSP in Spring 2007 (Year R), KS2 in 2013
* Cohort 2: EYFSP in Spring 2008, KS2 in 2014
* Cohort 3: EYFSP in Spring 2009, KS2 in 2015
********************************************************************************

gen dschool1 = (id_school08 != id_school09) if cohort == 1 & !missing(id_school07, id_school08)
gen dschool2 = (id_school09 != id_school10) if inlist(cohort, 1, 2) & !missing(id_school08, id_school09)
gen dschool3 = (id_school10 != id_school11) if !missing(id_school09, id_school10)
gen dschool4 = (id_school11 != id_school12) if !missing(id_school10, id_school11)
gen dschool5 = (id_school12 != id_school13) if !missing(id_school11, id_school12)
gen dschool6 = (id_school13 != id_school14) if inlist(cohort, 2, 3) & !missing(id_school12, id_school13)
gen dschool7 = (id_school14 != id_school15) if cohort == 3 & !missing(id_school13, id_school14)

* Count transitions over Reception–KS2 for each cohort
foreach c in 1 2 3 {
	local startd = `c'
	local endd   = `c' + 4
	if `c' == 1 local vars dschool1 dschool2 dschool3 dschool4 dschool5
	if `c' == 2 local vars dschool2 dschool3 dschool4 dschool5 dschool6
	if `c' == 3 local vars dschool3 dschool4 dschool5 dschool6 dschool7
	egen num_schools`c' = rowtotal(`vars') if cohort == `c', missing
}

gen num_schools = .
forvalues c = 1/3 {
	replace num_schools = num_schools`c' if cohort == `c'
}
drop num_schools1 num_schools2 num_schools3
tab num_schools


********************************************************************************
* SECTION 4 — COLLAPSE WIDE PANEL TO ONE OBSERVATION PER PUPIL
*
* For time-invariant characteristics: use value from the cohort's reception year,
* filling forward across later years for missing values.
* For time-varying characteristics: take the maximum across all relevant years
* (flags ever-true across Reception–KS2).
* Special handling: siblings (latest available count); birth order / group ID.
********************************************************************************

* Convert home-LA variables to numeric (mixed string/numeric across waves)
destring id_la_home*, replace force

*--------------------------------------------------------------------------
* 4.1  Initialise running variables
*--------------------------------------------------------------------------
gen sen_age      = .   // Age at first SEN registration
gen sen_plan_age = .   // Age at first SEN statement / EHC plan
gen nsiblings    = .
gen first_born   = .
gen groupid_sib  = .

*--------------------------------------------------------------------------
* 4.2  Time-invariant characteristics at reception year; fill forward for missing
*--------------------------------------------------------------------------
foreach var in female ybirth mbirth nowhite lang_neng imd_decile ///
               id_la_home id_la_school id_school {

	gen `var' = .

	* Assign from reception year
	replace `var' = `var'07 if cohort == 1
	replace `var' = `var'08 if cohort == 2
	replace `var' = `var'09 if cohort == 3

	* Forward-fill from any subsequent year if still missing
	foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {

		replace `var' = `var'`yr' if `var' == .

		* Age at first SEN entry (uses the year loop for convenience)
		replace sen_age      = age`yr' if sen_age == .      & sen`yr' == 1 & !missing(age`yr')
		replace sen_plan_age = age`yr' if sen_plan_age == . & sen`yr' == 1 & sen_plan`yr' == 1 & !missing(age`yr')

		* Siblings: take the maximum count observed across available waves
		* (waves 2008 and 2013 correspond to cohort 1's reception and KS2 years)
		if inlist("`yr'", "08", "13") {
			replace nsiblings = nsiblings`yr' ///
				if (!missing(nsiblings`yr')) & (missing(nsiblings) | nsiblings < nsiblings`yr')
			replace first_born = first_born`yr' if missing(first_born) & !missing(first_born`yr')
		}

		* Sibling group ID: take from 2016 wave only
		* NOTE: group identifier is not stable across census waves, so a single
		*       wave is used.  Wave 2016 is the last year siblings data are
		*       available and corresponds to cohort 3's KS2 year.
		if "`yr'" == "16" {
			replace nsiblings   = nsiblings16   if missing(nsiblings) & !missing(nsiblings16)
			replace groupid_sib = groupid_sib16
		}

	} // end inner year loop

} // end variable loop

*--------------------------------------------------------------------------
* 4.3  Ever-true indicators across Reception–KS2 (rowmax)
*--------------------------------------------------------------------------
foreach var in fsm part_time sen sen_plan ///
               sen_besd sen_ld sen_pd sen_other ///
               sen_besd2 sen_ld2 sen_pd2 sen_other2 {

	foreach c in 1 2 3 {
		* Cohort 1: Spring 2007/08 (Reception) → Spring 2013/14 (KS2)
		if `c' == 1 local yrlist 07 08 09 10 11 12 13
		if `c' == 2 local yrlist 08 09 10 11 12 13 14
		if `c' == 3 local yrlist 09 10 11 12 13 14 15

		local vlist
		foreach yr of local yrlist {
			local vlist `vlist' `var'`yr'
		}
		egen `var'_`c' = rowmax(`vlist') if cohort == `c'
	}

	gen `var' = .
	forvalues c = 1/3 {
		replace `var' = `var'_`c' if cohort == `c'
	}
	drop `var'_1 `var'_2 `var'_3
	tab `var'

}


********************************************************************************
* SECTION 5 — CONSTRUCT COHORT-ALIGNED TIME-VARYING INDICATORS
*
* Map calendar-year census waves onto a common "school year" index (yr0–yr11)
* that runs from Reception (yr0) to the end of the sample window.
* The calendar year differs by cohort, so each cohort's observations are
* assigned from the appropriate census wave.
*
* School year index:
*   yr0  = Reception
*   yr1  = Year 1
*   ...
*   yr6  = Year 6 (KS2)
*   yr7–yr11 = secondary school years (where available)
*
* Calendar year mapping:
*   Cohort 1 starts in Spring 2007 (school yr0 → census 07, yr1 → 08, …)
*   Cohort 2 starts in Spring 2008 (school yr0 → census 08, yr1 → 09, …)
*   Cohort 3 starts in Spring 2009 (school yr0 → census 09, yr1 → 10, …)
********************************************************************************

* Rename two-digit suffixed variables to numeric for the loop below
rename (id_school07 id_school08 id_school09) (id_school7 id_school8 id_school9)
rename (lsoa_code07 lsoa_code08 lsoa_code09) (lsoa_code7 lsoa_code8 lsoa_code9)

* School ID and LSOA code (yr0–yr11)
local i = 7
forvalues num = 0/11 {

	gen id_school_yr`num'  = .
	gen lsoa_code_yr`num'  = ""

	local j = `i' + 1
	local k = `j' + 1

	replace id_school_yr`num' = id_school`i' if cohort == 1
	replace id_school_yr`num' = id_school`j' if cohort == 2
	replace id_school_yr`num' = id_school`k' if cohort == 3

	replace lsoa_code_yr`num' = lsoa_code`i' if cohort == 1
	replace lsoa_code_yr`num' = lsoa_code`j' if cohort == 2
	replace lsoa_code_yr`num' = lsoa_code`k' if cohort == 3

	local ++i
}

* LA codes — only available from 2011 census onward (school year 4+)
local i = 11
forvalues num = 4/11 {

	gen LA_school_code_yr`num' = ""
	gen LA_home_code_yr`num'   = ""

	local j = `i' + 1
	local k = `j' + 1

	replace LA_school_code_yr`num' = LA_school_code`i' if cohort == 1
	replace LA_school_code_yr`num' = LA_school_code`j' if cohort == 2
	replace LA_school_code_yr`num' = LA_school_code`k' if cohort == 3

	replace LA_home_code_yr`num' = LA_home_code`i' if cohort == 1
	replace LA_home_code_yr`num' = LA_home_code`j' if cohort == 2
	replace LA_home_code_yr`num' = LA_home_code`k' if cohort == 3

	local ++i
}


********************************************************************************
* SECTION 6 — FINAL CLEAN-UP AND SAVE
********************************************************************************

keep id_dfe female ybirth mbirth nowhite lang_neng imd_decile ///
     id_la_home id_la_school sen_age sen_plan_age ///
     nsiblings first_born groupid_sib ///
     fsm part_time sen sen_plan ///
     sen_besd sen_ld sen_pd sen_other ///
     sen_besd2 sen_ld2 sen_pd2 sen_other2 ///
     num_schools ///
     id_school_yr* lsoa_code_yr* ///
     LA_home_code_yr* LA_school_code_yr*

mdesc
compress

* Remove intermediate per-year files
foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {
	rm "file_path\census_20`yr'.dta"
}

save "file_path\census_clean.dta", replace
mdesc

********************************************************************************
* END OF FILE
********************************************************************************
