********************************************************************************
* Project: Socio-Emotional Characteristics in Early Childhood and Offending
*          Behaviour in Adolescence
* Author:  Paul Garcia
* Institution: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
* Description: Extracts and cleans demographic, SEN, family and school 
*              characteristics from the National Pupil Database (NPD) Spring 
*              School Census (2007-2020). Merges with EYFSP data and constructs
*              time-invariant and time-varying variables for analysis.
********************************************************************************

clear all

********************************************************************************
* PART 1: Loop over Spring Census files (2007-2020)
* For each year: import, clean, and save a temporary cleaned census file
********************************************************************************

foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {

	import delimited "file_path\Spring_Census_20`yr'.csv", clear

	*---------------------------------------------------------------------------
	* Identifiers
	*---------------------------------------------------------------------------
	rename pupilmatchingrefanonymous_spr`yr' id_dfe

	* Check for duplicate pupil records within year
	bys id_dfe: gen n = _n
	tab n
	drop n

	*---------------------------------------------------------------------------
	* Keep only pupils with EYFSP information
	* Merge with clean EYFSP file to restrict to matched cohort
	*---------------------------------------------------------------------------
	merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(id_dfe cohort)
	keep if _merge == 3
	drop _merge

	*---------------------------------------------------------------------------
	* Demographics
	*---------------------------------------------------------------------------

	* Local Authority codes (available from 2011 onwards)
	capture confirm variable la_9code_spr`yr'
	if _rc == 0 {
		rename la_9code_spr`yr'     LA_school_code`yr'
		rename homela_9code_spr`yr' LA_home_code`yr'
	}

	* Gender
	tab gender_spr`yr'
	gen female`yr' = (gender_spr`yr' == "F")
	replace female`yr' = . if gender_spr`yr' == ""
	tab female`yr'
	label var female`yr' "whether female"

	* Age at start of academic year
	tab ageatstartofacademicyear_spr`yr'
	rename ageatstartofacademicyear_spr`yr' age`yr'
	label var age`yr' "age at the start of academic year"

	* Year of birth
	tab yearofbirth_spr`yr'
	rename yearofbirth_spr`yr' ybirth`yr'
	label var ybirth`yr' "year of birth"

	* Month of birth
	tab monthofbirth_spr`yr'
	rename monthofbirth_spr`yr' mbirth`yr'
	label var mbirth`yr' "month of birth"

	* Ethnicity: non-white indicator (excludes unclassified)
	tab ethnicgroupmajor_spr`yr'
	gen nowhite`yr' = (ethnicgroupmajor_spr`yr' != "WHIT")
	replace nowhite`yr' = . if ethnicgroupmajor_spr`yr' == "UNCL" | ethnicgroupmajor_spr`yr' == ""
	tab nowhite`yr'
	label var nowhite`yr' "whether pupil is not white"

	* Language: non-English first language (excludes unclassified/refused)
	tab languagegroupmajor_spr`yr'
	gen lang_neng`yr' = (languagegroupmajor_spr`yr' != "1_ENG")
	replace lang_neng`yr' = . if languagegroupmajor_spr`yr' == "3_UNCL" | languagegroupmajor_spr`yr' == ""
	tab lang_neng`yr'
	label var lang_neng`yr' "whether pupil's major language is not English"

	*---------------------------------------------------------------------------
	* Family socioeconomic status
	*---------------------------------------------------------------------------

	* Free School Meals eligibility
	tab fsmeligible_spr`yr'
	rename fsmeligible_spr`yr' fsm`yr'
	label var fsm`yr' "whether pupil is FSM eligible"

	* Income Deprivation Affecting Children Index (IDACI) decile
	* 1 = most deprived, 10 = least deprived
	* Variable name changed in 2015 so two capture blocks handle both versions
	capture confirm variable idacidecile_spr`yr'
	if _rc == 0 {
		rename idacidecile_spr`yr' imd_decile`yr'
		label var imd_decile`yr' "IMD affecting children (decile)"
	}

	capture confirm variable idacidecile_15_spr`yr'
	if _rc == 0 {
		rename idacidecile_15_spr`yr' imd_decile`yr'
		label var imd_decile`yr' "IMD affecting children (decile)"
	}

	* Siblings and birth order: only available in census years 2008, 2013, 2016
	if ("`yr'" == "08" | "`yr'" == "13" | "`yr'" == "16") {
		tab nsiblings_sgas_spr`yr'
		rename nsiblings_sgas_spr`yr' nsiblings`yr'
		tab nsiblings`yr'

		tab birthorder_sgas_spr`yr'
		gen first_born`yr' = (birthorder_sgas_spr`yr' == 1)
		replace first_born`yr' = . if birthorder_sgas_spr`yr' == .
		tab first_born`yr'

		rename groupid_sgas_spr`yr' groupid_sib`yr'
	}

	*---------------------------------------------------------------------------
	* School characteristics
	*---------------------------------------------------------------------------

	* Part-time attendance
	tab parttime_spr`yr'
	rename parttime_spr`yr' part_time`yr'
	label var part_time`yr' "whether pupil is part time"

	* SEN provision: any SEN status
	tab senprovision_spr`yr'
	gen sen`yr' = (senprovision_spr`yr' != "N")
	replace sen`yr' = . if senprovision_spr`yr' == ""
	tab sen`yr'
	label var sen`yr' "whether pupil has SEN status"

	* SEN statement or EHC plan
	* Variable and coding changed after 2008
	if ("`yr'" == "07" | "`yr'" == "08") {
		gen sen_plan`yr' = (senprovision_spr`yr' == "S")
		tab sen_plan`yr'
		label var sen_plan`yr' "whether pupil has SEN statement or EHC plan"
	}
	if ("`yr'" != "07" & "`yr'" != "08") {
		gen sen_plan`yr' = (senprovisionmajor_spr`yr' == "3_SS")
		replace sen_plan`yr' = . if senprovisionmajor_spr`yr' == "" | senprovisionmajor_spr`yr' == "4_UNCL"
		tab sen_plan`yr'
		label var sen_plan`yr' "whether pupil has SEN statement or EHC plan"
	}

	* SEN type: Behavioural, Emotional and Social Difficulties (BESD/SEMH)
	* Primary need
	tab primarysentype_spr`yr'
	gen sen_besd`yr' = 0 if primarysentype_spr`yr' != ""
	replace sen_besd`yr' = 1 if primarysentype_spr`yr' == "BESD" | primarysentype_spr`yr' == "SEMH"
	tab sen_besd`yr'
	label var sen_besd`yr' "whether primary need behav, emot, soc diff."

	* Secondary need
	tab secondarysentype_spr`yr'
	gen sen_besd2`yr' = 0 if secondarysentype_spr`yr' != ""
	replace sen_besd2`yr' = 1 if secondarysentype_spr`yr' == "BESD" | secondarysentype_spr`yr' == "SEMH"
	tab sen_besd2`yr'
	label var sen_besd2`yr' "whether secondary need behav, emot, soc diff."

	* SEN type: Learning Difficulties (primary and secondary)
	tab primarysentype_spr`yr'
	gen sen_ld`yr' = 0 if primarysentype_spr`yr' != ""
	replace sen_ld`yr' = 1 if inlist(primarysentype_spr`yr', "SPLD", "MLD", "SLS", "PMLD")
	tab sen_ld`yr'
	label var sen_ld`yr' "whether primary need multiple learning diff"

	tab secondarysentype_spr`yr'
	gen sen_ld2`yr' = 0 if secondarysentype_spr`yr' != ""
	replace sen_ld2`yr' = 1 if inlist(secondarysentype_spr`yr', "SPLD", "MLD", "SLS", "PMLD")
	tab sen_ld2`yr'
	label var sen_ld2`yr' "whether secondary need multiple learning diff"

	* SEN type: Physical/sensory disability (primary and secondary)
	tab primarysentype_spr`yr'
	gen sen_pd`yr' = 0 if primarysentype_spr`yr' != ""
	replace sen_pd`yr' = 1 if inlist(primarysentype_spr`yr', "SLCN", "HI", "VI", "MSI", "PD")
	tab sen_pd`yr'
	label var sen_pd`yr' "whether primary need physical disability"

	tab secondarysentype_spr`yr'
	gen sen_pd2`yr' = 0 if secondarysentype_spr`yr' != ""
	replace sen_pd2`yr' = 1 if inlist(secondarysentype_spr`yr', "SLCN", "HI", "VI", "MSI", "PD")
	tab sen_pd2`yr'
	label var sen_pd2`yr' "whether secondary need physical disability"

	* SEN type: Other (ASD, other, not specified) (primary and secondary)
	tab primarysentype_spr`yr'
	gen sen_other`yr' = 0 if primarysentype_spr`yr' != ""
	replace sen_other`yr' = 1 if inlist(primarysentype_spr`yr', "ASD", "OTH", "NSA")
	tab sen_other`yr'
	label var sen_other`yr' "whether primary need other difficulty"

	tab secondarysentype_spr`yr'
	gen sen_other2`yr' = 0 if secondarysentype_spr`yr' != ""
	replace sen_other2`yr' = 1 if inlist(secondarysentype_spr`yr', "ASD", "OTH", "NSA")
	tab sen_other2`yr'
	label var sen_other2`yr' "whether secondary need other difficulty"

	* School and home Local Authority identifiers
	summ la_spr`yr'
	rename la_spr`yr' id_la_school`yr'

	tab homela_spr`yr'
	rename homela_spr`yr' id_la_home`yr'

	* School URN identifier
	rename urn_spr`yr' id_school`yr'

	* Lower Super Output Area (LSOA) code
	* Variable name changed from llsoa to lsoa11 from 2013 onwards
	if inlist("`yr'", "07", "08", "09", "10", "11", "12") {
		gen lsoa_code`yr' = ""
		replace lsoa_code`yr' = llsoa_spr`yr'
	}
	if inlist("`yr'", "13", "14", "15", "16", "17", "18", "19", "20") {
		gen lsoa_code`yr' = ""
		replace lsoa_code`yr' = lsoa11_spr`yr'
	}

	*---------------------------------------------------------------------------
	* Keep only variables of interest (using capture to handle year-specific vars)
	*---------------------------------------------------------------------------
	local varlist id_dfe age`yr' female`yr' fsm`yr' ybirth`yr' mbirth`yr' ///
		nowhite`yr' lang_neng`yr' imd_decile`yr' part_time`yr' sen`yr'     ///
		sen_plan`yr' sen_besd`yr' sen_ld`yr' sen_pd`yr' sen_other`yr'       ///
		sen_besd2`yr' sen_ld2`yr' sen_pd2`yr' sen_other2`yr'                ///
		LA_home_code`yr' LA_school_code`yr' id_la_school`yr' id_la_home`yr' ///
		id_school`yr' nsiblings`yr' first_born`yr' groupid_sib`yr' lsoa_code`yr'

	local keeplist
	foreach var of local varlist {
		capture confirm variable `var'
		if !_rc {
			local keeplist `keeplist' `var'
		}
	}

	keep `keeplist'
	mdesc
	compress
	save "file_path\census_20`yr'.dta", replace

} // end census loop

********************************************************************************
* PART 2: Merge all Census years with EYFSP data
********************************************************************************

use "file_path\eyfsp_clean.dta", clear
keep id_dfe cohort age_EYFSP

* Sequentially merge each census year; drop unmatched from census (keep EYFSP base)
foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {
	merge 1:1 id_dfe using "file_path\census_20`yr'.dta"
	drop if _merge == 2
	drop _merge
}

********************************************************************************
* PART 3: Track school changes across primary school years
* Identifies whether a pupil changed school between consecutive years
* Spring census year maps to cohort as follows:
*   Cohort 1: EYFSP in Spring 2007 (reception year)
*   Cohort 2: EYFSP in Spring 2008
*   Cohort 3: EYFSP in Spring 2009
********************************************************************************

gen dschool1 = (id_school08 != id_school09) if cohort == 1 & id_school07 != . & id_school08 != .
gen dschool2 = (id_school09 != id_school10) if (cohort == 1 | cohort == 2) & id_school08 != . & id_school09 != .
gen dschool3 = (id_school10 != id_school11) if id_school09 != . & id_school10 != .
gen dschool4 = (id_school11 != id_school12) if id_school10 != . & id_school11 != .
gen dschool5 = (id_school12 != id_school13) if id_school11 != . & id_school12 != .
gen dschool6 = (id_school13 != id_school14) if (cohort == 2 | cohort == 3) & id_school12 != . & id_school13 != .
gen dschool7 = (id_school14 != id_school15) if cohort == 3 & id_school13 != . & id_school14 != .

* Total number of school changes across primary years by cohort
egen num_schools1 = rowtotal(dschool1 dschool2 dschool3 dschool4 dschool5) if cohort == 1, missing
egen num_schools2 = rowtotal(dschool2 dschool3 dschool4 dschool5 dschool6) if cohort == 2, missing
egen num_schools3 = rowtotal(dschool3 dschool4 dschool5 dschool6 dschool7) if cohort == 3, missing

gen num_schools = .
replace num_schools = num_schools1 if cohort == 1
replace num_schools = num_schools2 if cohort == 2
replace num_schools = num_schools3 if cohort == 3
tab num_schools

* Convert home LA identifiers to numeric (force handles string/numeric conflicts)
destring id_la_home*, replace force

********************************************************************************
* PART 4: Construct time-invariant characteristics
* (a) Characteristics recorded at age of EYFSP (reception year)
* (b) Characteristics ever recorded between reception and KS2
********************************************************************************

* Initialise variables to be constructed
gen sen_age      = .  // age at first SEN record
gen sen_plan_age = .  // age at first SEN statement/EHC plan
gen nsiblings    = .
gen first_born   = .
gen groupid_sib  = .

*---------------------------------------------------------------------------
* (a) Time-invariant characteristics: assigned from reception year census,
*     with forward-fill for missing observations
*---------------------------------------------------------------------------
foreach var in "female" "ybirth" "mbirth" "nowhite" "lang_neng" "imd_decile" ///
               "id_la_home" "id_la_school" "id_school" {

	gen `var' = .

	* Assign from reception year census by cohort
	replace `var' = `var'07 if cohort == 1
	replace `var' = `var'08 if cohort == 2
	replace `var' = `var'09 if cohort == 3

	* Fill missing from any available census year
	foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {

		replace `var' = `var'`yr' if `var' == .

		* Age at first SEN record (any SEN provision)
		replace sen_age = age`yr' if sen_age == . & sen`yr' == 1 & age`yr' != .

		* Age at first SEN statement or EHC plan
		replace sen_plan_age = age`yr' if sen_plan_age == . & sen`yr' == 1 & sen_plan`yr' == 1 & age`yr' != .

		* Number of siblings: updated to maximum observed across years 2008 and 2013
		* (takes highest value to account for new siblings over time)
		if ("`yr'" == "08" | "`yr'" == "13") {
			replace nsiblings  = nsiblings`yr'  if (nsiblings == . | (nsiblings < nsiblings`yr')) & nsiblings`yr' != .
			replace first_born = first_born`yr' if first_born == . & first_born`yr' != .
		}

		* 2016 siblings: used only to fill missing (group identifier not stable across years)
		if ("`yr'" == "16") {
			replace nsiblings   = nsiblings`yr'   if nsiblings == . & nsiblings`yr' != .
			replace groupid_sib = groupid_sib`yr'
		}
	}
}

*---------------------------------------------------------------------------
* (b) Ever-recorded characteristics between reception and KS2
*     Uses rowmax to capture any positive record across relevant census years
*---------------------------------------------------------------------------
foreach var in "fsm" "part_time" "sen" "sen_plan" "sen_besd" "sen_ld" ///
               "sen_pd" "sen_other" "sen_besd2" "sen_ld2" "sen_pd2" "sen_other2" {

	* Ever recorded across primary school years (cohort-specific year range)
	egen `var'_1 = rowmax(`var'07 `var'08 `var'09 `var'10 `var'11 `var'12 `var'13) if cohort == 1
	egen `var'_2 = rowmax(`var'08 `var'09 `var'10 `var'11 `var'12 `var'13 `var'14) if cohort == 2
	egen `var'_3 = rowmax(`var'09 `var'10 `var'11 `var'12 `var'13 `var'14 `var'15) if cohort == 3

	gen `var' = .
	replace `var' = `var'_1 if cohort == 1
	replace `var' = `var'_2 if cohort == 2
	replace `var' = `var'_3 if cohort == 3
	tab `var'
}

********************************************************************************
* PART 5: Construct time-varying school and area identifiers
* Maps census year to school year (year 0 = reception, year 11 = age 16)
********************************************************************************

* Rename two-digit year suffixes to single digits for years 7-9 (avoids conflict)
rename (id_school07 id_school08 id_school09 lsoa_code07 lsoa_code08 lsoa_code09) ///
       (id_school7  id_school8  id_school9  lsoa_code7  lsoa_code8  lsoa_code9)

* School and LSOA identifiers by academic year (year 0-11)
* Cohort offset: cohort 1 starts in census year 7, cohort 2 in year 8, cohort 3 in year 9
local i = 7
foreach num of numlist 0/11 {
	gen id_school_yr`num'  = .
	gen lsoa_code_yr`num'  = ""

	replace id_school_yr`num' = id_school`i'         if cohort == 1
	replace lsoa_code_yr`num' = lsoa_code`i'         if cohort == 1

	local j = `i' + 1
	replace id_school_yr`num' = id_school`j'         if cohort == 2
	replace lsoa_code_yr`num' = lsoa_code`j'         if cohort == 2

	local k = `j' + 1
	replace id_school_yr`num' = id_school`k'         if cohort == 3
	replace lsoa_code_yr`num' = lsoa_code`k'         if cohort == 3

	local ++i
	display `i'
}

* LA codes by academic year (only available from year 4 onwards, i.e. 2011)
local i = 11
foreach num of numlist 4/11 {
	gen LA_school_code_yr`num' = ""
	gen LA_home_code_yr`num'   = ""

	replace LA_school_code_yr`num' = LA_school_code`i'  if cohort == 1
	replace LA_home_code_yr`num'   = LA_home_code`i'    if cohort == 1

	local j = `i' + 1
	replace LA_school_code_yr`num' = LA_school_code`j'  if cohort == 2
	replace LA_home_code_yr`num'   = LA_home_code`j'    if cohort == 2

	local k = `j' + 1
	replace LA_school_code_yr`num' = LA_school_code`k'  if cohort == 3
	replace LA_home_code_yr`num'   = LA_home_code`k'    if cohort == 3

	local ++i
	display `i'
}

********************************************************************************
* PART 6: Final clean-up and save
********************************************************************************

keep id_dfe female ybirth mbirth nowhite lang_neng imd_decile id_la_home      ///
     id_la_school sen_age sen_plan_age nsiblings first_born groupid_sib fsm    ///
     part_time sen sen_plan sen_besd sen_ld sen_pd sen_other sen_besd2         ///
     sen_ld2 sen_pd2 sen_other2 num_schools id_school_yr* LA_home_code_yr*     ///
     LA_school_code_yr*

mdesc

* Delete temporary annual census files
foreach yr in "07" "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" {
	rm "file_path\census_20`yr'.dta"
}

compress
save "file_path\census_clean.dta", replace
mdesc