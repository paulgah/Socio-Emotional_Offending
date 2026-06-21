********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 06_exclusions_clean.do
* Purpose : Import and clean pupil-level exclusion records (2008–2015);
*           classify exclusion reasons and categories; merge annual files into
*           a wide panel; construct cohort-aligned cumulative exclusion
*           measures from Reception to KS2.
*
* Inputs  : Exclusions_20YY_Table3.csv  (YY = 08 … 15)
*           eyfsp_clean.dta             (cohort)
*
* Output  : exclusions_clean.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
*           Intermediate per-year .dta files are deleted after merging.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOOP OVER ANNUAL EXCLUSION FILES (2008–2015)
********************************************************************************

foreach yr in "08" "09" "10" "11" "12" "13" "14" "15" {

	import delimited "file_path\Exclusions_20`yr'_Table3.csv", clear

	*--------------------------------------------------------------------------
	* 1.1  Identifiers
	*--------------------------------------------------------------------------
	rename pupilmatchingrefanonymous_ex`yr' id_dfe

	*--------------------------------------------------------------------------
	* 1.2  Deduplicate within pupil (informational only — tab n)
	*--------------------------------------------------------------------------
	sort id_dfe
	bys id_dfe: gen n = _n
	tab n
	drop n

	*--------------------------------------------------------------------------
	* 1.3  Merge cohort from EYFSP; drop non-EYFSP records
	*--------------------------------------------------------------------------
	merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
	drop if _merge == 1
	drop _merge

	*--------------------------------------------------------------------------
	* 1.4  Exclusion counts and sessions
	*--------------------------------------------------------------------------
	rename permanentexclusioncount_ex`yr' perm_exc`yr'
	tab perm_exc`yr'

	rename totalfixedsessions_ex`yr'   sessions_fexc`yr'
	rename totalfixedexclusions_ex`yr' num_fexc`yr'
	summ sessions_fexc`yr' num_fexc`yr'

	*--------------------------------------------------------------------------
	* 1.5  Exclusion reasons (iterate over all reason* variables)
	*
	*   PP / PA  → physical assault (against pupil / adult)
	*   VP / VA / RA / BU → verbal abuse (against pupil/adult, racist, bullying)
	*   SM / DA / DM / TH / DB → misbehaviour (sexual, drugs, damage, theft, disruptive)
	*   OT       → other
	*--------------------------------------------------------------------------
	gen physical_assault`yr' = 0
	gen verbal_abuse`yr'     = 0
	gen misbehaviour`yr'     = 0
	gen other_exc`yr'        = 0

	foreach var of varlist reason* {
		tostring `var', force replace
		replace physical_assault`yr' = 1 if inlist(`var', "PP", "PA")
		replace verbal_abuse`yr'     = 1 if inlist(`var', "VP", "VA", "RA", "BU")
		replace misbehaviour`yr'     = 1 if inlist(`var', "SM", "DA", "DM", "TH", "DB")
		replace other_exc`yr'        = 1 if `var' == "OT"
	}

	tab physical_assault`yr'
	tab verbal_abuse`yr'
	tab misbehaviour`yr'
	tab other_exc`yr'

	*--------------------------------------------------------------------------
	* 1.6  Exclusion categories (iterate over all category* variables)
	*
	*   PERM        → permanent exclusion
	*   FIXD / SUSP → fixed-period exclusion
	*   LNCH        → lunchtime exclusion
	*--------------------------------------------------------------------------
	gen permanent`yr' = 0
	gen fixed`yr'     = 0
	gen lunch`yr'     = 0

	foreach var of varlist category* {
		tostring `var', force replace
		replace permanent`yr' = 1 if `var' == "PERM"
		replace fixed`yr'     = 1 if inlist(`var', "FIXD", "SUSP")
		replace lunch`yr'     = 1 if `var' == "LNCH"
	}

	tab permanent`yr'
	tab fixed`yr'
	tab lunch`yr'

	*--------------------------------------------------------------------------
	* 1.7  Keep, compress, and save
	*--------------------------------------------------------------------------
	keep id_dfe cohort perm_exc`yr' sessions_fexc`yr' num_fexc`yr' ///
	     physical_assault`yr' verbal_abuse`yr' misbehaviour`yr' other_exc`yr' ///
	     permanent`yr' fixed`yr' lunch`yr'

	mdesc
	compress
	save "file_path\exclusions_20`yr'.dta", replace

} // end year loop


********************************************************************************
* SECTION 2 — MERGE ANNUAL FILES INTO A SINGLE WIDE PANEL
********************************************************************************

use "file_path\exclusions_2008.dta", clear

foreach yr in "09" "10" "11" "12" "13" "14" "15" {
	merge 1:1 id_dfe using "file_path\exclusions_20`yr'.dta"
	drop _merge
	rm "file_path\exclusions_20`yr'.dta"
}
rm "file_path\exclusions_2008.dta"

save "file_path\exclusions_clean.dta", replace


********************************************************************************
* SECTION 3 — COHORT-ALIGNED CUMULATIVE EXCLUSION MEASURES (Reception to KS2)
*
* Cohort 1: academic years 2008–2013
* Cohort 2: academic years 2009–2014
* Cohort 3: academic years 2010–2015
********************************************************************************

*------------------------------------------------------------------------------
* 3.1  Total fixed exclusions
*------------------------------------------------------------------------------
egen num_fexc1 = rowtotal(num_fexc08 num_fexc09 num_fexc10 num_fexc11 num_fexc12 num_fexc13), missing
egen num_fexc2 = rowtotal(num_fexc09 num_fexc10 num_fexc11 num_fexc12 num_fexc13 num_fexc14), missing
egen num_fexc3 = rowtotal(num_fexc10 num_fexc11 num_fexc12 num_fexc13 num_fexc14 num_fexc15), missing

gen num_fexc = .
replace num_fexc = num_fexc1 if cohort == 1
replace num_fexc = num_fexc2 if cohort == 2
replace num_fexc = num_fexc3 if cohort == 3
drop num_fexc1 num_fexc2 num_fexc3

summ num_fexc
replace num_fexc = 0 if num_fexc == .

*------------------------------------------------------------------------------
* 3.2  Sessions missed due to fixed exclusions
*------------------------------------------------------------------------------
egen sessions_fexc1 = rowtotal(sessions_fexc08 sessions_fexc09 sessions_fexc10 sessions_fexc11 sessions_fexc12 sessions_fexc13), missing
egen sessions_fexc2 = rowtotal(sessions_fexc09 sessions_fexc10 sessions_fexc11 sessions_fexc12 sessions_fexc13 sessions_fexc14), missing
egen sessions_fexc3 = rowtotal(sessions_fexc10 sessions_fexc11 sessions_fexc12 sessions_fexc13 sessions_fexc14 sessions_fexc15), missing

gen sessions_fexc = .
replace sessions_fexc = sessions_fexc1 if cohort == 1
replace sessions_fexc = sessions_fexc2 if cohort == 2
replace sessions_fexc = sessions_fexc3 if cohort == 3
drop sessions_fexc1 sessions_fexc2 sessions_fexc3

summ sessions_fexc
replace sessions_fexc = 0 if sessions_fexc == .

*------------------------------------------------------------------------------
* 3.3  Ever-excluded indicators (rowmax across cohort years = 1 if any year = 1)
*------------------------------------------------------------------------------

foreach stub in perm_exc physical_assault verbal_abuse misbehaviour other_exc {

	egen `stub'1 = rowmax(`stub'08 `stub'09 `stub'10 `stub'11 `stub'12 `stub'13)
	egen `stub'2 = rowmax(`stub'09 `stub'10 `stub'11 `stub'12 `stub'13 `stub'14)
	egen `stub'3 = rowmax(`stub'10 `stub'11 `stub'12 `stub'13 `stub'14 `stub'15)

	* Rename target variable for perm_exc (used as-is); for reason indicators
	* append _exc suffix to distinguish cumulative from annual variables.
	local outvar = cond("`stub'" == "perm_exc", "perm_exc", "`stub'_exc")

	gen `outvar' = .
	replace `outvar' = `stub'1 if cohort == 1
	replace `outvar' = `stub'2 if cohort == 2
	replace `outvar' = `stub'3 if cohort == 3
	drop `stub'1 `stub'2 `stub'3

	summ `outvar'
	replace `outvar' = 0 if `outvar' == .
}


********************************************************************************
* SECTION 4 — FINAL CLEAN-UP AND SAVE
********************************************************************************

keep id_dfe num_fexc sessions_fexc perm_exc ///
     physical_assault_exc verbal_abuse_exc misbehaviour_exc other_exc_exc

* Rename other_exc_exc for clarity
rename other_exc_exc other_reason_exc

mdesc
compress

save "file_path\exclusions_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
