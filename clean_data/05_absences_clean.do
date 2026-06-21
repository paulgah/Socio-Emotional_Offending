********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 05_absences_clean.do
* Purpose : Import and clean pupil-level absence records (2008–2015); sum
*           authorised, unauthorised, and exclusion-related absences across
*           terms; merge annual files into a wide panel; construct cohort-
*           aligned cumulative absence measures from Reception to KS2.
*
* Inputs  : Absence_20YY_3Term.csv  (YY = 08 … 15)
*           eyfsp_clean.dta         (cohort)
*
* Output  : absences_clean.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
*           Intermediate per-year .dta files are deleted after merging.
*           Special schools report annual rather than termly sessions;
*           annual figures are used to fill missing termly totals.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOOP OVER ANNUAL ABSENCE FILES (2008–2015)
*             Clean, collapse to one row per pupil, merge EYFSP cohort,
*             construct absence counts, save to disk.
********************************************************************************

foreach yr in "08" "09" "10" "11" "12" "13" "14" "15" {

	import delimited "file_path\Absence_20`yr'_3Term.csv", clear

	*--------------------------------------------------------------------------
	* 1.1  Keep variables of interest for this wave
	*--------------------------------------------------------------------------
	keep pupilmatchingrefanonymous_ab`yr' estab_ab`yr' ///
	     authorisedabsence_autumn_ab`yr'   authorisedabsence_spring_ab`yr'   authorisedabsence_summer_ab`yr'   authorisedabsence_annual_ab`yr'   ///
	     unauthorisedabsence_autumn_ab`yr' unauthorisedabsence_spring_ab`yr' unauthorisedabsence_summer_ab`yr' unauthorisedabsence_annual_ab`yr' ///
	     sessionspossible_autumn_ab`yr'    sessionspossible_spring_ab`yr'    sessionspossible_summer_ab`yr'    sessionspossible_annual_ab`yr'    ///
	     annualreasone_ab`yr'

	*--------------------------------------------------------------------------
	* 1.2  Deduplicate within pupil-establishment before collapsing
	*      (guards against duplicate rows in the raw file)
	*--------------------------------------------------------------------------
	sort pupilmatchingrefanonymous_ab`yr' estab_ab`yr'
	bys  pupilmatchingrefanonymous_ab`yr' estab_ab`yr': gen n = _n
	tab n
	keep if n == 1
	drop n

	*--------------------------------------------------------------------------
	* 1.3  Collapse to one row per pupil, summing across establishments
	*      (pupils attending more than one school in the year have multiple rows)
	*      annualreasone (exclusion absences) is taken as the max across rows.
	*--------------------------------------------------------------------------
	collapse ///
		(sum) authorisedabsence_autumn_ab`yr'   authorisedabsence_spring_ab`yr'   ///
		      authorisedabsence_summer_ab`yr'   authorisedabsence_annual_ab`yr'   ///
		      unauthorisedabsence_autumn_ab`yr' unauthorisedabsence_spring_ab`yr' ///
		      unauthorisedabsence_summer_ab`yr' unauthorisedabsence_annual_ab`yr' ///
		      sessionspossible_autumn_ab`yr'    sessionspossible_spring_ab`yr'    ///
		      sessionspossible_summer_ab`yr'    sessionspossible_annual_ab`yr'    ///
		(max) annualreasone_ab`yr' ///
		, by(pupilmatchingrefanonymous_ab`yr')

	*--------------------------------------------------------------------------
	* 1.4  Rename identifier
	*--------------------------------------------------------------------------
	rename pupilmatchingrefanonymous_ab`yr' id_dfe

	*--------------------------------------------------------------------------
	* 1.5  Merge cohort from EYFSP; drop non-EYFSP records
	*--------------------------------------------------------------------------
	merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
	bys cohort: tab _merge
	drop if _merge == 1
	drop _merge

	*--------------------------------------------------------------------------
	* 1.6  Construct termly absence and session totals
	*      Primary source: sum of autumn + spring + summer terms.
	*      Fallback: annual figure (used for special schools).
	*--------------------------------------------------------------------------

	* Authorised absences
	gen num_absences`yr' = authorisedabsence_autumn_ab`yr' ///
	                     + authorisedabsence_spring_ab`yr'  ///
	                     + authorisedabsence_summer_ab`yr'
	replace num_absences`yr' = authorisedabsence_annual_ab`yr' if num_absences`yr' == .
	summ num_absences`yr'

	* Unauthorised absences
	gen num_uabsences`yr' = unauthorisedabsence_autumn_ab`yr' ///
	                      + unauthorisedabsence_spring_ab`yr'  ///
	                      + unauthorisedabsence_summer_ab`yr'
	replace num_uabsences`yr' = unauthorisedabsence_annual_ab`yr' if num_uabsences`yr' == .
	summ num_uabsences`yr'

	* Possible sessions
	gen num_sessions`yr' = sessionspossible_autumn_ab`yr' ///
	                     + sessionspossible_spring_ab`yr'  ///
	                     + sessionspossible_summer_ab`yr'
	replace num_sessions`yr' = sessionspossible_annual_ab`yr' if num_sessions`yr' == .
	summ num_sessions`yr'

	*--------------------------------------------------------------------------
	* 1.7  Exclusion-related absences
	*--------------------------------------------------------------------------
	gen absences_exc`yr' = annualreasone_ab`yr'
	summ absences_exc`yr'

	*--------------------------------------------------------------------------
	* 1.8  Keep, compress, and save
	*--------------------------------------------------------------------------
	keep id_dfe cohort num_absences`yr' num_uabsences`yr' num_sessions`yr' absences_exc`yr'

	mdesc
	compress
	save "file_path\absences_20`yr'.dta", replace

} // end year loop


********************************************************************************
* SECTION 2 — MERGE ANNUAL FILES INTO A SINGLE WIDE PANEL
********************************************************************************

use "file_path\absences_2008.dta", clear

foreach yr in "09" "10" "11" "12" "13" "14" "15" {
	merge 1:1 id_dfe using "file_path\absences_20`yr'.dta"
	drop _merge
	rm "file_path\absences_20`yr'.dta"
}
rm "file_path\absences_2008.dta"

* Intermediate save of wide panel (before cohort aggregation)
save "file_path\absences_clean.dta", replace


********************************************************************************
* SECTION 3 — COHORT-ALIGNED CUMULATIVE ABSENCE MEASURES (Reception to KS2)
*
* Cohort 1: academic years 2008–2013 (Spring 2008 Reception → KS2 2013)
* Cohort 2: academic years 2009–2014
* Cohort 3: academic years 2010–2015
********************************************************************************

*------------------------------------------------------------------------------
* 3.1  Authorised absences
*------------------------------------------------------------------------------
egen absences1 = rowtotal(num_absences08 num_absences09 num_absences10 num_absences11 num_absences12 num_absences13), missing
egen absences2 = rowtotal(num_absences09 num_absences10 num_absences11 num_absences12 num_absences13 num_absences14), missing
egen absences3 = rowtotal(num_absences10 num_absences11 num_absences12 num_absences13 num_absences14 num_absences15), missing

gen num_absences = .
replace num_absences = absences1 if cohort == 1
replace num_absences = absences2 if cohort == 2
replace num_absences = absences3 if cohort == 3
drop absences1 absences2 absences3

summ num_absences
count if num_absences == 0
replace num_absences = 0 if num_absences == .

*------------------------------------------------------------------------------
* 3.2  Unauthorised absences
*------------------------------------------------------------------------------
egen uabsences1 = rowtotal(num_uabsences08 num_uabsences09 num_uabsences10 num_uabsences11 num_uabsences12 num_uabsences13), missing
egen uabsences2 = rowtotal(num_uabsences09 num_uabsences10 num_uabsences11 num_uabsences12 num_uabsences13 num_uabsences14), missing
egen uabsences3 = rowtotal(num_uabsences10 num_uabsences11 num_uabsences12 num_uabsences13 num_uabsences14 num_uabsences15), missing

gen num_uabsences = .
replace num_uabsences = uabsences1 if cohort == 1
replace num_uabsences = uabsences2 if cohort == 2
replace num_uabsences = uabsences3 if cohort == 3
drop uabsences1 uabsences2 uabsences3

summ num_uabsences
count if num_uabsences == 0
replace num_uabsences = 0 if num_uabsences == .

*------------------------------------------------------------------------------
* 3.3  Possible sessions
*      Missing replaced with sample mean — pupils with no session data are
*      assumed to have an average exposure.
*------------------------------------------------------------------------------
egen sessions1 = rowtotal(num_sessions08 num_sessions09 num_sessions10 num_sessions11 num_sessions12 num_sessions13), missing
egen sessions2 = rowtotal(num_sessions09 num_sessions10 num_sessions11 num_sessions12 num_sessions13 num_sessions14), missing
egen sessions3 = rowtotal(num_sessions10 num_sessions11 num_sessions12 num_sessions13 num_sessions14 num_sessions15), missing

gen num_sessions = .
replace num_sessions = sessions1 if cohort == 1
replace num_sessions = sessions2 if cohort == 2
replace num_sessions = sessions3 if cohort == 3
drop sessions1 sessions2 sessions3

count if num_sessions == 0
replace num_sessions = . if num_sessions == 0
summ num_sessions
replace num_sessions = r(mean) if num_sessions == .

*------------------------------------------------------------------------------
* 3.4  Absence rates
*------------------------------------------------------------------------------
gen share_absences  = (num_absences + num_uabsences) / num_sessions
gen share_uabsences = num_uabsences / num_sessions
summ share_absences,  d
summ share_uabsences, d

*------------------------------------------------------------------------------
* 3.5  Exclusion-related absences
*------------------------------------------------------------------------------
egen exclusions1 = rowtotal(absences_exc08 absences_exc09 absences_exc10 absences_exc11 absences_exc12 absences_exc13), missing
egen exclusions2 = rowtotal(absences_exc09 absences_exc10 absences_exc11 absences_exc12 absences_exc13 absences_exc14), missing
egen exclusions3 = rowtotal(absences_exc10 absences_exc11 absences_exc12 absences_exc13 absences_exc14 absences_exc15), missing

gen absences_exc = .
replace absences_exc = exclusions1 if cohort == 1
replace absences_exc = exclusions2 if cohort == 2
replace absences_exc = exclusions3 if cohort == 3
drop exclusions1 exclusions2 exclusions3

summ absences_exc
replace absences_exc = 0 if absences_exc == .


********************************************************************************
* SECTION 4 — FINAL CLEAN-UP AND SAVE
********************************************************************************

keep id_dfe cohort num_absences num_uabsences num_sessions ///
     share_absences share_uabsences absences_exc

mdesc
compress

save "file_path\absences_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
