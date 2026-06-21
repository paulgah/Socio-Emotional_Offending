********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 05a_main_mediation.do
* Purpose : Bootstrap wrapper for the mediation analysis. Resamples clusters
*           (schools) with replacement on each iteration; calls
*           11_mediation_boot.do to estimate indirect effects and store
*           coefficients. The final iteration uses the original (unsampled)
*           data to obtain point estimates.
*
* Inputs  : data.dta               (analysis sample, produced by 12_prep.do)
*           05b_mediation_boot.do   (inner loop)
*
* Output  : coeff.dta              (bootstrap coefficient matrix)
*
* Globals set here (consumed by 11_mediation_boot.do):
*   `skills'     — cogn_esem semo_esem
*   `controls'   — pupil-level controls
*   `n'          — current iteration
*   `iteration'  — total iterations
*   `path'       — root directory
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
*           Bootstrap uses 250 cluster-resampled iterations; the point
*           estimate is stored on iteration 250 (unsampled data).
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — SETTINGS
********************************************************************************

local iteration = 250
set seed 12345

local path "file_path/mediation_analysis"

local outcomes  sen cin excluded absences_score ks2_score offending

local skills   "cogn_esem semo_esem"
local controls "female nsiblings first_born nowhite fsm b10.imd_decile i.ybirth i.mbirth lang_neng part_time sen_prior cin_prior"



********************************************************************************
* SECTION 2 — LOAD DATA AND CACHE AS TEMPFILE
********************************************************************************

use "`path'/data/data.dta", clear
tempfile data
save `data', replace


********************************************************************************
* SECTION 3 — BOOTSTRAP LOOP
*
* Iterations 1 to (iteration-1): cluster-resample by school_KS2.
* Final iteration (`iteration'): use original data for point estimates.
********************************************************************************

forval n = 1/`iteration' {

	use `data', clear

	if `n' < `iteration' {
		* Cluster resample by school
		bsample, cluster(school_KS2)
	}
	* Final iteration: original data used as-is for point estimates.

	include "`path'/files/05b_mediation_boot.do"

} // end bootstrap loop

********************************************************************************
* END OF FILE
********************************************************************************
