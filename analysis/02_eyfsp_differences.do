********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 02_eyfsp_differences.do
* Purpose : Construct plots of the difference in the distribution of EYFSP
*           point scales between offenders and non-offenders.
*
* Inputs  : eyfsp_clean.dta
*           clean_data.dta    (ybirth, missing_vars, offending)
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOAD AND RESTRICT SAMPLE
********************************************************************************

use "file_path\eyfsp_clean.dta", clear

merge 1:1 id_dfe using "file_path\clean_data.dta", ///
	keepusing(ybirth missing_vars offending LA_school_code_yr6)
drop _merge

keep if missing_vars == 0
drop if score_eyfsp_overall == 0

tab ybirth
replace ybirth = . if inlist(ybirth, 2000, 2005, 2008)
drop if ybirth == .


********************************************************************************
* SECTION 2 — RESTRICT TO PUPILS WITH COMPLETE EYFSP POINT SCALES
********************************************************************************

egen miss_all = rowmiss(att* soc* emo* lang* link* read* write* ///
                        num* cal* space* know* phy* cre*)
tab miss_all
keep if miss_all == 0


********************************************************************************
* SECTION 3 — COUNT PUPILS BY OFFENDING STATUS AND POINT SCORE
*
* For each EYFSP subscale variable, collapse to counts by offending × point
* score, save to a tempfile, then append all tempfiles.
*
********************************************************************************

* Count loop
local i = 1
local varcount = 0
foreach var of varlist att* soc* emo* lang* link* read* write* ///
                       num* cal* space* know* phy* cre* {
	local ++varcount
}

foreach var of varlist att* soc* emo* lang* link* read* write* ///
                       num* cal* space* know* phy* cre* {
	preserve
	gen N = 1
	collapse (sum) N, by(offending `var')
	gen var = "`var'"
	rename `var' points
	tempfile counts_`i'
	save `counts_`i'', replace
	restore
	local ++i
}

* Append count tempfiles
use `counts_1', clear
forvalues num = 2/`varcount' {
	append using `counts_`num''
}
* Intermediate count dataset available here if needed for output.


********************************************************************************
* SECTION 4 — PROPORTION OF PUPILS AT EACH POINT SCORE BY OFFENDING STATUS
********************************************************************************

use "file_path\eyfsp_clean.dta", clear

merge 1:1 id_dfe using "file_path\clean_data.dta", ///
	keepusing(ybirth missing_vars offending)
drop _merge

keep if missing_vars == 0
drop if score_eyfsp_overall == 0
replace ybirth = . if inlist(ybirth, 2000, 2005, 2008)
drop if ybirth == .

egen miss_all = rowmiss(att* soc* emo* lang* link* read* write* ///
                        num* cal* space* know* phy* cre*)
keep if miss_all == 0

* Collapse to mean proportion at each point score by offending status
* Each att*/soc*/etc. variable contains the point score (1–9); collapsing
* (mean) within offending gives the proportion at each level.

collapse att* soc* emo* lang* link* read* write* ///
         num* cal* space* know* phy* cre*, by(offending)

reshape long att soc emo lang link read write num cal space know phy cre, ///
	i(offending) j(points)

reshape wide att soc emo lang link read write num cal space know phy cre, ///
	i(points) j(offending)

* Percentage point difference (offenders minus non-offenders)
foreach var in att soc emo lang link read write num cal space know phy cre {
	gen `var'_diff = (`var'1 - `var'0) * 100
	summ `var'_diff
}

browse points *diff


********************************************************************************
* SECTION 5 — PLOTS
*
* Percentage point difference in EYFSP achievement by point scale,
* offenders versus non-offenders.
********************************************************************************

* Personal, Social and Emotional Development (PSED): attitude, social, emotional
twoway ///
	(connected att_diff points, ///
		msymbol(Oh) mcolor(navy)    msize(medium) lcolor(navy)    lwidth(medium)) ///
	(connected soc_diff points, ///
		msymbol(Sh) mcolor(dkorange) msize(medium) lcolor(dkorange) lwidth(medium)) ///
	(connected emo_diff points, ///
		msymbol(Th) mcolor(gs10)    msize(medium) lcolor(gs10)    lwidth(medium)) ///
	, graphregion(color(white)) ///
	legend(position(6) row(1) order(1 "Attitude" 2 "Social" 3 "Emotional")) ///
	xlabel(1(1)9, angle(0)) ///
	ylabel(-25(5)0, angle(0)) ///
	xtitle("Scale points", size(medium)) ///
	ytitle("Percentage point difference", size(medium))

********************************************************************************
* END OF FILE
********************************************************************************
