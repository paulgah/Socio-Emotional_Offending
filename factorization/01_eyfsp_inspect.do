********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 01_eyfsp_inspect.do
* Purpose : Inspect internal consistency of EYFSP point scales; check for
*           violations of the cumulative scoring assumption; identify pairs
*           of items with tetrachoric correlations >= 0.90 and >= 0.85 for
*           item reduction prior to factor analysis.
*
* Inputs  : eyfsp_clean.dta
*           clean_data.dta    (missing_vars, offending, LA_school_code_yr6)
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
*           Items retained for factor analysis (after dropping highly
*           correlated items at the 0.85 threshold):
*           att4-att8 soc4-soc7 emo4-emo8 lang4-lang8 link4 link6
*           read4-read8 write4-write7 num4 num5 num8
*           cal4 cal6 cal7 cal8 space4-space7
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — LOAD AND RESTRICT SAMPLE
********************************************************************************

use "file_path\eyfsp_clean.dta", clear

keep id_dfe id_moj ///
     att* emo* soc* lang* link* read* write* ///
     num* cal* space* know* phy* cre* ///
     missing_EYFSP score_eyfsp_overall

keep if missing_EYFSP == 0
drop if score_eyfsp_overall == 0

merge 1:1 id_dfe using "file_path\clean_data.dta", ///
	keepusing(missing_vars offending LA_school_code_yr6)
keep if _merge == 3
drop _merge

encode LA_school_code_yr6, gen(LA_KS2)


********************************************************************************
* SECTION 2 — CUMULATIVE SCORING CHECKS
*
* Each subscale is cumulative: a pupil achieving point k should have achieved
* all points 1 to k-1. We check two violations:
*   (a) Achieving point 4+ without achieving all of points 1-3.
*   (b) Achieving point 9 while holding all lower points simultaneously
*       (i.e. confirming the ceiling group).
********************************************************************************

foreach stub in att emo soc lang link read write num cal space know phy cre {

	* Tetrachoric correlations across middle items (4-8)
	tetrachoric `stub'4 `stub'5 `stub'6 `stub'7 `stub'8

	* Violation (a): higher point achieved without all lower points
	count if (`stub'1 == 0 | `stub'2 == 0 | `stub'3 == 0) ///
	       & (`stub'4 == 1 | `stub'5 == 1 | `stub'6 == 1 ///
	        | `stub'7 == 1 | `stub'8 == 1 | `stub'9 == 1)

	* Violation (b): ceiling achievers (all 9 points)
	tab `stub'9
	count if `stub'1 == 1 & `stub'2 == 1 & `stub'3 == 1 ///
	       & `stub'4 == 1 & `stub'5 == 1 & `stub'6 == 1 ///
	       & `stub'7 == 1 & `stub'8 == 1 & `stub'9 == 1

	* Missing check (only needed for subscales with sparse coverage)
	if inlist("`stub'", "know", "phy", "cre") {
		egen miss = rowmiss(`stub'4 `stub'5 `stub'6 `stub'7 `stub'8)
		tab miss
		drop miss
	}

}

*------------------------------------------------------------------------------
* Domain-level tetrachoric matrices
*------------------------------------------------------------------------------

* Socio-emotional development (PSED)
tetrachoric ///
	att4 att5 att6 att7 att8 ///
	emo4 emo5 emo6 emo7 emo8 ///
	soc4 soc5 soc6 soc7 soc8

* Cognitive development (CLL + PSRN)
tetrachoric ///
	lang4 lang5 lang6 lang7 lang8 ///
	link4 link5 link6 link7 link8 ///
	read4 read5 read6 read7 read8 ///
	write4 write5 write6 write7 write8 ///
	num4 num5 num6 num7 num8 ///
	cal4 cal5 cal6 cal7 cal8 ///
	space4 space5 space6 space7 space8

* Knowledge and understanding / physical / creative
tetrachoric ///
	know4 know5 know6 know7 know8 ///
	phy4  phy5  phy6  phy7  phy8  ///
	cre4  cre5  cre6  cre7  cre8


********************************************************************************
* SECTION 3 — HIGH-CORRELATION ITEM DETECTION
*
* Identify pairs of items with tetrachoric correlation >= threshold.
* Two thresholds applied: 0.90 and 0.85.
* Items flagged at 0.85 are candidates for dropping before factor analysis.
*
* NOTE: the drop_list macro accumulates both members of each flagged pair,
* so items appear multiple times if they correlate highly with several others.
********************************************************************************

* Full item set for correlation screening
local varlist ///
	att4 att5 att6 att7 att8 ///
	soc4 soc5 soc6 soc7 soc8 ///
	emo4 emo5 emo6 emo7 emo8 ///
	lang4 lang5 lang6 lang7 lang8 ///
	link4 link5 link6 link7 link8 ///
	read4 read5 read6 read7 read8 ///
	write4 write5 write6 write7 write8 ///
	num4 num5 num6 num7 num8 ///
	cal4 cal5 cal6 cal7 cal8 ///
	space4 space5 space6 space7 space8

tetrachoric `varlist'
matrix corr = r(Rho)
matlist corr

*------------------------------------------------------------------------------
* 3.1  Threshold: 0.90
*------------------------------------------------------------------------------
foreach threshold in 0.90 0.85 {

	local drop_list ""
	local i = 1

	foreach var1 of local varlist {
		local j = 1
		foreach var2 of local varlist {
			if "`var1'" != "`var2'" {
				local corr_val = corr[`i', `j']
				if abs(`corr_val') >= `threshold' {
					local drop_list "`drop_list' `var1'"
					local drop_list "`drop_list' `var2'"
				}
			}
			local j = `j' + 1
		}
		local i = `i' + 1
	}

	display "Pairs with |r| >= `threshold':"
	display "`drop_list'"
}

*------------------------------------------------------------------------------
* Summary of items to drop at 0.85 threshold and items retained
*
* Drop:  soc8 link5 link7 link8 write8 num6 num7 cal5 cal8
*
* Pairs driving these exclusions:
*   soc7–soc8 | link5–link6 | link5–link7 | link6–link7 | link7–link8
*   link7–read6 | link8–read6 | link8–write7 | link8–write8
*   write7–write8 | num4–num6 | num5–num6 | num5–num7 | num6–num7
*   num8–cal8 | cal4–cal5 | cal5–cal6 | cal8–space8
*
* Items retained for factor analysis:
*   att4  att5  att6  att7  att8
*   soc4  soc5  soc6  soc7
*   emo4  emo5  emo6  emo7  emo8
*   lang4 lang5 lang6 lang7 lang8
*   link4 link6
*   read4 read5 read6 read7 read8
*   write4 write5 write6 write7
*   num4  num5  num8
*   cal4  cal6  cal7  cal8
*   space4 space5 space6 space7
*------------------------------------------------------------------------------

********************************************************************************
* END OF FILE
********************************************************************************
