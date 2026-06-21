********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 04_ks2_clean.do
* Purpose : Import and clean KS2 examination results and teacher assessments;
*           impute missing test marks from point score categories; reshape to
*           wide format; merge exam and pupil-level files into a single
*           cross-sectional dataset.
*
* Inputs  : KS2Exam.csv          (subject-level test results, long format)
*           KS2Pupil.csv         (pupil-level teacher assessments and KS1 APS)
*           eyfsp_clean.dta      (cohort)
*
* Output  : ks2_clean.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — KS2 EXAMINATION RESULTS (long format → wide)
********************************************************************************

import delimited "file_path\KS2Exam.csv", clear

*------------------------------------------------------------------------------
* 1.1  Pupil identifiers
*------------------------------------------------------------------------------
rename ks2_pupilmatchingrefanonymous id_dfe
rename mojuid                        id_moj

*------------------------------------------------------------------------------
* 1.2  Merge cohort from EYFSP; retain matched and EYFSP-only records
*      (_merge == 1 are KS2 records with no EYFSP match — dropped below)
*------------------------------------------------------------------------------
merge m:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
drop if _merge == 1
drop _merge

*------------------------------------------------------------------------------
* 1.3  Subject codes
*
*   9980  Grammar, Punctuation & Spelling (KS2 test)
*   9981  English (KS2 test)
*   9982  Mathematics (KS2 test)
*   9983  Science (KS2 test)
*   9984  Reading (KS2 test)
*   9985  Writing (KS2 test)
*
* Retain test codes only (9980–9985).
*------------------------------------------------------------------------------
tab ks2_leapcode
keep if ks2_leapcode >= 9980 & ks2_leapcode <= 9985

*------------------------------------------------------------------------------
* 1.4  KS2 point score — impute from test level where mark is missing
*
*   Level B / 2 / N → 15 points
*   Level 3         → 21 points
*   Level 4         → 27 points
*   Level 5         → 33 points
*   Level 6         → 39 points
*------------------------------------------------------------------------------
tab ks2_testlev

replace ks2_points = 15 if ks2_points == . & inlist(ks2_testlev, "B", "2", "N")
replace ks2_points = 21 if ks2_points == . & ks2_testlev == "3"
replace ks2_points = 27 if ks2_points == . & ks2_testlev == "4"
replace ks2_points = 33 if ks2_points == . & ks2_testlev == "5"
replace ks2_points = 39 if ks2_points == . & ks2_testlev == "6"

*------------------------------------------------------------------------------
* 1.5  Drop records with no usable information
*      A record is uninformative if all of: test mark, test level, fine grade,
*      and points are missing or zero.
*------------------------------------------------------------------------------
drop if ks2_testmark == . & ks2_testlev == "" & ks2_fine == . ///
      & (ks2_points == . | ks2_points == 0)

*------------------------------------------------------------------------------
* 1.6  Numeric level variable
*      Recode "B" (below) and "N" (did not achieve) to level 2 before destring.
*------------------------------------------------------------------------------
gen ks2_lev = ks2_testlev
replace ks2_lev = "2" if inlist(ks2_testlev, "B", "N")
destring ks2_lev, replace force

*------------------------------------------------------------------------------
* 1.7  Restrict to cohort-matched academic years
*      Cohort 1: KS2 in 2012/2013
*      Cohort 2: KS2 in 2013/2014
*      Cohort 3: KS2 in 2014/2015
*------------------------------------------------------------------------------
tab ks2_acadyr
keep if (ks2_acadyr == "2012/2013" & cohort == 1) ///
      | (ks2_acadyr == "2013/2014" & cohort == 2) ///
      | (ks2_acadyr == "2014/2015" & cohort == 3)

*------------------------------------------------------------------------------
* 1.8  Keep variables needed for reshape
*------------------------------------------------------------------------------
keep id_dfe id_moj ks2_acadyr cohort ks2_leapcode ///
     ks2_testmark ks2_lev ks2_fine ks2_points

*------------------------------------------------------------------------------
* 1.9  Deduplicate before reshape (keep first record per pupil-subject)
*------------------------------------------------------------------------------
sort id_dfe ks2_leapcode
bys id_dfe ks2_leapcode: gen n = _n
tab n
keep if n == 1
drop n

*------------------------------------------------------------------------------
* 1.10  Reshape long → wide (one row per pupil, columns per subject)
*------------------------------------------------------------------------------
reshape wide ks2_testmark ks2_lev ks2_fine ks2_points, i(id_dfe) j(ks2_leapcode)

* Rename to subject labels
rename (ks2_testmark9980 ks2_lev9980 ks2_fine9980 ks2_points9980) ///
       (ks2_testmark_gps  ks2_lev_gps  ks2_fine_gps  ks2_points_gps)
rename (ks2_testmark9982 ks2_lev9982 ks2_fine9982 ks2_points9982) ///
       (ks2_testmark_math ks2_lev_math ks2_fine_math ks2_points_math)
rename (ks2_testmark9984 ks2_lev9984 ks2_fine9984 ks2_points9984) ///
       (ks2_testmark_read ks2_lev_read ks2_fine_read ks2_points_read)
	   
*------------------------------------------------------------------------------
* 1.11  Impute missing test marks from mean mark within point score category
*       Applies to GPS, maths, and reading (the three main KS2 tests).
*------------------------------------------------------------------------------
summ ks2*
count if ks2_testmark_gps  == . & ks2_points_gps  != .
count if ks2_testmark_math == . & ks2_points_math != .
count if ks2_testmark_read == . & ks2_points_read != .

foreach i of numlist 0 15 21 27 33 39 {

	summ ks2_testmark_gps if ks2_points_gps == `i'
	replace ks2_testmark_gps  = r(mean) if ks2_testmark_gps  == . & ks2_points_gps  == `i'

	summ ks2_testmark_math if ks2_points_math == `i'
	replace ks2_testmark_math = r(mean) if ks2_testmark_math == . & ks2_points_math == `i'

	summ ks2_testmark_read if ks2_points_read == `i'
	replace ks2_testmark_read = r(mean) if ks2_testmark_read == . & ks2_points_read == `i'
}

mdesc

keep id_dfe ks2_acadyr ks2_testmark* ks2_lev* ks2_points*

tempfile ks2_exams
save `ks2_exams', replace


********************************************************************************
* SECTION 2 — KS2 TEACHER ASSESSMENTS AND PUPIL-LEVEL FILE
********************************************************************************

import delimited "file_path\KS2Pupil.csv", clear

*------------------------------------------------------------------------------
* 2.1  Pupil identifier
*------------------------------------------------------------------------------
codebook ks2_pupilmatchingrefanonymous
rename ks2_pupilmatchingrefanonymous id_dfe

*------------------------------------------------------------------------------
* 2.2  Deduplicate: one record per pupil (keep earliest academic year)
*------------------------------------------------------------------------------
sort id_dfe ks2_acadyr
bys id_dfe: gen n = _n
tab n
keep if n == 1
drop n

*------------------------------------------------------------------------------
* 2.3  School and prior attainment
*------------------------------------------------------------------------------
rename ks2_msch   mschool
tab mschool

rename ks2_ks1aps ks1_score
summ ks1_score

rename ks2_aps    ks2_score_ta     // APS from reading, maths, writing TA
tab ks2_score_ta

*------------------------------------------------------------------------------
* 2.4  Subject-level level 4 attainment indicators
*------------------------------------------------------------------------------

* Reading
tab ks2_readmrk
tab ks2_readlev
tab ks2_levxread
gen ks2_read4 = ks2_levxread
tab ks2_read4

* Maths
tab ks2_mattotmrk
tab ks2_levxmat
gen ks2_maths4 = ks2_levxmat
tab ks2_maths4

* Grammar, Punctuation & Spelling
tab ks2_levxgps
gen ks2_gps4 = ks2_levxgps
tab ks2_gps4

* Descriptive checks
bys ks2_read4:  tab ks2_score_ta
bys ks2_maths4: tab ks2_score_ta
bys ks2_gps4:   tab ks2_score_ta

count if  ks2_read4 == 0 & ks2_maths4 == 0 & ks2_gps4 == 0
count if (ks2_read4 == 0 | ks2_maths4 == 0 | ks2_gps4 == 0)
summ ks2_maths4 ks2_read4 ks2_gps4

* Unable to access test or absent
count if ks2_maths4 == 0 & ks2_levatmat  == 1
count if ks2_read4  == 0 & ks2_levatread == 1
count if ks2_gps4   == 0 & ks2_levatgps  == 1

*------------------------------------------------------------------------------
* 2.5  Overall level 4 indicator
*      = 1 if level 4 achieved in all three subjects
*      = 0 if below level 4 in any subject (and not due to absence/access)
*      NOTE: missing if any subject missing — pupils absent or unable to access
*      test are not coded as failures.
*------------------------------------------------------------------------------
gen ks2_level4 = .
replace ks2_level4 = 1 if ks2_maths4 == 1 & ks2_read4 == 1 & ks2_gps4 == 1
replace ks2_level4 = 0 if ks2_maths4 == 0 & ks2_levatmat  == 0
replace ks2_level4 = 0 if ks2_read4  == 0 & ks2_levatread == 0
replace ks2_level4 = 0 if ks2_gps4   == 0 & ks2_levatgps  == 0
tab ks2_level4

*------------------------------------------------------------------------------
* 2.6  Keep variables of interest
*------------------------------------------------------------------------------
keep id_dfe mschool ks1_score ks2_score_ta ///
     ks2_read4 ks2_maths4 ks2_gps4 ks2_level4
compress


********************************************************************************
* SECTION 3 — MERGE PUPIL FILE WITH EYFSP AND EXAM RESULTS
********************************************************************************

merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
drop if _merge == 1
drop _merge

merge 1:1 id_dfe using `ks2_exams'
drop if _merge == 1

bys cohort: tab ks2_acadyr if _merge == 3
drop _merge cohort

mdesc
compress

save "file_path\ks2_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
