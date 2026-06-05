********************************************************************************
* Project : Socio-Emotional Characteristics in Early Childhood and
*           Offending Behaviour in Adolescence
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 02_eyfsp_clean.do
* Purpose : Import and clean the Early Years Foundation Stage Profile (EYFSP)
*           data (academic years 2006/07–2008/09).  Converts string scores to
*           numeric, recovers subscale totals from individual items where the
*           reported total is missing, and constructs the overall EYFSP score.
*
* Input   : EYFSP_2007_to_2009.csv
*
* Output  : eyfsp_clean.dta
*
* Subscale structure (all scored 0–9 unless noted):
*   Personal, Social and Emotional Development (PSE, 0–27)
*     as1  Dispositions and Attitudes       → sc_attitude  (att1–att9)
*     as2  Social Development               → sc_social    (soc1–soc9)
*     as3  Emotional Development            → sc_emotion   (emo1–emo9)
*   Communication, Language and Literacy (CLL, 0–36)
*     as1  Language and Thinking            → sc_language  (lang1–lang9)
*     as2  Linking Sounds and Letters       → sc_link      (link1–link9)
*     as3  Reading                          → sc_read      (read1–read9)
*     as4  Writing                          → sc_write     (write1–write9)
*   Problem Solving, Reasoning and Numeracy (PSRN, 0–27)
*     as1  Numbers as Labels / for Counting → sc_numbers   (num1–num9)
*     as2  Calculating                      → sc_calculating (cal1–cal9)
*     as3  Shape, Space and Measures        → sc_space     (space1–space9)
*   Knowledge and Understanding of the World → sc_knowledge (know1–know9)
*   Physical Development                     → sc_physical  (phy1–phy9)
*   Creative Development                     → sc_creative  (cre1–cre9)
*   Overall EYFSP total (0–117)              → score_eyfsp_overall
*
* Notes   : String-to-numeric conversion uses a foreach loop over the valid
*           score range; individual items are "F"/"T" recoded to 0/1.
*           Where the reported total is missing but all items are present,
*           the total is recovered by summation; a test variable confirms
*           consistency between reported and computed totals.
********************************************************************************

clear all
set more off

import delimited "file_path\EYFSP_2007_to_2009.csv"


********************************************************************************
* SECTION 1 — IDENTIFIERS AND COHORT
********************************************************************************

rename fsp_pupilmatchingrefanonymous id_dfe
rename mojuid                         id_moj

* Cohort (academic year of EYFSP assessment)
gen cohort = .
replace cohort = 1 if fsp_acadyr == "2006/2007"
replace cohort = 2 if fsp_acadyr == "2007/2008"
replace cohort = 3 if fsp_acadyr == "2008/2009"
tab cohort


********************************************************************************
* SECTION 2 — DUPLICATES
*
* A small number of pupils appear more than once (≈0.03% of the sample).
* We retain the first record per id_dfe.
********************************************************************************

bys id_dfe: gen n = _n
count if n != 1 & fsp_pseas1r1 != ""   // Flag: duplicates with EYFSP item data
keep if n == 1
drop n
tab cohort


********************************************************************************
* SECTION 3 — DEMOGRAPHICS
********************************************************************************

* Sex
gen female = .
replace female = 0 if inlist(fsp_gender, "M", "m")
replace female = 1 if inlist(fsp_gender, "F", "f")
tab female

* Age at school reception
gen age_EYFSP = fsp_age_start
tab age_EYFSP

* Local Authority of school
rename fsp_la LA_EYFSP

* LSOA of household
count if fsp_lsoa01 == ""   // Flag: missing household LSOA

* School identifier
rename fsp_urn school_EYFSP


********************************************************************************
* SECTION 4 — SCORE CLEANING: HELPER PROGRAMME
*
* Each subscale follows the same pattern:
*   (a) Convert reported string total to numeric.
*   (b) Convert individual items (F/T strings) to binary (0/1).
*   (c) Count missing items per pupil.
*   (d) Where total is missing but all items present, recover total by summation.
*   (e) Verify: computed sum matches reported total wherever both exist.
*
* A programme is defined once and called for each subscale to avoid repetition.
*
* Arguments:
*   stub    — item stub name (e.g. "att" → att1 … att9)
*   srcvar  — raw string total variable (e.g. fsp_pse_as1)
*   outvar  — cleaned numeric total variable (e.g. sc_attitude)
*   srcpfx  — varlist prefix for raw item variables (e.g. fsp_pseas1r)
*   maxscore— maximum valid score for the total (default 9 for subscales)
********************************************************************************

capture program drop clean_subscale
program define clean_subscale
    args stub srcvar outvar srcpfx maxscore

    * (a) String total → numeric
    gen `outvar' = .
    foreach i of numlist 0/`maxscore' {
        qui replace `outvar' = `i' if `srcvar' == "`i'"
    }
    count if `outvar' == .

    * (b) Individual items: F/f → 0, T/t → 1
    local i = 1
    foreach var of varlist `srcpfx'* {
        gen `stub'`i' = .
        replace `stub'`i' = 0 if inlist(`var', "F", "f")
        replace `stub'`i' = 1 if inlist(`var', "T", "t")
        local ++i
    }

    * (c) Count missing items
    egen miss_`stub' = rowmiss(`stub'*)
    tab miss_`stub'
    bys cohort: tab miss_`stub'

    * (d) Recover total from items where total missing but items complete
    count if `outvar' == . & miss_`stub' == 0
    replace `outvar' = 0 if `outvar' == . & miss_`stub' == 0   // initialise for sum
    foreach j of numlist 1/9 {
        replace `outvar' = `outvar' + `stub'`j' if miss_`stub' == 0 & `outvar' != .
    }
    * Cleaner alternative using egen (produces same result, left as reference):
    * egen `outvar'_tmp = rowtotal(`stub'*) if miss_`stub' == 0
    * replace `outvar' = `outvar'_tmp if `outvar' == . & miss_`stub' == 0
    * drop `outvar'_tmp
    tab `outvar'

    * (e) Consistency check: sum of items vs reported total
    egen `outvar'_test = rowtotal(`stub'*)
    count if `outvar'_test != `outvar' & `outvar'_test != .
    drop `outvar'_test

end


********************************************************************************
* SECTION 5 — PERSONAL, SOCIAL AND EMOTIONAL DEVELOPMENT (PSE, 0–27)
********************************************************************************

* Domain total (string → numeric)
gen score_pse_total = .
foreach i of numlist 0/27 {
    qui replace score_pse_total = `i' if fsp_pse_total == "`i'"
}
tab score_pse_total
tab cohort if score_pse_total == .

* Subscales
clean_subscale att  fsp_pse_as1 sc_attitude   fsp_pseas1r  9
clean_subscale soc  fsp_pse_as2 sc_social     fsp_pseas2r  9
clean_subscale emo  fsp_pse_as3 sc_emotion    fsp_pseas3r  9

* Recover PSE domain total from subscales where missing
count if score_pse_total == . & !missing(sc_attitude, sc_social, sc_emotion)
replace score_pse_total = sc_attitude + sc_social + sc_emotion ///
    if score_pse_total == . & !missing(sc_attitude, sc_social, sc_emotion)
tab score_pse_total


********************************************************************************
* SECTION 6 — COMMUNICATION, LANGUAGE AND LITERACY (CLL, 0–36)
********************************************************************************

gen score_cll_total = .
foreach i of numlist 0/36 {
    qui replace score_cll_total = `i' if fsp_cll_total == "`i'"
}
tab score_cll_total
count if score_cll_total == .

clean_subscale lang  fsp_cll_as1 sc_language  fsp_cllas1r  9
clean_subscale link  fsp_cll_as2 sc_link      fsp_cllas2r  9
clean_subscale read  fsp_cll_as3 sc_read      fsp_cllas3r  9
clean_subscale write fsp_cll_as4 sc_write     fsp_cllas4r  9

count if score_cll_total == . & !missing(sc_language, sc_link, sc_read, sc_write)
replace score_cll_total = sc_language + sc_link + sc_read + sc_write ///
    if score_cll_total == . & !missing(sc_language, sc_link, sc_read, sc_write)
tab score_cll_total


********************************************************************************
* SECTION 7 — PROBLEM SOLVING, REASONING AND NUMERACY (PSRN, 0–27)
********************************************************************************

gen score_psrn_total = .
foreach i of numlist 0/27 {
    qui replace score_psrn_total = `i' if fsp_psrn_total == "`i'"
}
tab score_psrn_total
count if score_psrn_total == .

* BUG FIX: original code used sc_num and sc_cal (non-existent variables) in the
* imputation conditions; corrected to sc_numbers and sc_calculating respectively.
clean_subscale num   fsp_psrn_as1 sc_numbers     fsp_psrnas1r  9
clean_subscale cal   fsp_psrn_as2 sc_calculating fsp_psrnas2r  9
clean_subscale space fsp_psrn_as3 sc_space       fsp_psrnas3r  9

count if score_psrn_total == . & !missing(sc_numbers, sc_calculating, sc_space)
replace score_psrn_total = sc_numbers + sc_calculating + sc_space ///
    if score_psrn_total == . & !missing(sc_numbers, sc_calculating, sc_space)
tab score_psrn_total


********************************************************************************
* SECTION 8 — SINGLE-SCALE DOMAINS
********************************************************************************

clean_subscale know fsp_rkuw  sc_knowledge fsp_rkuwr  9
clean_subscale phy  fsp_ripd  sc_physical  fsp_ripdr  9
clean_subscale cre  fsp_ricd  sc_creative  fsp_ricdr  9


********************************************************************************
* SECTION 9 — OVERALL EYFSP TOTAL (0–117)
********************************************************************************

gen score_eyfsp_overall = .
foreach i of numlist 0/117 {
    qui replace score_eyfsp_overall = `i' if fsp_eyfsp_total == "`i'"
}

* Recover from domain totals where missing and all domains present
count if score_eyfsp_overall == . & ///
    !missing(score_pse_total, score_cll_total, score_psrn_total, ///
             sc_physical, sc_knowledge, sc_creative)

replace score_eyfsp_overall = score_pse_total + score_cll_total + score_psrn_total ///
    + sc_physical + sc_knowledge + sc_creative ///
    if score_eyfsp_overall == . & ///
    !missing(score_pse_total, score_cll_total, score_psrn_total, ///
             sc_physical, sc_knowledge, sc_creative)
tab score_eyfsp_overall


********************************************************************************
* SECTION 10 — MISSINGNESS SUMMARY AND SAVE
********************************************************************************

* Overall item-level missingness across the three main scored domains
* (PSE, CLL, PSRN); physical, knowledge and creative excluded as single scales
* without sub-item imputation in downstream factor models
egen missing_EYFSP = rowmiss(att* soc* emo* lang* link* read* write* num* cal* space*)
tab missing_EYFSP

keep id* LA_EYFSP school_EYFSP cohort female age_EYFSP ///
     score_* sc_* ///
     att* soc* emo* lang* link* read* write* num* cal* space* ///
     know* phy* cre* missing_EYFSP

compress
save "file_path\eyfsp_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
