********************************************************************************
* Project: Socio-Emotional Characteristics in Early Childhood and Offending
*          Behaviour in Adolescence
* Author:  Paul Garcia
* Institution: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
* Description: Cleans and constructs variables from the Early Years Foundation
*              Stage Profile (EYFSP) dataset (2007-2009 cohorts). For each of
*              the six EYFSP learning areas, the script: (i) converts string
*              total scores to numeric, (ii) generates binary individual items
*              from T/F responses, (iii) recovers total scores from individual
*              items where the total is missing, and (iv) validates totals
*              against the sum of individual items.
********************************************************************************

clear all
import delimited "file_path\EYFSP_2007_to_2009.csv"

********************************************************************************
* PART 1: Identifiers and cohort
********************************************************************************

* Pupil matching reference (anonymised DfE identifier)
rename fsp_pupilmatchingrefanonymous id_dfe

* MoJ anonymised identifier (for linking to offending records)
rename mojuid id_moj

* Academic year maps to cohort: 1 = 2006/07, 2 = 2007/08, 3 = 2008/09
tab fsp_acadyr, nolab
gen cohort = .
replace cohort = 1 if fsp_acadyr == "2006/2007"
replace cohort = 2 if fsp_acadyr == "2007/2008"
replace cohort = 3 if fsp_acadyr == "2008/2009"

* Check for duplicate pupil records
* Keep only the first record per pupil (n==1); flag cases with non-missing EYFSP data
bys id_dfe: gen n = _n
tab n
count if n != 1 & fsp_pseas1r1 != ""   // how many duplicates have actual EYFSP data
keep if n == 1
tab cohort

********************************************************************************
* PART 2: Basic demographics
********************************************************************************

* Sex: generate binary female indicator from string gender variable
* Handles both upper and lower case entries
tab fsp_gender, nolab
gen female = .
replace female = 0 if fsp_gender == "M" | fsp_gender == "m"
replace female = 1 if fsp_gender == "F" | fsp_gender == "f"
tab female

* Age at school reception (used to control for season of birth effects)
bys cohort: tab fsp_age_start, nolab
gen age_EYFSP = fsp_age_start
tab age_EYFSP

* Local Authority of school at EYFSP assessment
codebook fsp_la
rename fsp_la LA_EYFSP

* Lower Super Output Area of household (residential location)
codebook fsp_lsoa01
count if fsp_lsoa01 == ""

* School URN (unique school identifier at time of EYFSP)
codebook fsp_urn
rename fsp_urn school_EYFSP

********************************************************************************
* PART 3: PERSONAL, SOCIAL AND EMOTIONAL DEVELOPMENT (PSE)
* Three sub-scales: Dispositions & Attitudes, Social Development, Emotional Dev.
* Each sub-scale has 9 binary items (T/F) scored 0-9; PSE total scored 0-27
********************************************************************************

* Overall PSE total score (string to numeric conversion)
* Raw total stored as string so loop converts each value individually
tab fsp_pse_total
gen score_pse_total = .
foreach i of numlist 0/27 {
    qui replace score_pse_total = `i' if fsp_pse_total == "`i'"
}
tab score_pse_total
tab cohort if score_pse_total == .   // missingness by cohort

*------------------------------------------------------------------------------
* Sub-scale 1: Dispositions and Attitudes (att)
*------------------------------------------------------------------------------

* Sub-scale total score (string to numeric)
tab fsp_pse_as1
gen sc_attitude = .
foreach i of numlist 0/9 {
    replace sc_attitude = `i' if fsp_pse_as1 == "`i'"
}
tab sc_attitude
count if sc_attitude == .
count if sc_attitude == . & score_pse_total == .   // missing both total and sub-scale

* Individual binary items: T=1 (achieved), F=0 (not achieved)
* Variables named att1-att9, looping over fsp_pseas1r* in order
local i = 1
foreach var of varlist fsp_pseas1r* {
    tab `var'
    gen att`i' = .
    replace att`i' = 0 if `var' == "F" | `var' == "f"
    replace att`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

* Count missing individual items per pupil
egen miss_att = rowmiss(att*)
tab miss_att
bys cohort: tab miss_att

* Recover sub-scale total from individual items where total is missing but all items present
count if sc_attitude == . & miss_att == 0
replace sc_attitude = att1+att2+att3+att4+att5+att6+att7+att8+att9 ///
    if sc_attitude == . & miss_att == 0
tab sc_attitude

* Validation: confirm sum of items equals reported total (should be zero mismatches)
gen sc_attitude_test = att1+att2+att3+att4+att5+att6+att7+att8+att9
count if sc_attitude_test != sc_attitude & sc_attitude_test != .

* Diagnostic counts
count if sc_attitude != . & miss_att != 0   // total present but some items missing
count if sc_attitude != . & miss_att == 0   // total present and all items present

*------------------------------------------------------------------------------
* Sub-scale 2: Social Development (soc)
*------------------------------------------------------------------------------

tab fsp_pse_as2
gen sc_social = .
foreach i of numlist 0/9 {
    replace sc_social = `i' if fsp_pse_as2 == "`i'"
}
tab sc_social
count if sc_social == .

local i = 1
foreach var of varlist fsp_pseas2r* {
    tab `var'
    gen soc`i' = .
    replace soc`i' = 0 if `var' == "F" | `var' == "f"
    replace soc`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_soc = rowmiss(soc*)
tab miss_soc
bys cohort: tab miss_soc

count if sc_social == . & miss_soc == 0
replace sc_social = soc1+soc2+soc3+soc4+soc5+soc6+soc7+soc8+soc9 ///
    if sc_social == . & miss_soc == 0
tab sc_social

gen sc_social_test = soc1+soc2+soc3+soc4+soc5+soc6+soc7+soc8+soc9
count if sc_social_test != sc_social & sc_social_test != .

count if sc_social != . & miss_soc != 0
count if sc_social != . & miss_soc == 0

*------------------------------------------------------------------------------
* Sub-scale 3: Emotional Development (emo)
*------------------------------------------------------------------------------

tab fsp_pse_as3
gen sc_emotion = .
foreach i of numlist 0/9 {
    replace sc_emotion = `i' if fsp_pse_as3 == "`i'"
}
tab sc_emotion
count if sc_emotion == .

local i = 1
foreach var of varlist fsp_pseas3r* {
    tab `var'
    gen emo`i' = .
    replace emo`i' = 0 if `var' == "F" | `var' == "f"
    replace emo`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_emo = rowmiss(emo*)
tab miss_emo
bys cohort: tab miss_emo

count if sc_emotion == . & miss_emo == 0
replace sc_emotion = emo1+emo2+emo3+emo4+emo5+emo6+emo7+emo8+emo9 ///
    if sc_emotion == . & miss_emo == 0
tab sc_emotion

gen sc_emotion_test = emo1+emo2+emo3+emo4+emo5+emo6+emo7+emo8+emo9
count if sc_emotion_test != sc_emotion & sc_emotion_test != .

count if sc_emotion != . & miss_emo != 0
count if sc_emotion != . & miss_emo == 0

* Recover PSE overall total where sub-scales are all present but total is missing
count if score_pse_total == . & (sc_attitude != . & sc_social != . & sc_emotion != .)
replace score_pse_total = sc_attitude + sc_social + sc_emotion if score_pse_total == .
tab score_pse_total

********************************************************************************
* PART 4: COMMUNICATION, LANGUAGE AND LITERACY (CLL)
* Four sub-scales: Language & Thinking, Linking Sounds & Letters, Reading, Writing
* Each sub-scale scored 0-9; CLL total scored 0-36
********************************************************************************

tab fsp_cll_total
gen score_cll_total = .
foreach i of numlist 0/36 {
    qui replace score_cll_total = `i' if fsp_cll_total == "`i'"
}
tab score_cll_total
count if score_cll_total == .

*------------------------------------------------------------------------------
* Sub-scale 1: Language and Thinking (lang)
*------------------------------------------------------------------------------

tab fsp_cll_as1
gen sc_language = .
foreach i of numlist 0/9 {
    replace sc_language = `i' if fsp_cll_as1 == "`i'"
}
tab sc_language
count if sc_language == .

local i = 1
foreach var of varlist fsp_cllas1r* {
    tab `var'
    gen lang`i' = .
    replace lang`i' = 0 if `var' == "F" | `var' == "f"
    replace lang`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_lang = rowmiss(lang*)
tab miss_lang
bys cohort: tab miss_lang

count if sc_language == . & miss_lang == 0
replace sc_language = lang1+lang2+lang3+lang4+lang5+lang6+lang7+lang8+lang9 ///
    if sc_language == . & miss_lang == 0
tab sc_language

gen sc_language_test = lang1+lang2+lang3+lang4+lang5+lang6+lang7+lang8+lang9
count if sc_language_test != sc_language & sc_language_test != .

count if sc_language != . & miss_lang != 0
count if sc_language != . & miss_lang == 0

*------------------------------------------------------------------------------
* Sub-scale 2: Linking Sounds and Letters (link)
*------------------------------------------------------------------------------

tab fsp_cll_as2
gen sc_link = .
foreach i of numlist 0/9 {
    replace sc_link = `i' if fsp_cll_as2 == "`i'"
}
tab sc_link
count if sc_link == .

local i = 1
foreach var of varlist fsp_cllas2r* {
    tab `var'
    gen link`i' = .
    replace link`i' = 0 if `var' == "F" | `var' == "f"
    replace link`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_link = rowmiss(link*)
tab miss_link
bys cohort: tab miss_link

count if sc_link == . & miss_link == 0
replace sc_link = link1+link2+link3+link4+link5+link6+link7+link8+link9 ///
    if sc_link == . & miss_link == 0
tab sc_link

gen sc_link_test = link1+link2+link3+link4+link5+link6+link7+link8+link9
count if sc_link_test != sc_link & sc_link_test != .

count if sc_link != . & miss_link != 0
count if sc_link != . & miss_link == 0

*------------------------------------------------------------------------------
* Sub-scale 3: Reading (read)
*------------------------------------------------------------------------------

tab fsp_cll_as3
gen sc_read = .
foreach i of numlist 0/9 {
    replace sc_read = `i' if fsp_cll_as3 == "`i'"
}
tab sc_read
count if sc_read == .

local i = 1
foreach var of varlist fsp_cllas3r* {
    tab `var'
    gen read`i' = .
    replace read`i' = 0 if `var' == "F" | `var' == "f"
    replace read`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_read = rowmiss(read*)
tab miss_read
bys cohort: tab miss_read

count if sc_read == . & miss_read == 0
replace sc_read = read1+read2+read3+read4+read5+read6+read7+read8+read9 ///
    if sc_read == . & miss_read == 0
tab sc_read

gen sc_read_test = read1+read2+read3+read4+read5+read6+read7+read8+read9
count if sc_read_test != sc_read & sc_read_test != .

count if sc_read != . & miss_read != 0
count if sc_read != . & miss_read == 0

*------------------------------------------------------------------------------
* Sub-scale 4: Writing (write)
*------------------------------------------------------------------------------

tab fsp_cll_as4
gen sc_write = .
foreach i of numlist 0/9 {
    replace sc_write = `i' if fsp_cll_as4 == "`i'"
}
tab sc_write
count if sc_write == .

local i = 1
foreach var of varlist fsp_cllas4r* {
    tab `var'
    gen write`i' = .
    replace write`i' = 0 if `var' == "F" | `var' == "f"
    replace write`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_write = rowmiss(write*)
tab miss_write
bys cohort: tab miss_write

count if sc_write == . & miss_write == 0
replace sc_write = write1+write2+write3+write4+write5+write6+write7+write8+write9 ///
    if sc_write == . & miss_write == 0
tab sc_write

gen sc_write_test = write1+write2+write3+write4+write5+write6+write7+write8+write9
count if sc_write_test != sc_write & sc_write_test != .

count if sc_write != . & miss_write != 0
count if sc_write != . & miss_write == 0

* Recover CLL total from sub-scales where total is missing
count if score_cll_total == . & (sc_language != . & sc_link != . & sc_read != . & sc_write != .)
replace score_cll_total = sc_language + sc_link + sc_read + sc_write if score_cll_total == .
tab score_cll_total

********************************************************************************
* PART 5: PROBLEM SOLVING, REASONING AND NUMERACY (PSRN)
* Three sub-scales: Numbers, Calculating, Shape/Space/Measures
* Each sub-scale scored 0-9; PSRN total scored 0-27
********************************************************************************

tab fsp_psrn_total
gen score_psrn_total = .
foreach i of numlist 0/27 {
    qui replace score_psrn_total = `i' if fsp_psrn_total == "`i'"
}
tab score_psrn_total
count if score_psrn_total == .

*------------------------------------------------------------------------------
* Sub-scale 1: Numbers as Labels and for Counting (num)
*------------------------------------------------------------------------------

tab fsp_psrn_as1
gen sc_numbers = .
foreach i of numlist 0/9 {
    replace sc_numbers = `i' if fsp_psrn_as1 == "`i'"
}
tab sc_numbers
count if sc_numbers == .

local i = 1
foreach var of varlist fsp_psrnas1r* {
    tab `var'
    gen num`i' = .
    replace num`i' = 0 if `var' == "F" | `var' == "f"
    replace num`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_num = rowmiss(num*)
tab miss_num
bys cohort: tab miss_num

count if sc_numbers == . & miss_num == 0
* BUG FIX: original used sc_num (undefined) instead of sc_numbers
replace sc_numbers = num1+num2+num3+num4+num5+num6+num7+num8+num9 ///
    if sc_numbers == . & miss_num == 0
tab sc_numbers

gen sc_numbers_test = num1+num2+num3+num4+num5+num6+num7+num8+num9
count if sc_numbers_test != sc_numbers & sc_numbers_test != .

count if sc_numbers != . & miss_num != 0
count if sc_numbers != . & miss_num == 0

*------------------------------------------------------------------------------
* Sub-scale 2: Calculating (cal)
*------------------------------------------------------------------------------

tab fsp_psrn_as2
gen sc_calculating = .
foreach i of numlist 0/9 {
    replace sc_calculating = `i' if fsp_psrn_as2 == "`i'"
}
tab sc_calculating
count if sc_calculating == .

local i = 1
foreach var of varlist fsp_psrnas2r* {
    tab `var'
    gen cal`i' = .
    replace cal`i' = 0 if `var' == "F" | `var' == "f"
    replace cal`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_cal = rowmiss(cal*)
tab miss_cal
bys cohort: tab miss_cal

count if sc_calculating == . & miss_cal == 0
* BUG FIX: original used sc_cal (undefined) instead of sc_calculating
replace sc_calculating = cal1+cal2+cal3+cal4+cal5+cal6+cal7+cal8+cal9 ///
    if sc_calculating == . & miss_cal == 0
tab sc_calculating

gen sc_calculating_test = cal1+cal2+cal3+cal4+cal5+cal6+cal7+cal8+cal9
count if sc_calculating_test != sc_calculating & sc_calculating_test != .

count if sc_calculating != . & miss_cal != 0
count if sc_calculating != . & miss_cal == 0

*------------------------------------------------------------------------------
* Sub-scale 3: Shape, Space and Measures (space)
*------------------------------------------------------------------------------

tab fsp_psrn_as3
gen sc_space = .
foreach i of numlist 0/9 {
    replace sc_space = `i' if fsp_psrn_as3 == "`i'"
}
tab sc_space
count if sc_space == .

local i = 1
foreach var of varlist fsp_psrnas3r* {
    tab `var'
    gen space`i' = .
    replace space`i' = 0 if `var' == "F" | `var' == "f"
    replace space`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_space = rowmiss(space*)
tab miss_space
bys cohort: tab miss_space

count if sc_space == . & miss_space == 0
replace sc_space = space1+space2+space3+space4+space5+space6+space7+space8+space9 ///
    if sc_space == . & miss_space == 0
tab sc_space

gen sc_space_test = space1+space2+space3+space4+space5+space6+space7+space8+space9
count if sc_space_test != sc_space & sc_space_test != .

count if sc_space != . & miss_space != 0
count if sc_space != . & miss_space == 0

* Recover PSRN total from sub-scales where total is missing
count if score_psrn_total == . & (sc_numbers != . & sc_calculating != . & sc_space != .)
replace score_psrn_total = sc_numbers + sc_calculating + sc_space if score_psrn_total == .
tab score_psrn_total

********************************************************************************
* PART 6: KNOWLEDGE AND UNDERSTANDING OF THE WORLD (KUW)
* Single scale, 9 binary items, scored 0-9
********************************************************************************

tab fsp_rkuw
gen sc_knowledge = .
foreach i of numlist 0/9 {
    replace sc_knowledge = `i' if fsp_rkuw == "`i'"
}
tab sc_knowledge
count if sc_knowledge == .

local i = 1
foreach var of varlist fsp_rkuwr* {
    tab `var'
    gen know`i' = .
    replace know`i' = 0 if `var' == "F" | `var' == "f"
    replace know`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_know = rowmiss(know*)
tab miss_know
bys cohort: tab miss_know

count if sc_knowledge == . & miss_know == 0
replace sc_knowledge = know1+know2+know3+know4+know5+know6+know7+know8+know9 ///
    if sc_knowledge == . & miss_know == 0
tab sc_knowledge

gen sc_knowledge_test = know1+know2+know3+know4+know5+know6+know7+know8+know9
count if sc_knowledge_test != sc_knowledge & sc_knowledge_test != .

count if sc_knowledge != . & miss_know != 0
count if sc_knowledge != . & miss_know == 0

********************************************************************************
* PART 7: PHYSICAL DEVELOPMENT (PD)
* Single scale, 9 binary items, scored 0-9
********************************************************************************

tab fsp_ripd
gen sc_physical = .
foreach i of numlist 0/9 {
    replace sc_physical = `i' if fsp_ripd == "`i'"
}
tab sc_physical
count if sc_physical == .

local i = 1
foreach var of varlist fsp_ripdr* {
    tab `var'
    gen phy`i' = .
    replace phy`i' = 0 if `var' == "F" | `var' == "f"
    replace phy`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_phy = rowmiss(phy*)
tab miss_phy
bys cohort: tab miss_phy

count if sc_physical == . & miss_phy == 0
replace sc_physical = phy1+phy2+phy3+phy4+phy5+phy6+phy7+phy8+phy9 ///
    if sc_physical == . & miss_phy == 0
tab sc_physical

gen sc_physical_test = phy1+phy2+phy3+phy4+phy5+phy6+phy7+phy8+phy9
count if sc_physical_test != sc_physical & sc_physical_test != .

count if sc_physical != . & miss_phy != 0
count if sc_physical != . & miss_phy == 0

********************************************************************************
* PART 8: CREATIVE DEVELOPMENT (CD)
* Single scale, 9 binary items, scored 0-9
********************************************************************************

tab fsp_ricd
gen sc_creative = .
foreach i of numlist 0/9 {
    replace sc_creative = `i' if fsp_ricd == "`i'"
}
tab sc_creative
count if sc_creative == .

local i = 1
foreach var of varlist fsp_ricdr* {
    tab `var'
    gen cre`i' = .
    replace cre`i' = 0 if `var' == "F" | `var' == "f"
    replace cre`i' = 1 if `var' == "T" | `var' == "t"
    local i = `i' + 1
}

egen miss_cre = rowmiss(cre*)
tab miss_cre
bys cohort: tab miss_cre

count if sc_creative == . & miss_cre == 0
replace sc_creative = cre1+cre2+cre3+cre4+cre5+cre6+cre7+cre8+cre9 ///
    if sc_creative == . & miss_cre == 0
tab sc_creative

gen sc_creative_test = cre1+cre2+cre3+cre4+cre5+cre6+cre7+cre8+cre9
count if sc_creative_test != sc_creative & sc_creative_test != .

count if sc_creative != . & miss_cre != 0
count if sc_creative != . & miss_cre == 0

********************************************************************************
* PART 9: OVERALL EYFSP TOTAL SCORE
* Sum of all six learning areas: PSE + CLL + PSRN + Physical + KUW + Creative
* Maximum possible score: 117
********************************************************************************

tab fsp_eyfsp_total
gen score_eyfsp_overall = .
foreach i of numlist 0/117 {
    qui replace score_eyfsp_overall = `i' if fsp_eyfsp_total == "`i'"
}
tab score_eyfsp_overall

* Recover overall total from learning area totals where missing
count if score_eyfsp_overall == . & ///
    (score_pse_total != . & score_cll_total != . & score_psrn_total != . & ///
     sc_physical != . & sc_knowledge != . & sc_creative != .)
replace score_eyfsp_overall = score_pse_total + score_cll_total + score_psrn_total ///
    + sc_physical + sc_knowledge + sc_creative if score_eyfsp_overall == .
tab score_eyfsp_overall

********************************************************************************
* PART 10: Final keep, missing data summary, and save
********************************************************************************

* Keep identifiers, demographics, all scores, and all binary items
* Note: space* and know* and phy* and cre* items are kept but not used in ESEM
* (ESEM uses only att, soc, emo, lang, link, read, write, num, cal items)
keep id* LA_EYFSP school_EYFSP cohort female age_EYFSP ///
     score_* sc_* att* soc* emo* lang* link* read* write* num* cal* ///
     space* know* phy* cre*

* Count missing individual EYFSP items used in ESEM
* (space* excluded as it is not used in the factor model)
egen missing_EYFSP = rowmiss(att* soc* emo* lang* link* read* write* num* cal* space*)
tab missing_EYFSP

compress
save "file_path\eyfsp_clean.dta", replace