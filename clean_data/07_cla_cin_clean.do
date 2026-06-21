********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 07_cla_cin_clean.do
* Purpose : Import and clean Children Looked After (CLA) and Children in Need
*           (CIN) records; construct cohort-aligned indicators for care
*           history, placement, category of need, and child protection plans.
*
* Inputs  : CLA_2006_to_2023.csv    (one record per child per processing year)
*           CIN_2009_to_2023.csv    (one record per child per academic year)
*           eyfsp_clean.dta         (cohort)
*
* Outputs : cla_clean.dta
*           cin_clean.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off


********************************************************************************
* PART A — CHILDREN LOOKED AFTER (CLA)
********************************************************************************

import delimited "file_path\CLA_2006_to_2023.csv", clear

*------------------------------------------------------------------------------
* A.1  Identifiers and deduplication
*------------------------------------------------------------------------------
rename cla_pupilmatchingrefanonymous id_dfe

sort id_dfe cla_processing_year
bys id_dfe cla_processing_year: gen n = _n
tab n
drop n

* Within pupil-year, keep the record with the longest placement
bys id_dfe cla_processing_year: egen max_length = max(cla_poc_length)
keep if max_length == cla_poc_length

* Drop remaining ties: keep first observation per pupil-year
sort id_dfe cla_processing_year
bys id_dfe cla_processing_year: gen n = _n
tab n
keep if n == 1
drop n

*------------------------------------------------------------------------------
* A.2  Keep variables of interest; restrict to sample window
*------------------------------------------------------------------------------
tab cla_processing_year
rename cla_processing_year year

keep id_dfe year cla_poc_start cla_poc_length cla_cat_need cla_placement
keep if year <= 2015

tab year
tab cla_poc_length
tab cla_cat_need
tab cla_placement

*------------------------------------------------------------------------------
* A.3  Cohort index flags
*      Cohort 1: Reception 2007, KS2 2013 → include years up to 2013
*      Cohort 2: Reception 2008, KS2 2014 → include years up to 2014
*      Cohort 3: Reception 2009, KS2 2015 → include years up to 2015
*------------------------------------------------------------------------------
gen index_ch1 = (year <= 2013)
gen index_ch2 = (year <= 2014)
gen index_ch3 = (year <= 2015)

*------------------------------------------------------------------------------
* A.4  Start year of most recent period of continuous care
*------------------------------------------------------------------------------
gen start_year_cla = substr(cla_poc_start, 1, 4)
destring start_year_cla, replace force
tab start_year_cla

foreach i in 1 2 3 {
	sort id_dfe year
	bys id_dfe: egen start_year_cla_ch`i' = min(start_year_cla) if index_ch`i' == 1
	replace start_year_cla_ch`i' = . if index_ch`i' == 0
	tab start_year_cla_ch`i'
}

*------------------------------------------------------------------------------
* A.5  Number of placement changes between Reception and KS2
*      A change is counted each time cla_placement differs from the previous
*      record for the same child (within the cohort window).
*------------------------------------------------------------------------------
foreach i in 1 2 3 {
	gen n = 0
	sort id_dfe year
	bys id_dfe: replace n = 1 if cla_placement != cla_placement[_n-1] & index_ch`i' == 1
	bys id_dfe: egen placement_changes_ch`i' = total(n)
	replace placement_changes_ch`i' = . if index_ch`i' == 0
	tab placement_changes_ch`i'
	drop n
}

*------------------------------------------------------------------------------
* A.6  Maximum placement length (months, cumulative for continuous care)
*------------------------------------------------------------------------------
foreach i in 1 2 3 {
	sort id_dfe year
	bys id_dfe: egen placement_length_ch`i' = max(cla_poc_length) if index_ch`i' == 1
	replace placement_length_ch`i' = . if index_ch`i' == 0
	tab placement_length_ch`i'
}

*------------------------------------------------------------------------------
* A.7  Category of need indicators
*      N1        → abuse/neglect
*      N4 / N5   → family problems
*      N6        → social misbehaviour
*      N2/N3/N7/N8 → other (disability, parental disability, income, absent parenting)
*------------------------------------------------------------------------------
gen abuse        = (cla_cat_need == "N1")
gen fam_prob     = inlist(cla_cat_need, "N4", "N5")
gen soc_misbehave = (cla_cat_need == "N6")
gen other_prob   = inlist(cla_cat_need, "N2", "N3", "N7", "N8")

foreach i in 1 2 3 {
	sort id_dfe year
	foreach stub in abuse fam_prob soc_misbehave other_prob {
		bys id_dfe: egen `stub'_ch`i' = max(`stub') if index_ch`i' == 1
		replace `stub'_ch`i' = . if index_ch`i' == 0
		tab `stub'_ch`i'
	}
}

*------------------------------------------------------------------------------
* A.8  Collapse to one observation per child
*------------------------------------------------------------------------------
collapse placement_changes_ch* placement_length_ch* ///
         fam_prob_ch* soc_misbehave_ch* abuse_ch* other_prob_ch* ///
         start_year_cla_ch*, by(id_dfe)

summ placement_changes_ch* placement_length_ch* fam_prob_ch* ///
     soc_misbehave_ch* abuse_ch* other_prob_ch* start_year_cla_ch*
mdesc

*------------------------------------------------------------------------------
* A.9  Merge EYFSP cohort; construct cohort-aligned variables
*------------------------------------------------------------------------------
merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
gen cla = (_merge == 3)
drop _merge

foreach stub in placement_changes placement_length abuse_cla fam_prob_cla ///
                soc_misbehave_cla other_prob_cla year_cla {
	gen `stub' = .
}

foreach i in 1 2 3 {
	replace placement_changes  = placement_changes_ch`i'  if cohort == `i' & placement_changes  == .
	replace placement_length   = placement_length_ch`i'   if cohort == `i' & placement_length   == .
	replace abuse_cla          = abuse_ch`i'              if cohort == `i' & abuse_cla          == .
	replace fam_prob_cla       = fam_prob_ch`i'           if cohort == `i' & fam_prob_cla       == .
	replace soc_misbehave_cla  = soc_misbehave_ch`i'     if cohort == `i' & soc_misbehave_cla  == .
	replace other_prob_cla     = other_prob_ch`i'         if cohort == `i' & other_prob_cla     == .
	replace year_cla           = start_year_cla_ch`i'    if cohort == `i' & year_cla           == .
}

*------------------------------------------------------------------------------
* A.10  Flag and recode CLA records that post-date KS2
*       Cohort 1: year_cla >= 2014 implies CLA only after KS2
*       Cohort 2: year_cla >= 2015 implies CLA only after KS2
*       Cohort 3: all years up to 2015 are within window — no correction needed
*------------------------------------------------------------------------------
count if cla == 1 & year_cla >= 2014 & cohort == 1
count if cla == 1 & year_cla >= 2015 & cohort == 2

replace cla = 0 if (year_cla >= 2014 & cohort == 1) ///
                 | (year_cla >= 2015 & cohort == 2)

summ cla placement_changes placement_length abuse_cla fam_prob_cla ///
     soc_misbehave_cla other_prob_cla year_cla

keep id_dfe cohort cla placement_changes placement_length ///
     abuse_cla fam_prob_cla soc_misbehave_cla other_prob_cla year_cla

mdesc
compress

save "file_path\cla_clean.dta", replace


********************************************************************************
* PART B — CHILDREN IN NEED (CIN)
********************************************************************************

import delimited "file_path\CIN_2009_to_2023.csv", clear

keep cin_pupilmatchingrefanonymous cin_acadyr cin_latestreferraldate ///
     cin_numberofpreviouscpp cin_latestcategoryofabuse ///
     cin_referraldate cin_agestartofcinperiod
mdesc

*------------------------------------------------------------------------------
* B.1  Identifiers
*------------------------------------------------------------------------------
rename cin_pupilmatchingrefanonymous id_dfe

*------------------------------------------------------------------------------
* B.2  Academic year to calendar year; restrict to sample window
*------------------------------------------------------------------------------
gen year = substr(cin_acadyr, 6, 4)
destring year, replace
tab year
keep if year <= 2015

*------------------------------------------------------------------------------
* B.3  One observation per pupil-year (latest referral date within year)
*------------------------------------------------------------------------------
sort id_dfe cin_acadyr cin_latestreferraldate
bys id_dfe cin_acadyr: gen n = _n
keep if n == 1
drop n

*------------------------------------------------------------------------------
* B.4  Cohort index flags
*------------------------------------------------------------------------------
gen index_ch1 = (year <= 2013)
gen index_ch2 = (year <= 2014)
gen index_ch3 = (year <= 2015)

*------------------------------------------------------------------------------
* B.5  Maximum number of previous CPP plans within cohort window
*------------------------------------------------------------------------------
foreach i of numlist 1 2 3 {
	sort id_dfe year
	bys id_dfe: egen previous_cpp_ch`i' = max(cin_numberofpreviouscpp) if index_ch`i' == 1
	replace previous_cpp_ch`i' = . if index_ch`i' == 0
	tab previous_cpp_ch`i'
}

*------------------------------------------------------------------------------
* B.6  Latest category of abuse (CPP records only; missing = no CPP)
*      EMO → emotional abuse
*      NEG → neglect
*      PHY / SAB → physical abuse / sexual abuse
*      MUL → multiple categories
*------------------------------------------------------------------------------
tab cin_latestcategoryofabuse

gen cpp_emotion  = (cin_latestcategoryofabuse == "EMO")
gen cpp_neglect  = (cin_latestcategoryofabuse == "NEG")
gen cpp_physical = inlist(cin_latestcategoryofabuse, "PHY", "SAB")
gen cpp_mult     = (cin_latestcategoryofabuse == "MUL")

* Set to missing where no CPP (category blank)
foreach stub in cpp_emotion cpp_neglect cpp_physical cpp_mult {
	replace `stub' = . if cin_latestcategoryofabuse == ""
}

foreach i in 1 2 3 {
	sort id_dfe year
	foreach stub in cpp_emotion cpp_neglect cpp_physical cpp_mult {
		bys id_dfe: egen `stub'_ch`i' = max(`stub') if index_ch`i' == 1
		replace `stub'_ch`i' = . if index_ch`i' == 0
		tab `stub'_ch`i'
	}
}

*------------------------------------------------------------------------------
* B.7  Number of CIN episodes (distinct referral dates within cohort window)
*------------------------------------------------------------------------------
gen referral_date = date(cin_referraldate, "YMD")
format referral_date %td
mdesc referral_date

gen referral_year = substr(cin_referraldate, 1, 4)
destring referral_year, replace
tab referral_year

gen episodes = 0
sort id_dfe referral_date
bys id_dfe: replace episodes = 1 if referral_date != referral_date[_n-1]

foreach i in 1 2 3 {
	bys id_dfe: egen episodes_ch`i' = total(episodes) if index_ch`i' == 1
	tab episodes_ch`i'
}

*------------------------------------------------------------------------------
* B.8  Age at first CIN referral
*------------------------------------------------------------------------------
bys id_dfe: egen age_start_cin = min(cin_agestartofcinperiod) ///
	if cin_agestartofcinperiod >= 0
tab age_start_cin

*------------------------------------------------------------------------------
* B.9  Collapse to one observation per child
*------------------------------------------------------------------------------
keep id_dfe previous_cpp_ch* cpp_emotion_ch* cpp_neglect_ch* ///
     cpp_physical_ch* cpp_mult_ch* episodes_ch* age_start_cin

collapse (mean) previous_cpp_ch* cpp_emotion_ch* cpp_neglect_ch* ///
                cpp_physical_ch* cpp_mult_ch* episodes_ch* age_start_cin ///
         , by(id_dfe)

summ previous_cpp_ch* cpp_emotion_ch* cpp_neglect_ch* ///
     cpp_physical_ch* cpp_mult_ch* episodes_ch* age_start_cin
mdesc

*------------------------------------------------------------------------------
* B.10  Merge EYFSP cohort; construct cohort-aligned variables
*------------------------------------------------------------------------------
merge 1:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
gen cin = (_merge == 3)
drop _merge

foreach stub in previous_cpp episodes_cin cpp_emotion cpp_neglect cpp_physical cpp_mult {
	gen `stub' = .
}

foreach i in 1 2 3 {
	replace previous_cpp  = previous_cpp_ch`i'  if cohort == `i' & previous_cpp  == .
	replace episodes_cin  = episodes_ch`i'       if cohort == `i' & episodes_cin  == .
	replace cpp_emotion   = cpp_emotion_ch`i'    if cohort == `i' & cpp_emotion   == .
	replace cpp_neglect   = cpp_neglect_ch`i'    if cohort == `i' & cpp_neglect   == .
	replace cpp_physical  = cpp_physical_ch`i'   if cohort == `i' & cpp_physical  == .
	replace cpp_mult      = cpp_mult_ch`i'       if cohort == `i' & cpp_mult      == .
}

*------------------------------------------------------------------------------
* B.11  Recode CIN status for pupils referred only after KS2
*       Age >= 12 at first referral implies post-KS2 involvement only.
*------------------------------------------------------------------------------
tab cin
count if cin == 1 & age_start_cin >= 12
replace cin           = . if age_start_cin >= 12 & age_start_cin != .
replace age_start_cin = . if age_start_cin >= 12 & age_start_cin != .
tab age_start_cin

summ cin previous_cpp episodes_cin cpp_emotion cpp_neglect cpp_physical cpp_mult age_start_cin

keep id_dfe cin previous_cpp episodes_cin cpp_emotion cpp_neglect ///
     cpp_physical cpp_mult age_start_cin

mdesc
compress

save "file_path\cin_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
