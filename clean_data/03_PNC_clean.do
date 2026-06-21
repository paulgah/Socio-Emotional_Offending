********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 03_PNC_clean.do
* Purpose : Import and clean MoJ Police National Computer (PNC) offending
*           records; merge with census and EYFSP identifiers; construct
*           offence-level variables; collapse to one observation per offender
*           for the cross-sectional analysis.
*
* Inputs  : MOJ_PNC.csv
*           census_clean.dta   (ybirth, mbirth)
*           eyfsp_clean.dta    (cohort)
*           serious_crime.do   (generates serious_violence indicator)
*           custodial_sentence.do
*
* Output  : crime_clean.dta
*
* Notes   : All file paths use the placeholder "file_path\" and should be
*           updated to absolute paths before running.
********************************************************************************

clear all
set more off

********************************************************************************
* SECTION 1 — IMPORT AND MERGE
********************************************************************************

import delimited "file_path\MOJ_PNC.csv", clear

*------------------------------------------------------------------------------
* 1.1  Pupil identifiers
*------------------------------------------------------------------------------
codebook mojuid
rename mojuid               id_moj
rename pupilmatchingrefanonymous id_dfe

*------------------------------------------------------------------------------
* 1.2  Merge year and month of birth from census
*      m:1 because multiple offence records map to a single pupil
*------------------------------------------------------------------------------
merge m:1 id_dfe using "file_path\census_clean.dta", keepusing(ybirth mbirth)
keep if _merge == 3
tab ybirth
drop _merge

*------------------------------------------------------------------------------
* 1.3  Merge cohort from EYFSP
*------------------------------------------------------------------------------
merge m:1 id_dfe using "file_path\eyfsp_clean.dta", keepusing(cohort)
keep if _merge == 3
tab cohort
drop _merge

********************************************************************************
* SECTION 2 — OFFENCE-LEVEL VARIABLES
********************************************************************************

*------------------------------------------------------------------------------
* 2.1  Date and year of offence
*      Offences outside England & Wales may have missing offencestartdate.
*------------------------------------------------------------------------------
gen offence_year = substr(offencestartdate, 1, 4)
destring offence_year, replace
replace offence_year = . if offence_year == 1900
tab offence_year

gen offence_date = date(offencestartdate, "YMD")
format offence_date %td
replace offence_date = . if offence_year == .

*------------------------------------------------------------------------------
* 2.2  Age at offence
*------------------------------------------------------------------------------
rename offencestartage age
destring age, replace
replace age = . if age < 0
tab age

* Cross-check: age implied by year of birth
gen age_birth = offence_year - ybirth
replace age_birth = . if age_birth < 0
tab age_birth
* Check consistency (within ±1 year of reported age)
count if age == age_birth | age == age_birth + 1 | age == age_birth - 1

*------------------------------------------------------------------------------
* 2.3  Primary offence indicator
*------------------------------------------------------------------------------
gen prim_offence = .
replace prim_offence = 0 if isprimaryoffence == "N"
replace prim_offence = 1 if isprimaryoffence == "Y"
tab prim_offence

*------------------------------------------------------------------------------
* 2.4  Disposal rank
*------------------------------------------------------------------------------
destring disposalrank, replace
tab disposalrank

*------------------------------------------------------------------------------
* 2.5  Adjudication — guilty indicator
*      "G" = guilty; "J" = guilty (judicial decision); others = not guilty
*------------------------------------------------------------------------------
gen guilty = (adjudicationcode == "G" | adjudicationcode == "J")
tab adjudicationcode guilty

*------------------------------------------------------------------------------
* 2.6  Offence group classification
*      Groups derived from first two characters of offence_group code.
*      NOTE: confirm group codes against MoJ data dictionary — groupings below
*      follow the version used in the original analysis but should be verified
*      if the PNC extract is updated.
*------------------------------------------------------------------------------
gen group = substr(offence_group, 1, 2)
destring group, replace
tab group

gen violent_offence  = (group == 1  | group == 2  | group == 3)
gen property_offence = (group == 4  | group == 5  | group == 8  | group == 9  | group == 10)
gen drugs_offence    = (group == 6  | group == 7)
gen summary_offence  = (group == 11 | group == 12)
gen other_offence    = (group == 20 | group == 21 | group == 23)

tab violent_offence
tab property_offence
tab drugs_offence
tab summary_offence
tab other_offence

*------------------------------------------------------------------------------
* 2.7  Serious violent offence — defined in external do-file
*------------------------------------------------------------------------------
qui do "file_path\serious_crime.do"
tab serious_violence
tab offence_group if serious_violence == 1


********************************************************************************
* SECTION 3 — SAMPLE SELECTION
*
* Retain one record per offender-case-offence-date at the highest disposal rank.
* For records with missing disposal rank, retain the first observation,
* giving priority to serious violence offences.
********************************************************************************

codebook id_moj

* Retain primary, guilty offences only
mdesc prim_offence guilty
count if prim_offence == 1 & guilty == 1
count if prim_offence == 1 & guilty == 0
keep if prim_offence == 1 & guilty == 1

* Within offender-case-offence-date, keep lowest (most serious) disposal rank
gsort id_moj caseid offenceid offencestartdate disposalrank
bys id_moj caseid offenceid offencestartdate: gen n = _n

keep if (n == 1 & disposalrank == 1) | disposalrank == .
tab n
drop n

* For remaining ties (missing disposal rank), keep first, prioritising serious violence
gsort id_moj caseid offenceid offencestartdate -serious_violence
bys id_moj caseid offenceid: gen n = _n
tab n
keep if n == 1
drop n


********************************************************************************
* SECTION 4 — OFFENDING VARIABLES
********************************************************************************

*------------------------------------------------------------------------------
* 4.1  Custodial sentence — defined in external do-file
*------------------------------------------------------------------------------
do "file_path\custodial_sentence.do"
tab custodial_sentence
bys serious_violence: tab custodial_sentence

*------------------------------------------------------------------------------
* 4.2  Unique offence tag (for counts)
*------------------------------------------------------------------------------
sort id_moj caseid offenceid
egen id_offence = tag(id_moj caseid offenceid)
tab id_offence

*------------------------------------------------------------------------------
* 4.3  Age at first offence
*      Two age measures available: reported (age) and birth-year-derived
*      (age_birth). The composite age_first_offence takes the birth-year
*      measure and accepts the reported age when it falls within ±1 year,
*      to accommodate month-of-birth timing differences.
*------------------------------------------------------------------------------
sort id_moj age caseid offenceid
bys id_moj: egen age_offence = min(age)
tab age_offence

sort id_moj age_birth caseid offenceid
bys id_moj: egen age_min = min(age_birth)
tab age_min

gen age_first_offence = age_min
replace age_first_offence = age_offence ///
	if age_offence == age_min + 1 | age_offence == age_min - 1
tab age_first_offence

*------------------------------------------------------------------------------
* 4.4  Flag offences outside the adolescent window (age < 10 or age >= 18)
*      Used to identify implausible or out-of-scope records in sensitivity checks.
*------------------------------------------------------------------------------
gen flag_offending_noteen = 0
replace flag_offending_noteen = 1 if age_first_offence <= 10 & age_first_offence != .
replace flag_offending_noteen = 1 if age_first_offence >= 18 & age_first_offence != .


********************************************************************************
* SECTION 5 — COLLAPSE TO CROSS-SECTIONAL FILE (one observation per offender)
********************************************************************************
collapse ///
	(max)  offending serious_violence violent_offence property_offence ///
	       drugs_offence summary_offence other_offence guilty ///
	(sum)  num_offences = offending num_serious_offences = serious_violence ///
	(mean) age_first_offence flag_offending_noteen ///
	, by(id_moj id_dfe)

mdesc
compress

save "file_path\crime_clean.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
