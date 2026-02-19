********************************************************************************
* Project: Socio-Emotional Characteristics in Early Childhood and Offending
*          Behaviour in Adolescence
* Author:  Paul Garcia
* Institution: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
* Description: Main regression analysis file. Merges analysis dataset with
*              factor scores (ESEM, bifactor, PCA), IPW weights, and runs
*              all core regressions for SEN, CIN, school outcomes, KS2, and
*              offending. Includes robustness checks using sibling FE, bifactor
*              scores, and IPW reweighting for sample selection correction.
********************************************************************************

clear all

********************************************************************************
* PART 1: Data preparation — merge all datasets
********************************************************************************

use "file_path\clean_data.dta", clear

* Restrict to analysis sample: complete observable characteristics and valid EYFSP
keep if missing_vars == 0
drop if score_eyfsp_overall == 0

* Merge PCA factor scores (alternative factorization for robustness)
merge 1:1 id_dfe using "file_path\pca_scores.dta"
keep if _merge == 3
drop _merge

* Merge ESEM factor scores (main specification: cognitive and socio-emotional)
merge 1:1 id_dfe using "file_path\fs_esem.dta"
keep if _merge == 3
drop _merge

* Merge bifactor scores (appendix robustness check)
merge 1:1 id_dfe using "file_path\fs_bifactor.dta"
keep if _merge == 3
drop _merge
rename (gend_R cogn_R semo_R) (gend_bfs cogn_bfs semo_bfs)

* Merge inverse probability weights (left join: keep all analysis sample obs)
merge 1:1 id_dfe using "file_path\ipw.dta"
drop if _merge == 2
drop _merge

********************************************************************************
* PART 2: Variable construction
********************************************************************************

* Diagnostic tabs for birth year and month
tab ybirth
tab mbirth

* School and Local Authority at KS2 (Year 6)
gen school_KS2 = id_school_yr6
encode LA_school_code_yr6, gen(LA_KS2)   // numeric LA identifier for FE

* Keep variables needed for analysis
keep id_dfe id_moj cohort school_diff* ks2_* ks2_level4 offending *esem *fs  ///
     female fsm nowhite lang_neng part_time nsiblings first_born ybirth mbirth ///
     imd_decile LA_KS2 serious_violence violent_offence property_offence       ///
     drugs_offence summary_offence num_offences* school_KS2 groupid_sib        ///
     cpp cla *exc sen* cin excluded *ipw*

*------------------------------------------------------------------------------
* SEN: recode pupils with SEN recorded before school reception age (age < 4)
* These are pre-existing conditions, not school-related SEN
*------------------------------------------------------------------------------
bys sen: tab sen_age
replace sen = 0 if sen_age < 4 & sen_age != .

* Flag pupils with SEN prior to reception (used as control in regressions)
gen sen_prior = (sen_age < 4 & sen_age != .)
tab sen_prior
tab sen

count if sen_plan != 0 & sen_prior == 1   // SEN plan holders with pre-reception SEN

*------------------------------------------------------------------------------
* CIN: recode child in need status outside the relevant window
* Exclude CIN starting at age >= 11 (post-KS2) or before age 4 (pre-reception)
*------------------------------------------------------------------------------
bys cin: tab age_start_cin
replace cin = 0 if age_start_cin >= 11 & age_start_cin != .
replace cin = 0 if age_start_cin < 4  & age_start_cin != .

* Flag pupils with CIN prior to reception
gen cin_prior = (age_start_cin < 4 & age_start_cin != .)
tab cin_prior
tab cin

* School exclusions
tab excluded

*------------------------------------------------------------------------------
* Sibling sample: identify families with >= 2 siblings in the data
* Used for sibling fixed effects robustness check
*------------------------------------------------------------------------------
count if groupid_sib == .
bys groupid_sib: gen N = _N
tab N
gen sample_siblings = (N >= 2 & groupid_sib != .)
tab sample_siblings

*------------------------------------------------------------------------------
* Standardise and reverse-code factor scores
* Reverse coding: higher score = more difficulties (intuitive direction)
*------------------------------------------------------------------------------
foreach var of varlist *fs *esem {
    summ `var'
    replace `var' = (`var' - r(mean)) / r(sd)
    replace `var' = -`var'
}

*------------------------------------------------------------------------------
* KS2 outcomes: standardise test scores
* ks2_score: average of GPS, reading and maths standardised scores
*------------------------------------------------------------------------------
summ ks2_testmark_gps ks2_testmark_read ks2_testmark_math

foreach var of varlist *diff school_diff_alt ks1_score ks2_testmark_gps ///
                        ks2_testmark_read ks2_testmark_math ks2_score_ta {
    summ `var'
    replace `var' = (`var' - r(mean)) / r(sd)
}

* Composite KS2 score: row mean of three standardised test scores
egen ks2_total = rowmean(ks2_testmark_gps ks2_testmark_read ks2_testmark_math)
egen ks2_score = std(ks2_total), mean(0) sd(1)
summ ks2_score

* KS2 Level 4 indicators: achieving national expected standard in each subject
gen ks2_level4_read = (ks2_lev_read >= 4)
gen ks2_level4_math = (ks2_lev_math >= 4)
gen ks2_level4_gps  = (ks2_lev_gps  >= 4)

* Level 4 in at least one subject (used as binary outcome in LPM regressions)
gen ks2_level4_alt = (ks2_level4_read == 1 | ks2_level4_gps == 1 | ks2_level4_math == 1)
tab ks2_level4_alt

* Persistent absenteeism: >= 15% of sessions missed (DfE threshold)
gen persisten_absent = (share_absences >= 0.15)
tab persisten_absent

*------------------------------------------------------------------------------
* Global controls macro: used across all regressions
* Includes gender, family structure, ethnicity, FSM, deprivation, birth timing,
* language, part-time attendance, and pre-reception SEN/CIN status
*------------------------------------------------------------------------------
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm               ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng 1.part_time      ///
              1.sen_prior 1.cin_prior

* Summary statistics for key variables
summ offending *score *diff *fs *esem *mirt
summ sen cin school_diff_alt ks2_score offending

********************************************************************************
* PART 3: SEN regressions
* Outcome: ever had SEN status between reception and KS2
* Estimator: reghdfe (high-dimensional FE OLS / LPM)
* FE: Local Authority or school; SE clustered at school level
********************************************************************************

summ sen

* Main specification: LA FE
reghdfe sen semo_esem cogn_esem $census, absorb(LA_KS2) vce(cluster school_KS2)

* Non-linearity check: quadratic terms for both factors
reghdfe sen c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census, ///
    absorb(LA_KS2) vce(cluster school_KS2)

* School FE (more restrictive)
reghdfe sen c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Sub-group analysis within SEN pupils: type of need
* Drop sen_prior from controls since conditioning on sen == 1
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm               ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng 1.part_time      ///
              1.cin_prior

summ *esem if sen == 1 | sen_prior
summ sen_plan sen_besd sen_ld sen_pd if sen == 1 | sen_prior

* SEN statement or EHC plan
tab sen_plan
reghdfe sen_plan semo_esem cogn_esem $census if sen == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Behavioural, emotional and social difficulties
tab sen_besd
reghdfe sen_besd semo_esem cogn_esem $census if sen == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Learning difficulties
tab sen_ld
reghdfe sen_ld semo_esem cogn_esem $census if sen == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Physical disability
tab sen_pd
reghdfe sen_pd semo_esem cogn_esem $census if sen == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 4: CIN regressions
* Outcome: child in need status between reception and KS2
********************************************************************************

* Reset global to include sen_prior (drop cin_prior since conditioning on cin)
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm               ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng 1.part_time      ///
              1.sen_prior 1.cin_prior

summ cin
reghdfe cin semo_esem cogn_esem $census, absorb(LA_KS2) vce(cluster school_KS2)

reghdfe cin c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census, ///
    absorb(LA_KS2) vce(cluster school_KS2)

reghdfe cin c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Sub-group within CIN: child protection plan and looked-after status
* Drop cin_prior from controls
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm               ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng 1.part_time      ///
              1.sen_prior

summ *esem if cin == 1 | cin_prior == 1
summ cpp cla if cin == 1 | cin_prior == 1

* Child protection plan
reghdfe cpp semo_esem cogn_esem $census if cin == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Looked after / child looked after
reghdfe cla semo_esem cogn_esem $census if cin == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 5: School attendance regressions
* Outcome: school_diff_alt (standardised absenteeism score)
********************************************************************************

summ *esem school_diff_alt share_absences persisten_absent

* LA FE
reghdfe school_diff_alt semo_esem cogn_esem $census, ///
    absorb(LA_KS2) vce(cluster school_KS2)

* School FE
reghdfe school_diff_alt semo_esem cogn_esem $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 6: School exclusion regressions
* Outcome: ever permanently or fixed-term excluded
********************************************************************************

summ excluded

* LA FE
reghdfe excluded semo_esem cogn_esem $census, ///
    absorb(LA_KS2) vce(cluster school_KS2)

* School FE
reghdfe excluded semo_esem cogn_esem $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Restrict to schools with at least one excluded pupil
* (avoids identification from schools with no variation in outcome)
bys school_KS2: egen max_excluded = max(excluded)
tab max_excluded
summ excluded if max_excluded == 1
reghdfe excluded semo_esem cogn_esem $census if max_excluded == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

* Sub-group within excluded pupils: reason for exclusion
summ *esem if excluded == 1
summ physical_assault_exc verbal_abuse_exc misbehaviour_exc if excluded == 1

reghdfe physical_assault_exc semo_esem cogn_esem $census if excluded == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe verbal_abuse_exc semo_esem cogn_esem $census if excluded == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe misbehaviour_exc semo_esem cogn_esem $census if excluded == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 7: KS2 regressions
* Outcomes: standardised KS2 score and Level 4 binary indicator
********************************************************************************

summ *esem ks2_score ks2_level4_alt

* Main specifications: LA FE and school FE
reghdfe ks2_score    semo_esem cogn_esem $census, absorb(LA_KS2)    vce(cluster school_KS2)
reghdfe ks2_level4_alt semo_esem cogn_esem $census, absorb(LA_KS2)  vce(cluster school_KS2)
reghdfe ks2_score    semo_esem cogn_esem $census, absorb(school_KS2) vce(cluster school_KS2)
reghdfe ks2_level4_alt semo_esem cogn_esem $census, absorb(school_KS2) vce(cluster school_KS2)

* Adding school outcomes as controls (mediation exploration)
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem sen cin $census, ///
    absorb(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 8: Offending regressions
* Outcome: any criminal conviction by age 17
* Estimator: logit with marginal effects; conditional logit (school FE)
********************************************************************************

summ *esem offending school_diff_alt ks2_score

* Reset controls macro to include both sen_prior and cin_prior
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm               ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng 1.part_time      ///
              1.sen_prior 1.cin_prior

* Baseline logit with LA FE: average marginal effects for key variables
logit offending semo_esem cogn_esem $census i.LA_KS2, or vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem 1.female 1.nowhite 1.fsm) post

* Conditional logit: school FE (within-school identification)
clogit offending semo_esem cogn_esem $census, or group(school_KS2) vce(cluster school_KS2)

* Add school outcome mediators: absenteeism and exclusion
logit offending semo_esem cogn_esem school_diff_alt excluded $census i.LA_KS2, ///
    vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem school_diff_alt excluded 1.female 1.nowhite 1.fsm) post

* Add KS2 score as additional mediator
logit offending semo_esem cogn_esem school_diff_alt excluded ks2_score $census ///
    i.LA_KS2, or vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem school_diff_alt excluded ks2_score 1.female 1.nowhite 1.fsm) post

* Full model including SEN and CIN
logit offending ks2_score school_diff_alt excluded *semo_esem cogn_esem sen cin ///
    $census i.LA_KS2, or vce(cluster school_KS2)

* Interaction model: heterogeneous effects by SEN and CIN status
logit offending ks2_score excluded school_diff_alt semo_esem cogn_esem         ///
    c.cogn_esem#i.sen c.semo_esem#i.sen c.cogn_esem#i.cin c.semo_esem#i.cin   ///
    i.sen i.cin $census i.LA_KS2, or vce(cluster school_KS2)

* Marginal effects at sen=0, cin=0 (non-vulnerable pupils)
margins, dydx(ks2_score excluded school_diff_alt semo_esem cogn_esem 1.sen     ///
    1.cin 1.female nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile)    ///
    at(sen=0 cin=0) post

* Marginal effects at sen=1, cin=1 (SEN and CIN pupils)
margins, dydx(ks2_score excluded school_diff_alt semo_esem cogn_esem i.sen     ///
    i.cin 1.female nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile)    ///
    at(sen=1 cin=1) post

* Restrict to schools with at least one offender (variation in outcome)
bys school_KS2: egen max_offending = max(offending)
tab max_offending
summ offending if max_offending == 1

* Conditional logit with full mediator set and interactions
clogit offending ks2_score excluded school_diff_alt semo_esem cogn_esem        ///
    c.cogn_esem#1.sen c.semo_esem#1.sen c.cogn_esem#1.cin c.semo_esem#1.cin   ///
    1.sen 1.cin $census, or group(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 9: Offence type and count regressions (within offending sample)
* Restricts to offending == 1; examines violent, drug, property and summary offences
********************************************************************************

summ *esem school_diff_alt ks2_score excluded if offending == 1
summ violent_offence drugs_offence property_offence summary_offence

*--- Violent offences (violence, sexual, robbery) ---
logit violent_offence semo_esem cogn_esem $census i.LA_KS2 if offending == 1, ///
    vce(cluster school_KS2)

logit violent_offence ks2_score school_diff_alt excluded semo_esem cogn_esem  ///
    $census i.LA_KS2 if offending == 1, vce(cluster school_KS2)

*--- Number of offences: negative binomial (accounts for overdispersion) ---
summ num_offences num_offences_cens

nbreg num_offences_cens semo_esem cogn_esem $census i.LA_KS2 if offending == 1, ///
    vce(cluster school_KS2)

nbreg num_offences_cens ks2_score school_diff_alt excluded semo_esem cogn_esem ///
    $census i.LA_KS2 if offending == 1, vce(cluster school_KS2)

margins, dydx(*esem 1.female nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile) post
margins, dydx(ks2_score school_diff_alt excluded *esem 1.female nsiblings      ///
    1.first_born 1.nowhite 1.fsm ib10.imd_decile) post

outreg2 using table, alpha(0.01, 0.05, 0.10) bracket nocons excel dec(3) replace

*--- Non-violent offenders subsample (violent_offence == 0) ---
summ *esem school_diff_alt ks2_score if offending == 1 & violent_offence == 0
summ drugs_offence property_offence summary_offence if offending == 1 & violent_offence == 0

* Drugs and weapons
logit drugs_offence semo_esem cogn_esem $census i.LA_KS2 ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit drugs_offence ks2_score school_diff_alt excluded semo_esem cogn_esem    ///
    $census i.LA_KS2 if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

* Property offences (theft, damage/arson, public order, fraud)
logit property_offence semo_esem cogn_esem $census i.LA_KS2 ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit property_offence ks2_score school_diff_alt excluded semo_esem cogn_esem ///
    $census i.LA_KS2 if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

* Summary offences (non-motoring and motoring)
logit summary_offence semo_esem cogn_esem $census i.LA_KS2 ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit summary_offence ks2_score school_diff_alt excluded semo_esem cogn_esem  ///
    $census i.LA_KS2 if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

********************************************************************************
* PART 10: Appendix — Sibling fixed effects
* Exploits within-family variation to control for shared family background
* Clusters SE at family (groupid_sib) level
********************************************************************************

summ *esem school_diff_alt ks2_score excluded offending if sample_siblings

* Full sibling sample: include female as control (varies within family)
global census 1.female nsiblings 1.first_born i.ybirth i.mbirth 1.part_time  ///
              1.sen_prior 1.cin_prior

reghdfe offending semo_esem cogn_esem $census, ///
    absorb(groupid_sib) vce(cluster groupid_sib)

* Male-only subsample: drop female indicator (no within-family variation)
global census nsiblings 1.first_born i.ybirth i.mbirth 1.part_time           ///
              1.sen_prior 1.cin_prior

reghdfe offending semo_esem cogn_esem $census if female == 0, ///
    absorb(groupid_sib) vce(cluster groupid_sib)

reghdfe offending ks2_score school_diff_alt excluded semo_esem cogn_esem      ///
    $census if female == 0, absorb(groupid_sib) vce(cluster groupid_sib)

margins, dydx(*esem 1.female nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile) post
margins, dydx(ks2_score school_diff_alt excluded *esem sen cin 1.female        ///
    nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile) post

outreg2 using table, alpha(0.01, 0.05, 0.10) bracket nocons excel dec(5) replace

********************************************************************************
* PART 11: Appendix — Bifactor model robustness check
* Uses general (gend_bfs) and specific (cogn_bfs, semo_bfs) bifactor scores
* as alternative to ESEM two-factor specification
********************************************************************************

summ *bfs school_diff_alt ks2_score

* General factor only
clogit offending gend_bfs $census, or group(school_KS2) vce(cluster school_KS2)

* All bifactor scores
clogit offending *bfs $census, or group(school_KS2) vce(cluster school_KS2)

* With mediators
clogit offending ks2_score school_diff_alt excluded *bfs $census, ///
    or group(school_KS2) vce(cluster school_KS2)

clogit offending ks2_score school_diff_alt excluded *bfs sen cin $census, ///
    or group(school_KS2) vce(cluster school_KS2)

* With SEN and CIN interactions
clogit offending ks2_score school_diff_alt excluded *bfs                       ///
    (c.gend_bfs c.cogn_bfs c.semo_bfs)#1.sen                                  ///
    (c.gend_bfs c.cogn_bfs c.semo_bfs)#1.cin 1.sen 1.cin $census,            ///
    or group(school_KS2) vce(cluster school_KS2)

********************************************************************************
* PART 12: Appendix — IPW robustness check for sample selection
* Compares unweighted (baseline) vs IPW-weighted estimates
* IPW corrects for non-random selection into the linked MoJ-DfE sample
********************************************************************************

summ ipw ipw_school
corr ipw ipw_school

* Exclusions: unweighted vs IPW
reghdfe excluded semo_esem cogn_esem $census if ipw ~= ., ///
    absorb(school_KS2) vce(cluster school_KS2)
reghdfe excluded semo_esem cogn_esem $census [pw=ipw], ///
    absorb(school_KS2) vce(cluster school_KS2)

* Absenteeism: unweighted vs IPW
reghdfe school_diff_alt semo_esem cogn_esem $census if ipw ~= ., ///
    absorb(school_KS2) vce(cluster school_KS2)
reghdfe school_diff_alt semo_esem cogn_esem $census [pw=ipw], ///
    absorb(school_KS2) vce(cluster school_KS2)

* KS2 score: unweighted vs IPW
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census if ipw ~= ., ///
    absorb(school_KS2) vce(cluster school_KS2)
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census [pw=ipw], ///
    absorb(school_KS2) vce(cluster school_KS2)

outreg2 using table, alpha(0.01, 0.05, 0.10) bracket nocons excel dec(3) replace

* Offending: unweighted vs IPW
* Note: logit with [pw=ipw] gives pseudo-maximum likelihood weighted estimates
logit offending ks2_score school_diff_alt excluded semo_esem cogn_esem        ///
    $census i.LA_KS2 if ipw != ., or vce(cluster school_KS2)

logit offending ks2_score school_diff_alt excluded semo_esem cogn_esem        ///
    $census i.LA_KS2 [pw=ipw], or vce(cluster school_KS2)

margins, dydx(ks2_score school_diff_alt excluded semo_esem cogn_esem 1.female ///
    nsiblings 1.first_born 1.nowhite 1.fsm ib10.imd_decile) post

outreg2 using table, alpha(0.01, 0.05, 0.10) bracket nocons excel dec(5) replace