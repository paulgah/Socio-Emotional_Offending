********************************************************************************
* Project : Socio-Emotional Characteristics in Early Childhood and
*           Offending Behaviour in Adolescence
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 04_regressions.do
* Purpose : Main regression analysis linking EYFSP cognitive (cogn_esem) and
*           socio-emotional (semo_esem) factor scores to school-age outcomes
*           and adolescent offending.
*
*           Sections:
*             1.  Data preparation and sample restrictions
*             2.  SEN regressions (any provision; by type within SEN sample)
*             3.  CIN regressions (any CIN; CPP and CLA within CIN sample)
*             4.  School difficulties (transitions and absences)
*             5.  School exclusions (any; by reason within excluded sample)
*             6.  KS2 attainment
*             7.  Offending — main specifications
*             8.  Offending — type and count (offending sample)
*             9.  Appendix: sibling fixed effects
*             10. Appendix: bifactor model
*             11. Appendix: sample selection and IPW
*
* Inputs  : clean_data.dta, pca_scores.dta, fs_esem.dta,
*           fs_bifactor.dta, ipw.dta
*
* Notes   : All factor scores (cogn_esem, semo_esem, bifactor) are standardised
*           with sign reversed so that higher values indicate greater difficulty.
*           KS2 test scores are standardised to mean 0, sd 1.
*           School fixed effects absorbed via reghdfe; standard errors clustered
*           at school level throughout unless otherwise noted.
*           global $census is redefined at several points — each redefinition
*           is flagged with a comment explaining what changed and why.
********************************************************************************

clear all
set more off


********************************************************************************
* SECTION 1 — DATA PREPARATION
********************************************************************************

use "file_path\clean_data.dta", clear
keep if missing_vars == 0
drop if score_eyfsp_overall == 0

* Factor scores from alternative measurement models
merge 1:1 id_dfe using "file_path\pca_scores.dta"
keep if _merge == 3
drop _merge

merge 1:1 id_dfe using "file_path\fs_esem.dta"
keep if _merge == 3
drop _merge

* Bifactor model scores; rename to distinguish from ESEM scores
merge 1:1 id_dfe using "file_path\fs_bifactor.dta"
keep if _merge == 3
drop _merge
rename (gend_R cogn_R semo_R) (gend_bfs cogn_bfs semo_bfs)

* IPW weights for sample selection correction (not all obs have weights)
merge 1:1 id_dfe using "file_path\ipw.dta"
drop if _merge == 2
drop _merge

*------------------------------------------------------------------------------
* 1.1  Geographic identifiers
*------------------------------------------------------------------------------
gen school_KS2 = id_school_yr6
encode LA_school_code_yr6, gen(LA_KS2)

*------------------------------------------------------------------------------
* 1.2  Keep variables of interest
*------------------------------------------------------------------------------
keep id_dfe id_moj cohort school_diff* ks2_* ks2_level4 offending        ///
     *esem *fs female fsm nowhite lang_neng part_time                     ///
     nsiblings first_born ybirth mbirth imd_decile LA_KS2                 ///
     serious_violence violent_offence property_offence                    ///
     drugs_offence summary_offence num_offences* school_KS2               ///
     groupid_sib cpp cla *exc sen* cin excluded *ipw*                     ///
     *bfs

*------------------------------------------------------------------------------
* 1.3  SEN: recode pre-school entries
*      Children flagged with SEN before school reception (age < 4) are
*      recoded to sen = 0 for the school-age analysis; a separate indicator
*      sen_prior captures pre-reception SEN status for use as a control.
*------------------------------------------------------------------------------
bys sen: tab sen_age
replace sen = 0 if sen_age < 4 & sen_age != .

gen sen_prior = (sen_age < 4 & sen_age != .)
tab sen_prior
tab sen

count if sen_plan != 0 & sen_prior == 1

*------------------------------------------------------------------------------
* 1.4  CIN: recode entries outside school-age window
*      CIN spells starting at age >= 11 (secondary school) or before
*      reception (age < 4) are excluded from the school-age CIN indicator.
*------------------------------------------------------------------------------
bys cin: tab age_start_cin
replace cin = 0 if age_start_cin >= 11 & age_start_cin != .
replace cin = 0 if age_start_cin < 4  & age_start_cin != .

gen cin_prior = (age_start_cin < 4 & age_start_cin != .)
tab cin_prior
tab cin

*------------------------------------------------------------------------------
* 1.5  School exclusions
*------------------------------------------------------------------------------
tab excluded

*------------------------------------------------------------------------------
* 1.6  Sibling sample
*      Sibling group identifier is only available from the 2016 census wave;
*      sample_siblings flags families with two or more linked siblings.
*------------------------------------------------------------------------------
count if groupid_sib == .
bys groupid_sib: gen N = _N
tab N
gen sample_siblings = (N >= 2 & groupid_sib != .)
tab sample_siblings

*------------------------------------------------------------------------------
* 1.7  Standardise and sign-reverse factor scores
*      Higher values = greater difficulty (consistent with paper framing).
*------------------------------------------------------------------------------
foreach var of varlist *bfs *esem {
    summ `var'
    replace `var' = -(`var' - r(mean)) / r(sd)
}

*------------------------------------------------------------------------------
* 1.8  Standardise KS2 and school difficulty measures
*------------------------------------------------------------------------------
summ ks2_testmark_gps ks2_testmark_read ks2_testmark_math

foreach var of varlist *diff school_diff_alt ks1_score                    ///
                        ks2_testmark_gps ks2_testmark_read                ///
                        ks2_testmark_math ks2_score_ta {
    summ `var'
    replace `var' = (`var' - r(mean)) / r(sd)
}

* Composite KS2 score: mean of GPS, reading and maths test marks
egen ks2_total = rowmean(ks2_testmark_gps ks2_testmark_read ks2_testmark_math)
egen ks2_score = std(ks2_total), mean(0) sd(1)
summ ks2_score

* Level 4+ indicators by subject and combined
gen ks2_level4_read = (ks2_lev_read >= 4)
gen ks2_level4_math = (ks2_lev_math >= 4)
gen ks2_level4_gps  = (ks2_lev_gps  >= 4)
gen ks2_level4_alt  = (ks2_level4_read == 1 | ks2_level4_gps == 1 | ks2_level4_math == 1)
tab ks2_level4_alt

* Persistent absenteeism (DfE threshold: >= 15% of sessions missed)
gen persistent_absent = (share_absences >= 0.15)
tab persistent_absent

*------------------------------------------------------------------------------
* 1.9  Baseline control set
*      Full demographic and SES controls used in main specifications.
*      Note: $census is redefined in Sections 9 for sibling FE models
*      (ethnicity, FSM and IMD are collinear within sibling groups and dropped).
*------------------------------------------------------------------------------
global census 1.female nsiblings 1.first_born 1.nowhite 1.fsm            ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng               ///
              1.part_time 1.sen_prior 1.cin_prior

summ offending *score *diff *fs *esem
summ sen cin school_diff_alt ks2_score offending


********************************************************************************
* SECTION 2 — SEN REGRESSIONS
*
* Outcome: any SEN provision during primary school (sen).
* Absorbing LA fixed effects; school FE used as robustness.
* Within-SEN sample: SEN type outcomes (plan, BESD, LD, PD).
********************************************************************************
global census_sen 1.female nsiblings 1.first_born 1.nowhite 1.fsm            ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng               ///
              1.part_time 1.cin_prior

summ sen

* LA fixed effects (main specification)
reghdfe sen semo_esem cogn_esem $census_sen if sen_prior == 0,             ///
    absorb(LA_KS2) vce(cluster school_KS2)

* Quadratic terms: test for non-linearity
reghdfe sen c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census_sen if sen_prior == 0,   ///
    absorb(LA_KS2) vce(cluster school_KS2)

* School fixed effects (robustness)
reghdfe sen c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census_sen if sen_prior == 0,   ///
    absorb(school_KS2) vce(cluster school_KS2)

*--- Within SEN sample: SEN type outcomes ------------------------------------
summ sen_plan sen_besd sen_ld sen_pd if sen == 1 | sen_prior == 1
summ *esem if sen == 1 | sen_prior == 1

tab sen_plan
reghdfe sen_plan semo_esem cogn_esem $census_sen if sen == 1 & sen_prior == 0, ///
    absorb(school_KS2) vce(cluster school_KS2)

tab sen_besd
reghdfe sen_besd semo_esem cogn_esem $census_sen if sen == 1 & sen_prior == 0, ///
    absorb(school_KS2) vce(cluster school_KS2)

tab sen_ld
reghdfe sen_ld semo_esem cogn_esem $census_sen if sen == 1 & sen_prior == 0, ///
    absorb(school_KS2) vce(cluster school_KS2)

tab sen_pd
reghdfe sen_pd semo_esem cogn_esem $census_sen if sen == 1 & sen_prior == 0, ///
    absorb(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 3 — CIN REGRESSIONS
*
* Outcome: Child in Need status during primary school (cin).
* Within-CIN sample: Child Protection Plan (cpp) and Care Leaver (cla).
********************************************************************************
global census_cin 1.female nsiblings 1.first_born 1.nowhite 1.fsm            ///
              ib10.imd_decile i.ybirth i.mbirth 1.lang_neng               ///
              1.part_time 1.sen_prior 

summ cin

reghdfe cin semo_esem cogn_esem $census_cin if cin_prior == 0, ///
    absorb(LA_KS2) vce(cluster school_KS2)

reghdfe cin c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census_cin if cin_prior == 0, ///
    absorb(LA_KS2) vce(cluster school_KS2)

reghdfe cin c.semo_esem##c.semo_esem c.cogn_esem##c.cogn_esem $census_cin if cin_prior == 0, ///
    absorb(school_KS2) vce(cluster school_KS2)

*--- Within CIN sample: CPP and CLA ------------------------------------------
summ cpp cla if cin == 1 | cin_prior == 1
summ *esem if cin == 1 | cin_prior == 1

reghdfe cpp semo_esem cogn_esem $census_cin if cin == 1 & cin_prior == 0,  ///
    absorb(school_KS2) vce(cluster school_KS2)
	
reghdfe cpp_neglect semo_esem cogn_esem $census_cin if cin == 1 & cin_prior == 0,  ///
    absorb(school_KS2) vce(cluster school_KS2)

cpp_abuse = (cpp_emotion == 1 | cpp_physical == 1) & cpp != .
reghdfe cpp_abuse semo_esem cogn_esem $census_cin if cin == 1 & cin_prior == 0,  ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe cla semo_esem cogn_esem $census_cin if cin == 1 & cin_prior == 0,  ///
    absorb(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 4 — SCHOOL DIFFICULTIES (TRANSITIONS AND ABSENCES)
********************************************************************************
summ *esem school_diff_alt share_absences persistent_absent

* School transitions
reghdfe school_diff_alt semo_esem cogn_esem $census,                      ///
    absorb(LA_KS2) vce(cluster school_KS2)

reghdfe school_diff_alt semo_esem cogn_esem $census,                      ///
    absorb(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 5 — SCHOOL EXCLUSION REGRESSIONS
*
* Main spec uses LA FE; robustness uses school FE conditional on schools with
* at least one excluded pupil (max_excluded == 1).
* Within-excluded sample: reason for exclusion.
********************************************************************************

summ excluded

reghdfe excluded semo_esem cogn_esem $census,                             ///
    absorb(LA_KS2) vce(cluster school_KS2)

reghdfe excluded semo_esem cogn_esem $census,                             ///
    absorb(school_KS2) vce(cluster school_KS2)

* Restrict to schools with at least one excluded pupil for school FE model
bys school_KS2: egen max_excluded = max(excluded)
tab max_excluded
summ excluded if max_excluded == 1

reghdfe excluded semo_esem cogn_esem $census if max_excluded == 1,        ///
    absorb(school_KS2) vce(cluster school_KS2)

*--- Within excluded sample: reason for exclusion ----------------------------
summ *esem if excluded == 1
summ physical_assault_exc verbal_abuse_exc misbehaviour_exc if excluded == 1

reghdfe physical_assault_exc semo_esem cogn_esem $census if excluded == 1, ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe verbal_abuse_exc semo_esem cogn_esem $census if excluded == 1,    ///
    absorb(school_KS2) vce(cluster school_KS2)

reghdfe misbehaviour_exc semo_esem cogn_esem $census if excluded == 1,    ///
    absorb(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 6 — KS2 ATTAINMENT REGRESSIONS
********************************************************************************

summ *esem ks2_score ks2_level4_alt

* LA fixed effects
reghdfe ks2_score     semo_esem cogn_esem $census,                        ///
    absorb(LA_KS2) vce(cluster school_KS2)
reghdfe ks2_level4_alt semo_esem cogn_esem $census,                       ///
    absorb(LA_KS2) vce(cluster school_KS2)

* School fixed effects
reghdfe ks2_score     semo_esem cogn_esem $census,                        ///
    absorb(school_KS2) vce(cluster school_KS2)
reghdfe ks2_level4_alt semo_esem cogn_esem $census,                       ///
    absorb(school_KS2) vce(cluster school_KS2)

* Adding school difficulties and exclusion as mediators
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census,   ///
    absorb(school_KS2) vce(cluster school_KS2)
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem            ///
    sen cin $census,                                                       ///
    absorb(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 7 — OFFENDING REGRESSIONS (MAIN SPECIFICATIONS)
*
* Base model: logit with LA FE and clustered SEs.
* School FE model: conditional logit (clogit) grouped by school.
* Sequential specifications add school difficulties, exclusions and KS2 score
* to trace the mediation pathway from early skills to offending.
* Interaction models test heterogeneity by SEN and CIN status.
********************************************************************************

summ *esem offending school_diff_alt ks2_score

* Base logit with LA FE
logit offending semo_esem cogn_esem $census i.LA_KS2,                     ///
    or vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem 1.female 1.nowhite 1.fsm) post

* School FE via conditional logit
clogit offending semo_esem cogn_esem $census,                             ///
    or group(school_KS2) vce(cluster school_KS2)

* Adding school difficulties and exclusions
logit offending semo_esem cogn_esem school_diff_alt excluded $census      ///
    i.LA_KS2, vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem school_diff_alt excluded                ///
              1.female 1.nowhite 1.fsm) post

* Adding KS2 score (full mediation chain)
logit offending semo_esem cogn_esem school_diff_alt excluded              ///
    ks2_score $census i.LA_KS2,                                           ///
    or vce(cluster school_KS2)
margins, dydx(semo_esem cogn_esem school_diff_alt excluded ks2_score      ///
              1.female 1.nowhite 1.fsm) post

* Adding SEN and CIN indicators
logit offending ks2_score school_diff_alt excluded                        ///
    semo_esem cogn_esem sen cin $census i.LA_KS2,                         ///
    or vce(cluster school_KS2)

* Interaction model: heterogeneity by SEN and CIN status
logit offending ks2_score excluded school_diff_alt semo_esem cogn_esem   ///
    c.cogn_esem#i.sen c.semo_esem#i.sen                                   ///
    c.cogn_esem#i.cin c.semo_esem#i.cin                                   ///
    i.sen i.cin $census i.LA_KS2,                                         ///
    or vce(cluster school_KS2)

* Marginal effects at sen = 0, cin = 0
margins, dydx(ks2_score excluded school_diff_alt semo_esem cogn_esem      ///
              1.sen 1.cin 1.female nsiblings 1.first_born                 ///
              1.nowhite 1.fsm ib10.imd_decile)                            ///
    at(sen = 0 cin = 0) post

* Marginal effects at sen = 1, cin = 1
margins, dydx(ks2_score excluded school_diff_alt semo_esem cogn_esem      ///
              i.sen i.cin 1.female nsiblings 1.first_born                 ///
              1.nowhite 1.fsm ib10.imd_decile)                            ///
    at(sen = 1 cin = 1) post

* School FE via conditional logit: restrict to schools with any offending
bys school_KS2: egen max_offending = max(offending)
tab max_offending
summ offending if max_offending == 1

clogit offending ks2_score excluded school_diff_alt semo_esem cogn_esem  ///
    c.cogn_esem#1.sen c.semo_esem#1.sen                                   ///
    c.cogn_esem#1.cin c.semo_esem#1.cin                                   ///
    1.sen 1.cin $census,                                                   ///
    or group(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 8 — OFFENDING TYPE AND COUNT (OFFENDING SAMPLE)
*
* Restricted to offenders (offending == 1).
* Violent offence models use full offending sample.
* Drug, property and summary offence models exclude violent offenders to
* isolate non-violent offending pathways.
* Count model uses negative binomial regression (num_offences_cens).
********************************************************************************

summ *esem school_diff_alt ks2_score excluded if offending == 1
summ violent_offence drugs_offence property_offence summary_offence

*--- Violent offences --------------------------------------------------------
logit violent_offence semo_esem cogn_esem $census i.LA_KS2                ///
    if offending == 1, vce(cluster school_KS2)

logit violent_offence ks2_score school_diff_alt excluded                  ///
    semo_esem cogn_esem $census i.LA_KS2                                  ///
    if offending == 1, vce(cluster school_KS2)

*--- Count of offences -------------------------------------------------------
summ num_offences num_offences_cens

nbreg num_offences_cens semo_esem cogn_esem $census i.LA_KS2              ///
    if offending == 1, vce(cluster school_KS2)
margins, dydx(*esem 1.female nsiblings 1.first_born                       ///
              1.nowhite 1.fsm ib10.imd_decile) post

nbreg num_offences_cens ks2_score school_diff_alt excluded                ///
    semo_esem cogn_esem $census i.LA_KS2                                  ///
    if offending == 1, vce(cluster school_KS2)
margins, dydx(ks2_score school_diff_alt excluded *esem                    ///
              1.female nsiblings 1.first_born                             ///
              1.nowhite 1.fsm ib10.imd_decile) post

*--- Non-violent offences (exclude violent offenders) ------------------------
summ *esem school_diff_alt ks2_score if offending == 1 & violent_offence == 0
summ drugs_offence property_offence summary_offence                       ///
    if offending == 1 & violent_offence == 0

* Drugs and weapons
logit drugs_offence semo_esem cogn_esem $census i.LA_KS2                  ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit drugs_offence ks2_score school_diff_alt excluded                    ///
    semo_esem cogn_esem $census i.LA_KS2                                  ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

* Property offences (theft, damage/arson, public order, fraud)
logit property_offence semo_esem cogn_esem $census i.LA_KS2               ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit property_offence ks2_score school_diff_alt excluded                 ///
    semo_esem cogn_esem $census i.LA_KS2                                  ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

* Summary offences (non-motoring and motoring)
logit summary_offence semo_esem cogn_esem $census i.LA_KS2                ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)

logit summary_offence ks2_score school_diff_alt excluded                  ///
    semo_esem cogn_esem $census i.LA_KS2                                  ///
    if offending == 1 & violent_offence == 0, vce(cluster school_KS2)


********************************************************************************
* SECTION 9 — APPENDIX: SIBLING FIXED EFFECTS
*
* Uses sibling group identifier (groupid_sib) as the absorbed fixed effect.
* Standard errors clustered at the sibling group level.
* $census redefined: ethnicity (nowhite), FSM, IMD and language are dropped
* as they are collinear within sibling groups; female dropped in male-only
* subsample.
********************************************************************************

summ *esem school_diff_alt ks2_score excluded offending if sample_siblings == 1

* Full sibling sample: include female indicator
global census_siblings nsiblings 1.first_born i.ybirth i.mbirth                    ///
              1.part_time 1.sen_prior 1.cin_prior
// CHANGE from baseline: nowhite, fsm, imd_decile, lang_neng dropped
// (collinear within sibling groups); female retained here

reghdfe offending semo_esem cogn_esem 1.female $census_siblings ///
    if sample_siblings == 1,                                               ///
    absorb(groupid_sib) vce(cluster groupid_sib)

* Male subsample: drop female indicator
// CHANGE: female dropped (male-only subsample)
reghdfe offending semo_esem cogn_esem $census_siblings ///
    if sample_siblings == 1 & female == 0,                                ///
    absorb(groupid_sib) vce(cluster groupid_sib)

reghdfe offending ks2_score school_diff_alt excluded semo_esem cogn_esem  ///
    $census_siblings if sample_siblings == 1 & female == 0,                        ///
    absorb(groupid_sib) vce(cluster groupid_sib)

********************************************************************************
* SECTION 10 — APPENDIX: BIFACTOR MODEL
*
* Uses bifactor factor scores (gend_bfs, cogn_bfs, semo_bfs) as alternative
* measurement specification.  School FE via conditional logit throughout.
********************************************************************************
summ *bfs school_diff_alt ks2_score

clogit offending gend_bfs $census,                                        ///
    or group(school_KS2) vce(cluster school_KS2)

clogit offending *bfs $census,                                            ///
    or group(school_KS2) vce(cluster school_KS2)

clogit offending ks2_score school_diff_alt excluded *bfs $census,         ///
    or group(school_KS2) vce(cluster school_KS2)

clogit offending ks2_score school_diff_alt excluded *bfs sen cin $census, ///
    or group(school_KS2) vce(cluster school_KS2)

* Interaction model: heterogeneity by SEN and CIN
clogit offending ks2_score school_diff_alt excluded *bfs                  ///
    (c.gend_bfs c.cogn_bfs c.semo_bfs)#1.sen                             ///
    (c.gend_bfs c.cogn_bfs c.semo_bfs)#1.cin                             ///
    1.sen 1.cin $census,                                                   ///
    or group(school_KS2) vce(cluster school_KS2)


********************************************************************************
* SECTION 11 — APPENDIX: SAMPLE SELECTION AND IPW
*
* Compares unweighted and IPW-weighted estimates to assess attrition bias.
* Two IPW variants: individual-level (ipw) and school-level (ipw_school).
* For each outcome, the unweighted model is restricted to observations with
* non-missing IPW weights (if ipw != .) for a like-for-like comparison.
********************************************************************************

summ ipw ipw_school
corr ipw ipw_school

*--- School exclusions -------------------------------------------------------
reghdfe excluded semo_esem cogn_esem $census                              ///
    if ipw != ., absorb(school_KS2) vce(cluster school_KS2)
reghdfe excluded semo_esem cogn_esem $census                              ///
    [pw = ipw], absorb(school_KS2) vce(cluster school_KS2)

*--- School difficulties -----------------------------------------------------
reghdfe school_diff_alt semo_esem cogn_esem $census                       ///
    if ipw != ., absorb(school_KS2) vce(cluster school_KS2)
reghdfe school_diff_alt semo_esem cogn_esem $census                       ///
    [pw = ipw], absorb(school_KS2) vce(cluster school_KS2)

*--- KS2 attainment ----------------------------------------------------------
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census    ///
    if ipw != ., absorb(school_KS2) vce(cluster school_KS2)
reghdfe ks2_score school_diff_alt excluded semo_esem cogn_esem $census    ///
    [pw = ipw], absorb(school_KS2) vce(cluster school_KS2)

*--- Offending ---------------------------------------------------------------
logit offending ks2_score school_diff_alt excluded semo_esem cogn_esem    ///
    $census i.LA_KS2                                                       ///
    if ipw != ., or vce(cluster school_KS2)

logit offending ks2_score school_diff_alt excluded semo_esem cogn_esem    ///
    $census i.LA_KS2                                                       ///
    [pw = ipw], or vce(cluster school_KS2)
margins, dydx(ks2_score school_diff_alt excluded semo_esem cogn_esem      ///
              1.female nsiblings 1.first_born                             ///
              1.nowhite 1.fsm ib10.imd_decile) post

********************************************************************************
* END OF FILE
********************************************************************************
