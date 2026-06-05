********************************************************************************
* Project : Socio-Emotional Characteristics in Early Childhood and
*           Offending Behaviour in Adolescence
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 03_eyfsp_efa.do
* Purpose : Exploratory Factor Analysis (EFA) of EYFSP binary items using
*           tetrachoric correlations.  Two parallel EFA streams are run:
*
*           Stream A — All 65 items (PSE + CLL + PSRN + KUW + PD + CD)
*                      iteratively reduced to 28 items; two-factor solution.
*           Stream B — 45 items restricted to PSE + CLL + PSRN domains only
*                      iteratively reduced to 32 items; two-factor solution.
*
*           Both streams use oblique quartimin rotation.  Item reduction at
*           each step removes items with inter-item tetrachoric correlations
*           >= 0.85 (near-redundant) or cross-loadings that prevent clean
*           factor separation.  Scree plots and Cronbach's alpha checks are
*           included as diagnostics; both are exploratory and not reported
*           in the paper.
*
* Inputs  : eyfsp_clean.dta
*           clean_data.dta  (for sample restriction: missing_vars, offending,
*                            LA_school_code_yr6)
*
* Notes   : factormat requires the tetrachoric correlation matrix r(Rho) and
*           the sample size r(N); r(N) is stored as global $N immediately after
*           tetrachoric and before factormat to ensure the correct value is used.
*           sortl and rotate clear are commented out throughout — sortl reorders
*           displayed loadings by size but does not affect stored results or
*           downstream factor score extraction; rotate clear would discard the
*           rotation and is retained only as a reference.
********************************************************************************

clear all
set more off

********************************************************************************
* SECTION 1 — SAMPLE RESTRICTIONS
********************************************************************************

use "file_path\eyfsp_clean.dta", clear

keep id_dfe id_moj ///
     att* emo* soc* lang* link* read* write* num* cal* space* ///
     know* phy* cre* ///
     missing_EYFSP score_eyfsp_overall

* Restrict to complete item responses
keep if missing_EYFSP == 0

* Merge with analysis sample to apply consistent sample restrictions
merge 1:1 id_dfe using "file_path\clean_data.dta", ///
    keepusing(missing_vars offending LA_school_code_yr6)
keep if _merge == 3
drop _merge

tab missing_vars
keep if missing_vars == 0

* Drop children with a zero overall EYFSP score (not evaluated)
count if score_eyfsp_overall == 0
drop  if score_eyfsp_overall == 0
summ score_eyfsp_overall


********************************************************************************
* SECTION 2 — STREAM A: EFA ACROSS ALL ITEMS
*
* Starting pool: 65 items spanning all six EYFSP domains.
* Items with tetrachoric correlations >= 0.85 dropped first (near-redundant).
* Subsequent rounds drop items with weak or cross-loading factor structure.
* Four rounds of EFA; final solution retains 28 items loading on two factors:
*   Factor 1 — Cognitive / academic skills (literacy and numeracy items)
*   Factor 2 — Socio-emotional skills (PSE items)
********************************************************************************

*------------------------------------------------------------------------------
* Stream A — Round 1: 65 items → 56 items
* Dropped due to correlations >= 0.85: soc8 link5 link7 link8 write8
*                                       num6 num7 cal5 cal8
*------------------------------------------------------------------------------
tetrachoric ///
    att4 att5 att6 att7 att8 ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo6 emo7 emo8 ///
    lang4 lang5 lang6 lang7 lang8 ///
    link4 link6 ///
    read4 read5 read6 read7 read8 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7 ///
    space4 space5 space6 space7 space8 ///
    know4 know5 know6 know7 know8 ///
    phy4 phy5 phy6 phy7 phy8 ///
    cre4 cre5 cre6 cre7 cre8

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(2)
rotate, oblique quartimin
*sortl       // reorders displayed loadings by size; does not affect stored results
*rotate clear

*------------------------------------------------------------------------------
* Stream A — Round 2: 56 items → 36 items
* Dropped items with weak loadings or cross-loadings preventing clean separation
*------------------------------------------------------------------------------
tetrachoric ///
    att7 att8 ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo7 emo8 ///
    lang7 lang8 ///
    link4 link6 ///
    read4 read5 read6 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7 ///
    space4 space5 space6 space8 ///
    know7 know8 ///
    phy8 ///
    cre7 cre8

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(3)   // Scree plot indicates two factors; third tested for robustness
rotate, oblique quartimin
*sortl
*rotate clear

*------------------------------------------------------------------------------
* Stream A — Round 3: 36 items → 33 items
* Dropped: lang7, know7, know8 (low communality / cross-loading)
*------------------------------------------------------------------------------
tetrachoric ///
    att7 att8 ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo7 emo8 ///
    lang8 ///
    link4 link6 ///
    read4 read5 read6 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7 ///
    space4 space5 space6 space8 ///
    phy8 ///
    cre7 cre8

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(2)
rotate, oblique quartimin
*sortl
*rotate clear

*------------------------------------------------------------------------------
* Stream A — Round 4 (final): 33 items → 28 items
* Dropped: att7, att8, lang8, phy8, cre7, cre8
* (cross-load across both factors; do not contribute to clean structure)
*
* Final two-factor solution:
*   Factor 1 (Cognitive)      : link4 link6 read4–read6 write4–write7
*                                num4 num5 num8 cal4 cal6 cal7
*                                space4–space6 space8
*   Factor 2 (Socio-emotional): soc4–soc7 emo4 emo5 emo7 emo8
*------------------------------------------------------------------------------
tetrachoric ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo7 emo8 ///
    link4 link6 ///
    read4 read5 read6 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7 ///
    space4 space5 space6 space8
    // att7 att8 excluded — see note above

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(2)
rotate, oblique quartimin
*sortl
*rotate clear

* Scree plot for final Stream A solution (exploratory; not reported in paper)
screeplot, yline(1) lcolor(black) mcolor(black) ///
    title("") ytitle(, size(med)) xtitle("Factors", size(med)) ///
    graphregion(fcolor(white))

* Cronbach's alpha as internal consistency check for each factor (exploratory)
* Factor 1 — Cognitive / academic items
alpha write7 read6 num5 cal7 write4 link6 num8 cal6 cal4 read4 link4 ///
      space8 num4 write6 space6 read5 write5 space4 space5

* Factor 2 — Socio-emotional items
alpha emo5 soc5 soc4 soc6 emo7 emo4 emo8 soc7


********************************************************************************
* SECTION 3 — STREAM B: EFA RESTRICTED TO PSE + CLL + PSRN DOMAINS
*
* Starting pool: 45 items (PSE + CLL + PSRN only; KUW, PD, CD excluded).
* Same item-reduction logic as Stream A.
* Two rounds of EFA; final solution retains 32 items on two factors.
* Mirrors Stream A factor structure within the three core scored domains.
********************************************************************************

*------------------------------------------------------------------------------
* Stream B — Round 1: 45 items → 36 items
* Dropped due to correlations >= 0.85: soc8 link5 link7 link8 write8
*                                       num6 num7 cal5 cal8  (same as Stream A)
*------------------------------------------------------------------------------
tetrachoric ///
    att4 att5 att6 att7 att8 ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo6 emo7 emo8 ///
    lang4 lang5 lang6 lang7 lang8 ///
    link4 link6 ///
    read4 read5 read6 read7 read8 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(3)
rotate, oblique quartimin
*sortl
*rotate clear

*------------------------------------------------------------------------------
* Stream B — Round 2 (final): 36 items → 32 items
* Dropped: att7, att8, lang7, lang8
* (att7/att8 cross-load; lang7/lang8 load weakly on both factors)
*
* NOTE: a third eigenvalue hovers around 1.0 but no items load cleanly on a
*       third factor; two-factor solution adopted.
*
* Final two-factor solution:
*   Factor 1 (Cognitive)      : lang4–lang6 link4 link6 read4–read8
*                                write4–write7 num4 num5 num8 cal4 cal6 cal7
*   Factor 2 (Socio-emotional): att4–att6 soc4–soc7 emo4–emo8 lang4–lang6
*------------------------------------------------------------------------------
tetrachoric ///
    att4 att5 att6 ///
    soc4 soc5 soc6 soc7 ///
    emo4 emo5 emo6 emo7 emo8 ///
    lang4 lang5 lang6 ///
    link4 link6 ///
    read4 read5 read6 read7 read8 ///
    write4 write5 write6 write7 ///
    num4 num5 num8 ///
    cal4 cal6 cal7

global N = r(N)
matrix r = r(Rho)
factormat r, n($N) factors(2)   // Third eigenvalue ~1 but no clean third factor
rotate, oblique quartimin
*sortl
*rotate clear

* Scree plot for final Stream B solution (exploratory; not reported in paper)
screeplot, yline(1) ///
    title("") ytitle(, size(med)) xtitle("Factor", size(med)) ///
    graphregion(fcolor(white))

* Cronbach's alpha as internal consistency check (exploratory)
* Factor 1 — Cognitive / academic items
alpha write7 read6 cal7 num5 link6 num8 write4 cal6 read8 cal4 read4 link4 ///
      read7 write6 num4 read5 write5

* Factor 2 — Socio-emotional items
alpha soc5 emo5 soc4 emo4 soc6 emo7 att5 lang6 att6 emo6 emo8 lang5 att4 soc7 lang4

********************************************************************************
* END OF FILE
********************************************************************************
