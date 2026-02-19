********************************************************************************
* Project: Socio-Emotional Characteristics in Early Childhood and Offending
*          Behaviour in Adolescence
* Author:  Paul Garcia
* Institution: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
* Description: Exploratory Factor Analysis (EFA) on EYFSP binary items to
*              determine the number of factors and item selection for the final
*              ESEM specification. Uses tetrachoric correlations (appropriate
*              for binary items) and oblique rotation (quartimin) to allow
*              factors to correlate. Items are progressively dropped based on
*              high inter-item correlations (>=0.85) and weak/cross-loadings.
*              Two separate factorization strategies are explored:
*              (1) All EYFSP domains (6 domains, cognitive + socio-emotional)
*              (2) Two-domain solution (cognitive + socio-emotional only)
********************************************************************************

clear all

use "file_path\eyfsp_clean.dta", clear

* Keep only EYFSP item-level variables and key identifiers
keep id_dfe id_moj att* emo* soc* lang* link* read* write* num* cal* ///
     space* know* phy* cre* missing_EYFSP score_eyfsp_overall

* Restrict to pupils with complete item-level information
keep if missing_EYFSP == 0

* Merge with main analysis dataset to align sample with estimation sample
* Keeps offending outcome, missing data indicator, and school LA code at year 6
merge 1:1 id_dfe using "file_path\clean_data.dta", ///
    keepusing(missing_vars offending LA_school_code_yr6)
keep if _merge == 3
drop _merge

* Apply same sample restriction as main analysis
tab missing_vars
keep if missing_vars == 0

* Drop pupils with zero overall EYFSP score (not evaluated / no valid assessment)
count if score_eyfsp_overall == 0
drop if score_eyfsp_overall == 0
summ score_eyfsp_overall

********************************************************************************
* SECTION 1: FACTORIZATION USING ALL EYFSP DOMAINS
* Includes all six EYFSP learning areas: socio-emotional (PSE), language (CLL),
* numeracy (PSRN), knowledge of the world, physical, and creative development.
* Items dropped iteratively based on: (i) tetrachoric correlations >= 0.85
* (near-perfect collinearity), and (ii) weak or ambiguous factor loadings.
********************************************************************************

*------------------------------------------------------------------------------
* EFA Round 1 — All domains, 56 items
* Starting point: 65 items, dropping 9 with correlations >= 0.85
* Dropped: soc8, link5, link7, link8, write8, num6, num7, cal5, cal8
*------------------------------------------------------------------------------
tetrachoric att4 att5 att6 att7 att8 soc4 soc5 soc6 soc7 emo4 emo5 emo6 emo7 ///
    emo8 lang4 lang5 lang6 lang7 lang8 link4 link6 read4 read5 read6 read7     ///
    read8 write4 write5 write6 write7 num4 num5 num8 cal4 cal6 cal7 space4     ///
    space5 space6 space7 space8 know4 know5 know6 know7 know8 phy4 phy5 phy6   ///
    phy7 phy8 cre4 cre5 cre6 cre7 cre8

* Store sample size and tetrachoric correlation matrix for factormat
display r(N)
global N = r(N)
matrix r = r(Rho)

* Factor analysis on correlation matrix (factormat used since tetrachoric
* returns matrix rather than raw data; oblique rotation allows factor correlation)
factormat r, n($N) factors(2)
rotate, oblique quartimin

*------------------------------------------------------------------------------
* EFA Round 2 — All domains, 36 items
* Dropped 20 items with weak or cross-loadings from Round 1
*------------------------------------------------------------------------------
tetrachoric att7 att8 soc4 soc5 soc6 soc7 emo4 emo5 emo7 emo8 lang7 lang8    ///
    link4 link6 read4 read5 read6 write4 write5 write6 write7 num4 num5 num8   ///
    cal4 cal6 cal7 space4 space5 space6 space8 know7 know8 phy8 cre7 cre8

display r(N)
global N = r(N)
matrix r = r(Rho)

* Note: 3 factors requested to check whether a third factor emerges;
* scree plot confirms only 2 meaningful factors
factormat r, n($N) factors(3)
rotate, oblique quartimin

*------------------------------------------------------------------------------
* EFA Round 3 — All domains, 33 items
* Dropped 3 further items from Round 2
*------------------------------------------------------------------------------
tetrachoric att7 att8 soc4 soc5 soc6 soc7 emo4 emo5 emo7 emo8 lang8 link4    ///
    link6 read4 read5 read6 write4 write5 write6 write7 num4 num5 num8 cal4    ///
    cal6 cal7 space4 space5 space6 space8 phy8 cre7 cre8

display r(N)
global N = r(N)
matrix r = r(Rho)

factormat r, n($N) factors(2)
rotate, oblique quartimin

*------------------------------------------------------------------------------
* EFA Round 4 (Final) — All domains, 27 items
* Dropped 5 further items; att7 and att8 commented out for sensitivity check
* This gives the final item set for the all-domain factorization
*------------------------------------------------------------------------------
tetrachoric soc4 soc5 soc6 soc7 emo4 emo5 emo7 emo8 link4 link6 read4 read5  ///
    read6 write4 write5 write6 write7 num4 num5 num8 cal4 cal6 cal7 space4    ///
    space5 space6 space8
* att7 att8 excluded from final solution (sensitivity check: add back to assess impact)

display r(N)
global N = r(N)
matrix r = r(Rho)

factormat r, n($N) factors(2)
rotate, oblique quartimin

* Scree plot to visualise eigenvalues and confirm 2-factor solution
* yline(1) marks the Kaiser criterion (eigenvalue > 1)
screeplot, yline(1) lcolor(black) mcolor(black) title("") ///
    ytitle(, size(med)) graphregion(fcolor(white)) xtitle("Factors", size(med))

* Internal consistency (Cronbach's alpha) for each proposed factor
* Cognitive items
alpha write7 read6 num5 cal7 write4 link6 num8 cal6 cal4 read4 link4 space8 ///
      num4 write6 space6 read5 write5 space4 space5

* Socio-emotional items
alpha emo5 soc5 soc4 soc6 emo7 emo4 emo8 soc7

********************************************************************************
* SECTION 2: FACTORIZATION USING TWO DOMAINS ONLY (Cognitive + Socio-emotional)
* Restricts item pool to PSE (socio-emotional) and CLL/PSRN (cognitive) domains.
* Excludes knowledge of the world, physical, and creative items.
* This is the specification used in the final ESEM model.
********************************************************************************

*------------------------------------------------------------------------------
* EFA Round 1 — Two domains, 36 items
* Starting point: 45 items, dropping 9 with correlations >= 0.85
* Same high-correlation exclusions as Section 1
*------------------------------------------------------------------------------
tetrachoric att4 att5 att6 att7 att8 soc4 soc5 soc6 soc7 emo4 emo5 emo6 emo7 ///
    emo8 lang4 lang5 lang6 lang7 lang8 link4 link6 read4 read5 read6 read7    ///
    read8 write4 write5 write6 write7 num4 num5 num8 cal4 cal6 cal7

display r(N)
global N = r(N)
matrix r = r(Rho)

* 3 factors requested to check robustness; one eigenvalue is close to 1
* but no items load exclusively on a third factor
factormat r, n($N) factors(3)
rotate, oblique quartimin

*------------------------------------------------------------------------------
* EFA Round 2 (Final) — Two domains, 32 items
* Dropped 4 items from Round 1 with weak/ambiguous loadings
* This is the final item set used in the ESEM specification
*------------------------------------------------------------------------------
tetrachoric att4 att5 att6 soc4 soc5 soc6 soc7 emo4 emo5 emo6 emo7 emo8     ///
    lang4 lang5 lang6 link4 link6 read4 read5 read6 read7 read8 write4 write5 ///
    write6 write7 num4 num5 num8 cal4 cal6 cal7

display r(N)
global N = r(N)
matrix r = r(Rho)

* Two-factor solution confirmed: third eigenvalue does not represent
* a meaningful independent factor
factormat r, n($N) factors(2)
rotate, oblique quartimin

* Scree plot for final two-domain solution
screeplot, yline(1) title("") ytitle(, size(med)) ///
    graphregion(fcolor(white)) xtitle("Factor", size(med))

* Internal consistency (Cronbach's alpha) for final item sets
* Cognitive factor items (from two-domain solution)
alpha write7 read6 cal7 num5 link6 num8 write4 cal6 read8 cal4 read4 link4 ///
      read7 write6 num4 read5 write5

* Socio-emotional factor items (from two-domain solution)
alpha soc5 emo5 soc4 emo4 soc6 emo7 att5 lang6 att6 emo6 emo8 lang5 att4 soc7 lang4