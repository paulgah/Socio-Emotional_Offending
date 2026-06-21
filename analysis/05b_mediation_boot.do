********************************************************************************
* Project : Early Socio-Emotional Skills and Adolescent Offending:
*           Evidence from Administrative Data
* Author  : Paul Garcia
* Institute: Institute for Social and Economic Research (ISER), University of Essex
* Fellowship: ADR UK (ESRC) Research Fellowship
*
* File    : 05b_mediation_boot.do
* Purpose : Inner loop of the bootstrap procedure for the mediation analysis.
*           Called on each bootstrap iteration `n' from the main regression
*           script. Estimates reduced-form and structural equations; computes
*           Level 1 and Level 2 indirect effects for cognitive and
*           socio-emotional skill factors on offending; stores coefficients
*           in coeff.dta for later inference.
*
*           Two decompositions are estimated:
*             Model 1 (gamma): school pathways only
*                              (absences, exclusions, KS2)
*             Model 2 (alpha): school pathways + vulnerability mediators
*                              (absences, exclusions, KS2, SEN, CIN)
*
* Globals : `skills'    — cogn_esem semo_esem (set in calling script)
*           `controls'  — pupil-level controls (set in calling script)
*           `n'         — current bootstrap iteration
*           `iteration' — total number of iterations
*           `path'      — root path for data (set in calling script)
*
* Notes   : This file is not run standalone. It is called from 12_main.do
*           within a bootstrap loop.
********************************************************************************


********************************************************************************
* PART A — MODEL 1: SCHOOL PATHWAYS ONLY (gamma scalars)
********************************************************************************

*------------------------------------------------------------------------------
* A.1  First-stage regressions (mediators on skills)
*------------------------------------------------------------------------------

* Absences
reghdfe absences_score `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_cA  = _b[cogn_esem]
scalar gamma_sA  = _b[semo_esem]

* Exclusions (socio-emotional only)
reghdfe excluded semo_esem `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_sE  = _b[semo_esem]

* KS2 score (cognitive only)
reghdfe ks2_score cogn_esem `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_cKS = _b[cogn_esem]

*------------------------------------------------------------------------------
* A.2  Reduced-form offending on skills (no mediators)
*------------------------------------------------------------------------------
reghdfe offending `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_cO  = _b[cogn_esem]
scalar gamma_sO  = _b[semo_esem]

*------------------------------------------------------------------------------
* A.3  Structural offending equation (mediators included)
*------------------------------------------------------------------------------
reghdfe offending ks2_score absences_score excluded `skills' `controls', ///
	absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_KS_O = _b[ks2_score]
scalar gamma_A_O  = _b[absences_score]
scalar gamma_E_O  = _b[excluded]
scalar gamma_C_O  = _b[cogn_esem]
scalar gamma_S_O  = _b[semo_esem]

*------------------------------------------------------------------------------
* A.4  Level 1 indirect effects (proportion of total effect via each mediator)
*
* Cognitive:
*   Via absences:   (gamma_cA * gamma_A_O) / gamma_cO
*   Via exclusions: gamma_E_O / gamma_cO
*   NOTE: exclusions effect on cognitive is just gamma_E_O/gamma_cO with
*   no first-stage scalar in the numerator. This is because cogn_esem is not
*   included in the exclusions equation (semo_esem only), so the cognitive
*   indirect effect via exclusions is zero at Level 1.
*   Via KS2:        (gamma_cKS * gamma_KS_O) / gamma_cO
*
* Socio-emotional:
*   Via absences:   (gamma_sA * gamma_A_O) / gamma_sO
*   Via exclusions: (gamma_sE * gamma_E_O) / gamma_sO
*   Via KS2:        gamma_KS_O / gamma_sO
*   NOTE: KS2 effect on socio-emotional has no first-stage scalar —
*   symmetric to the cognitive/exclusions note above. 
*------------------------------------------------------------------------------
scalar b1_cA_O   = (gamma_cA  * gamma_A_O)  / gamma_cO
scalar b1_cE_O   = (gamma_E_O)              / gamma_cO
scalar b1_cKS_O  = (gamma_cKS * gamma_KS_O) / gamma_cO

scalar b1_sA_O   = (gamma_sA  * gamma_A_O)  / gamma_sO
scalar b1_sE_O   = (gamma_sE  * gamma_E_O)  / gamma_sO
scalar b1_sKS_O  = (gamma_KS_O)             / gamma_sO

*------------------------------------------------------------------------------
* A.5  Level 2 indirect effects (via KS2, absorbing mediation through absences
*      and exclusions → KS2 → offending)
*------------------------------------------------------------------------------
reghdfe ks2_score absences_score excluded cogn_esem `controls', ///
	absorb(school_KS2) vce(cluster school_KS2)
scalar gamma_A_KS = _b[absences_score]
scalar gamma_E_KS = _b[excluded]
scalar gamma_c_KS = _b[cogn_esem]

scalar b1_cA_KS_O  = b1_cA_O  + (gamma_KS_O * gamma_cA * gamma_A_KS) / gamma_cO
scalar b1_cE_KS_O  = b1_cE_O  + (gamma_KS_O * gamma_E_KS)             / gamma_cO

scalar b1_sA_KS_O  = b1_sA_O  + (gamma_KS_O * gamma_sA * gamma_A_KS) / gamma_sO
scalar b1_sE_KS_O  = b1_sE_O  + (gamma_KS_O * gamma_sE * gamma_E_KS) / gamma_sO

* Share of KS2 effect unexplained by absences/exclusions
scalar b1_cKS_nomed    = gamma_c_KS / gamma_cKS
scalar b1_cKS_O_total  = gamma_KS_O * gamma_cKS / gamma_cO
scalar b1_cKS_O_unex   = b1_cKS_nomed * b1_cKS_O_total

********************************************************************************
* PART B — MODEL 2: SCHOOL PATHWAYS + VULNERABILITY MEDIATORS (alpha scalars)
********************************************************************************

*------------------------------------------------------------------------------
* B.1  First-stage regressions (mediators on skills)
*------------------------------------------------------------------------------

* SEN
reghdfe sen `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_cSEN = _b[cogn_esem]
scalar alpha_sSEN = _b[semo_esem]

* CIN
reghdfe cin `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_cCIN = _b[cogn_esem]
scalar alpha_sCIN = _b[semo_esem]

* Absences
reghdfe absences_score `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_cA  = _b[cogn_esem]
scalar alpha_sA  = _b[semo_esem]

* Exclusions (socio-emotional only)
reghdfe excluded semo_esem `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_sE  = _b[semo_esem]

* KS2 score (cognitive only)
reghdfe ks2_score cogn_esem `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_cKS = _b[cogn_esem]

*------------------------------------------------------------------------------
* B.2  Reduced-form offending on skills (no mediators)
*------------------------------------------------------------------------------
reghdfe offending `skills' `controls', absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_cO  = _b[cogn_esem]
scalar alpha_sO  = _b[semo_esem]

*------------------------------------------------------------------------------
* B.3  Structural offending equation (all mediators)
*------------------------------------------------------------------------------
reghdfe offending ks2_score absences_score excluded `skills' sen cin `controls', ///
	absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_KS_O  = _b[ks2_score]
scalar alpha_A_O   = _b[absences_score]
scalar alpha_E_O   = _b[excluded]
scalar alpha_C_O   = _b[cogn_esem]
scalar alpha_S_O   = _b[semo_esem]
scalar alpha_SEN_O = _b[sen]
scalar alpha_CIN_O = _b[cin]

*------------------------------------------------------------------------------
* B.4  Level 1 indirect effects
*------------------------------------------------------------------------------
scalar b2_cA_O    = (alpha_cA   * alpha_A_O)   / alpha_cO
scalar b2_cE_O    = (alpha_E_O)                / alpha_cO
scalar b2_cKS_O   = (alpha_cKS  * alpha_KS_O)  / alpha_cO
scalar b2_cSEN_O  = (alpha_cSEN * alpha_SEN_O) / alpha_cO
scalar b2_cCIN_O  = (alpha_cCIN * alpha_CIN_O) / alpha_cO

scalar b2_sA_O    = (alpha_sA   * alpha_A_O)   / alpha_sO
scalar b2_sE_O    = (alpha_sE   * alpha_E_O)   / alpha_sO
scalar b2_sKS_O   = (alpha_KS_O)               / alpha_sO
scalar b2_sSEN_O  = (alpha_sSEN * alpha_SEN_O) / alpha_sO
scalar b2_sCIN_O  = (alpha_sCIN * alpha_CIN_O) / alpha_sO

*------------------------------------------------------------------------------
* B.5  Level 2 indirect effects
*------------------------------------------------------------------------------
reghdfe ks2_score absences_score excluded cogn_esem sen cin `controls', ///
	absorb(school_KS2) vce(cluster school_KS2)
scalar alpha_A_KS   = _b[absences_score]
scalar alpha_E_KS   = _b[excluded]
scalar alpha_c_KS   = _b[cogn_esem]
scalar alpha_SEN_KS = _b[sen]
scalar alpha_CIN_KS = _b[cin]

scalar b2_cA_KS_O   = b2_cA_O   + (alpha_KS_O * alpha_cA   * alpha_A_KS)   / alpha_cO
scalar b2_cE_KS_O   = b2_cE_O   + (alpha_KS_O * alpha_E_KS)                / alpha_cO
scalar b2_cSEN_KS_O = b2_cSEN_O + (alpha_KS_O * alpha_cSEN * alpha_SEN_KS) / alpha_cO
scalar b2_cCIN_KS_O = b2_cCIN_O + (alpha_KS_O * alpha_cCIN * alpha_CIN_KS) / alpha_cO

scalar b2_sA_KS_O   = b2_sA_O   + (alpha_KS_O * alpha_sA   * alpha_A_KS)   / alpha_sO
scalar b2_sE_KS_O   = b2_sE_O   + (alpha_KS_O * alpha_sE   * alpha_E_KS)   / alpha_sO
scalar b2_sSEN_KS_O = b2_sSEN_O + (alpha_KS_O * alpha_sSEN * alpha_SEN_KS) / alpha_sO
scalar b2_sCIN_KS_O = b2_sCIN_O + (alpha_KS_O * alpha_sCIN * alpha_CIN_KS) / alpha_sO

scalar b2_cKS_nomed   = alpha_c_KS / alpha_cKS
scalar b2_cKS_O_total = alpha_KS_O * alpha_cKS / alpha_cO
scalar b2_cKS_O_unex  = b2_cKS_nomed * b2_cKS_O_total

********************************************************************************
* PART C — STORE BOOTSTRAP COEFFICIENTS
********************************************************************************

*------------------------------------------------------------------------------
* C.1  On first iteration: initialise coeff.dta
*------------------------------------------------------------------------------
if `n' == 1 {
	clear
	set obs `iteration'
	gen iteration = .

	* Model 1 totals
	gen gamma_cO = .
	gen gamma_sO = .

	* Model 2 totals
	gen alpha_cO = .
	gen alpha_sO = .

	* Model 2 SEN/CIN indirect effects
	foreach stub in b2_cSEN_O b2_cCIN_O b2_sSEN_O b2_sCIN_O ///
	                b2_cSEN_KS_O b2_cCIN_KS_O b2_sSEN_KS_O b2_sCIN_KS_O {
		gen `stub' = .
	}

	* Indirect effect scalars for both models
	forval i = 1/2 {
		foreach stub in ///
			b`i'_cA_O b`i'_cE_O b`i'_cKS_O ///
			b`i'_sA_O b`i'_sE_O b`i'_sKS_O ///
			b`i'_cA_KS_O b`i'_cE_KS_O ///
			b`i'_sA_KS_O b`i'_sE_KS_O ///
			b`i'_cKS_nomed b`i'_sKS_nomed ///
			b`i'_cKS_O_total b`i'_sKS_O_total ///
			b`i'_cKS_O_unex  b`i'_sKS_O_unex {
			gen `stub' = .
		}
	}

	save "`path'/data/coeff.dta", replace
}

*------------------------------------------------------------------------------
* C.2  Write current iteration's scalars into coeff.dta
*------------------------------------------------------------------------------
use "`path'/data/coeff.dta", clear

replace iteration  = `n'               in `n'
replace gamma_cO   = scalar(gamma_cO)  in `n'
replace gamma_sO   = scalar(gamma_sO)  in `n'
replace alpha_cO   = scalar(alpha_cO)  in `n'
replace alpha_sO   = scalar(alpha_sO)  in `n'

foreach stub in b2_cSEN_O b2_cCIN_O b2_sSEN_O b2_sCIN_O ///
                b2_cSEN_KS_O b2_cCIN_KS_O b2_sSEN_KS_O b2_sCIN_KS_O {
	replace `stub' = scalar(`stub') in `n'
}

forval i = 1/2 {
	foreach stub in ///
		b`i'_cA_O b`i'_cE_O b`i'_cKS_O ///
		b`i'_sA_O b`i'_sE_O b`i'_sKS_O ///
		b`i'_cA_KS_O b`i'_cE_KS_O ///
		b`i'_sA_KS_O b`i'_sE_KS_O ///
		b`i'_cKS_nomed b`i'_sKS_nomed ///
		b`i'_cKS_O_total b`i'_sKS_O_total ///
		b`i'_cKS_O_unex  b`i'_sKS_O_unex {
		replace `stub' = scalar(`stub') in `n'
	}
}

*------------------------------------------------------------------------------
* C.3  Order variables and save
*------------------------------------------------------------------------------
order ///
	gamma_cO  b1_cA_O b1_cE_O b1_cKS_O b1_cA_KS_O b1_cE_KS_O ///
	          b1_cKS_nomed b1_cKS_O_total b1_cKS_O_unex ///
	gamma_sO  b1_sA_O b1_sE_O b1_sKS_O b1_sA_KS_O b1_sE_KS_O ///
	          b1_sKS_nomed b1_sKS_O_total b1_sKS_O_unex ///
	alpha_cO  b2_cA_O b2_cE_O b2_cKS_O b2_cSEN_O b2_cCIN_O ///
	          b2_cA_KS_O b2_cE_KS_O b2_cSEN_KS_O b2_cCIN_KS_O ///
	          b2_cKS_nomed b2_cKS_O_total b2_cKS_O_unex ///
	alpha_sO  b2_sA_O b2_sE_O b2_sKS_O b2_sSEN_O b2_sCIN_O ///
	          b2_sA_KS_O b2_sE_KS_O b2_sSEN_KS_O b2_sCIN_KS_O ///
	          b2_sKS_nomed b2_sKS_O_total b2_sKS_O_unex

save "`path'/data/coeff.dta", replace

********************************************************************************
* END OF FILE
********************************************************************************
