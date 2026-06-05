################################################################################
# Project : Socio-Emotional Characteristics in Early Childhood and
#           Offending Behaviour in Adolescence
# Author  : Paul Garcia
# Institute: Institute for Social and Economic Research (ISER), University of Essex
# Fellowship: ADR UK (ESRC) Research Fellowship
#
# File    : 03_esem_eyfsp.R
# Purpose : Estimate an Exploratory Structural Equation Model (ESEM) for the
#           EYFSP items identified in the EFA (03_eyfsp_efa.do).
#
#           Steps:
#           1. Re-estimate the EFA tetrachoric factor solution in R (geominQ,
#              two factors) to obtain starting values for the ESEM.
#           2. Construct the ESEM model syntax from EFA loadings using
#              target-rotation anchor constraints.
#           3. Fit the ESEM as a CFA with WLSMV estimator (ordered indicators).
#           4. Extract EBM factor scores and measurement-error variance
#              (used downstream in SIMEX correction).
#           5. Save factor scores to Stata .dta.
#
# Inputs  : clean_data.dta    (sample restrictions: missing_vars)
#           eyfsp_clean.dta   (EYFSP item-level data)
#
# Outputs : fs_esem.dta       (id_dfe + cogn_esem + semo_esem factor scores)
#           ESEM_EYFSP.txt    (sink log: model summary, fit indices, loadings)
#
# Factor labelling:
#   cogn_esem — Cognitive / academic skills (literacy and numeracy items)
#   semo_esem — Socio-emotional skills (PSE items)
#
# Notes   : Anchor variables fix the cross-loadings for identification:
#             cogn anchor: write7 (highest cognitive loading)
#             semo anchor: soc5   (highest socio-emotional loading)
#           EBM scores are used (not Bartlett) because indicators are binary;
#           measurement-error variance V_error_ebm is computed from the
#           EBM shrinkage matrix for use in SIMEX.
################################################################################

################################################################################
# SECTION 1 — LIBRARY AND PATH SETUP
################################################################################

InstallLocation <- "/file_path"
assign(x      = ".lib.loc",
       value  = InstallLocation,
       envir  = environment(.libPaths))
Sys.setenv("R_LIBS_USER" = InstallLocation)

rm(list = ls())

library(rlang)
library(dplyr)
library(mnormt)
library(lattice)
library(psych)
library(GPArotation)
library(haven)
library(corrplot)
library(readxl)
library(lavaan)
library(polycor)
library(ggplot2)
library(semTools)
library(sirt)
library(data.table)

# Redirect console output to log file
sink("file_path/ESEM_EYFSP.txt")


################################################################################
# SECTION 2 — DATA PREPARATION
################################################################################

# Load sample restriction flags from main analysis file
data    <- read_dta("file_path/clean_data.dta")
missing <- data[, c("id_dfe", "missing_vars")]

# Load EYFSP item-level data and merge with sample flags
data_eyfsp <- read_dta("file_path/eyfsp_clean.dta")
merged_df  <- merge(missing, data_eyfsp, by = "id_dfe")

# Apply sample restrictions:
#   missing_vars == 0 : complete data on all analysis variables
#   score_eyfsp_overall != 0 : drop children not evaluated at EYFSP
filtered_df <- subset(merged_df, missing_vars == 0 & score_eyfsp_overall != 0)

# Items retained from final Stream B EFA (32 items, two factors)
# Factor 1 (cogn): literacy and numeracy items
# Factor 2 (semo): PSE and language items
esem_items <- c(
  "write7", "read6",  "cal7",  "num5",  "link6", "num8",
  "write4", "cal6",   "read8", "cal4",  "read4", "link4",
  "read7",  "write6", "num4",  "read5", "write5",
  "soc5",   "emo5",   "soc4",  "emo4",  "soc6",  "emo7",
  "att5",   "lang6",  "att6",  "emo6",  "emo8",  "lang5",
  "att4",   "soc7",   "lang4"
)

data_cfa <- filtered_df[, c("id_dfe", esem_items)]
str(data_cfa)


################################################################################
# SECTION 3 — EFA TO OBTAIN ESEM STARTING VALUES
#
# Tetrachoric correlation matrix estimated from binary items; two-factor
# geominQ (oblique) solution provides starting values for the ESEM loadings.
# Eigenvalues stored for reference.
################################################################################

corr_efa   <- tetrachoric(data_cfa[, -1])$rho
esem_efa   <- fa(corr_efa, nfactors = 2, rotate = "geominQ", fm = "minres")
eigenvalues <- esem_efa$e.values

# Reshape loadings into long-format data.table for model syntax construction
esem_loadings <- data.table(
  matrix(round(esem_efa$loadings, 5), nrow = length(esem_items), ncol = 2)
)
names(esem_loadings) <- c("cogn_esem", "semo_esem")
esem_loadings[, item := esem_items]
esem_loadings <- melt(esem_loadings, id.vars = "item", variable.name = "latent")
esem_loadings


################################################################################
# SECTION 4 — ESEM MODEL SYNTAX
#
# Anchor variables fix the cross-loading of each factor for identification.
# For each factor, the anchor item's cross-loading on the other factor is
# fixed to its EFA value (not freely estimated); all other cross-loadings
# use their EFA values as starting values only (start(...)*item syntax).
#
# Anchors chosen as the item with the highest loading on each factor:
#   cogn: write7   semo: soc5
################################################################################

anchors <- c(cogn_esem = "write7", semo_esem = "soc5")

esem_model <- function(loadings_dt, anchors) {

  loadings_dt[, is_anchor := 0L]

  # Flag cross-loading anchor items (anchor item for the *other* factor)
  for (l in names(anchors)) {
    loadings_dt[latent != l & item == anchors[l], is_anchor := 1L]
  }

  # Free parameters use start() syntax; anchors are fixed to EFA value
  loadings_dt[is_anchor == 0L, syntax := paste0("start(", value, ")*", item)]
  loadings_dt[is_anchor == 1L, syntax := paste0(value, "*", item)]

  # Construct one model syntax line per latent factor
  each_syntax <- function(l) {
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = " + "), "\n")
  }

  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

# ------------------------------------------------------------------------------
# NOTE: esem_model() generates the syntax string below automatically.
# The structure of the resulting syntax is as follows (loadings illustrative):
#
# cogn_esem =~ start(l11)*write7 +                              # anchor: fixed in semo_esem
#              start(l12)*read6  + ... + start(l1k)*write5 +    # primary loadings
#              l13*soc5          +                              # anchor: fixed in cogn_esem
#              start(l14)*emo5   + ... + start(l1k)*lang4       # cross-loadings
#
# semo_esem =~ l21*write7        +                              # anchor: fixed in semo_esem
#              start(l22)*read6  + ... + start(l2k)*write5 +    # cross-loadings
#              start(l23)*soc5   +                              # anchor: fixed in cogn_esem
#              start(l24)*emo5   + ... + start(l2k)*lang4       # primary loadings
#
# Where start(lij)*item = freely estimated, EFA value as starting point
#       lij*item        = anchor cross-loading, fixed to EFA value to preserve 
#                         rotation structure
# ------------------------------------------------------------------------------
################################################################################
# SECTION 5 — ESEM / CFA ESTIMATION
#
# Fitted as a CFA with WLSMV estimator (appropriate for ordered/binary items).
# std.lv = TRUE fixes factor variances to 1 for identification.
################################################################################

esem_fit <- cfa(
  model   = esem_model(esem_loadings, anchors),
  data    = data_cfa[, -1],
  ordered = esem_items,
  estimator = "WLSMV",
  std.lv    = TRUE,
  verbose   = TRUE
)

summary(esem_fit, fit.measures = TRUE, standardized = TRUE)
modindices(esem_fit, sort. = TRUE, minimum.value = 10)


################################################################################
# SECTION 6 — MEASUREMENT ERROR VARIANCE (FOR SIMEX)
#
# Under the EBM scoring rule, the measurement error variance for each factor
# is derived from the factor loading matrix (Lambda), residual variance matrix
# (Theta), and factor covariance matrix (Psi).
#
# V_error_ebm = [Psi^{-1} + Lambda' Theta^{-1} Lambda]^{-1}
#
# This is an approximation: indicators are binary so the linear factor model
# underlying this formula holds only asymptotically.  EBM scores (not Bartlett)
# are used because Bartlett scores are inconsistent with binary indicators.
################################################################################

Lambda <- lavInspect(esem_fit, "est")$lambda
Theta  <- lavInspect(esem_fit, "est")$theta
Psi    <- lavInspect(esem_fit, "est")$psi

# Measurement error variance matrix
V_error_ebm <- solve(solve(Psi) + t(Lambda) %*% solve(Theta) %*% Lambda)
me_sd_ebm   <- sqrt(diag(V_error_ebm))
me_sd_ebm

# EBM shrinkage matrix and implied factor score covariance
# W_ebm = Psi Lambda' [Lambda Psi Lambda' + Theta]^{-1} Lambda
W_ebm       <- Psi %*% t(Lambda) %*% solve(Lambda %*% Psi %*% t(Lambda) + Theta) %*% Lambda
Cov_eta_ebm <- W_ebm %*% Psi
Cov_eta_ebm


################################################################################
# SECTION 7 — FACTOR SCORES AND RELIABILITY
################################################################################

scores     <- lavPredict(esem_fit, method = "EBM", se = "standard")
data_scores <- cbind(data_cfa[, "id_dfe", drop = FALSE], as.data.frame(scores))
data_scores <- data_scores[, c("id_dfe", "cogn_esem", "semo_esem")]

# Reliability diagnostics (exploratory; not reported in paper)
omega_values <- compRelSEM(esem_fit)
rel          <- reliability(esem_fit, return.total = FALSE)


################################################################################
# SECTION 8 — SAVE AND CLOSE
################################################################################

write_dta(data_scores, "file_path/fs_esem.dta")

sink()  # Close log file

################################################################################
# END OF FILE
################################################################################
