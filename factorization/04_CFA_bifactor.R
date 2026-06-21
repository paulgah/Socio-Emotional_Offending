################################################################################
# Project : Early Socio-Emotional Skills and Adolescent Offending:
#           Evidence from Administrative Data
# Author  : Paul Garcia
# Institute: Institute for Social and Economic Research (ISER), University of Essex
# Fellowship: ADR UK (ESRC) Research Fellowship
#
# File    : 04_CFA_bifactor.R
# Purpose : Estimate bifactor and unidimensional confirmatory factor models
#           on EYFSP point-scale items using WLSMV; compute reliability and
#           model fit indices; extract and export factor scores.
#
# Inputs  : clean_data.dta    (missing_vars)
#           eyfsp_clean.dta   (item-level point scales)
#
# Outputs : fs_bifactor.dta   (gend_R, cogn_R, semo_R factor scores)
#           fs_unifactor.dta  (gend_R factor score)
#           01_CFA_Bartlett.txt (console output log)
#
# Notes   : All file paths use the placeholder "file_path" and should be
#           updated to absolute paths before running.
#           WLSMV estimator used throughout (appropriate for ordered
#           categorical indicators).
################################################################################


################################################################################
# SECTION 1 — SETUP
################################################################################

## Library location
lib_base <- "file_path/R_libs"
assign(x = ".lib.loc", value = lib_base, envir = environment(.libPaths))
Sys.setenv("R_LIBS_USER" = lib_base)

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

## Redirect console output to log file
sink("file_path/output/04_CFA_bifactor.txt")

################################################################################
# SECTION 2 — LOAD AND MERGE DATA
################################################################################

data       <- read_dta("file_path/clean_data.dta")
data_eyfsp <- read_dta("file_path/eyfsp_clean.dta")

## Merge on id_dfe, restrict to analysis sample
merge_df    <- merge(data[, c("id_dfe", "missing_vars")], data_eyfsp, by = "id_dfe")
filtered_df <- subset(merge_df, missing_vars == 0 & score_eyfsp_overall != 0)

## Items retained after high-correlation screening (see 01_eyfsp_inspect.do)
items <- c(
  # Cognitive items (CLL + PSRN)
  "write7", "read6",  "cal7",  "num5",  "link6", "num8",
  "write4", "cal6",   "read8", "cal4",  "read4", "link4",
  "read7",  "write6", "num4",  "read5", "write5",
  # Socio-emotional items (PSED + language)
  "soc5",  "emo5",  "soc4",  "emo4",  "soc6",  "emo7",
  "att5",  "lang6", "att6",  "emo6",  "emo8",  "lang5",
  "att4",  "soc7",  "lang4"
)

data_cfa <- filtered_df[, c("id_dfe", items)]
str(data_cfa)


################################################################################
# SECTION 3 — BIFACTOR MODEL
#
# Three orthogonal factors:
#   gend_R  — general factor (all items)
#   cogn_R  — cognitive specific factor (CLL + PSRN items)
#   semo_R  — socio-emotional specific factor (PSED + language items)
#
# All factor variances fixed to 1 (for identification with std.lv = TRUE).
# Factors constrained orthogonal.
################################################################################

model_bifactor <- '
  gend_R =~ write7 + read6  + cal7   + num5   + link6  + num8   +
             write4 + cal6   + read8  + cal4   + read4  + link4  +
             read7  + write6 + num4   + read5  + write5 +
             soc5   + emo5   + soc4   + emo4   + soc6   + emo7   +
             att5   + lang6  + att6   + emo6   + emo8   + lang5  +
             att4   + soc7   + lang4

  cogn_R =~ write7 + read6  + cal7   + num5   + link6  + num8   +
             write4 + cal6   + read8  + cal4   + read4  + link4  +
             read7  + write6 + num4   + read5  + write5

  semo_R =~ soc5   + emo5   + soc4   + emo4   + soc6   + emo7   +
             att5   + lang6  + att6   + emo6   + emo8   + lang5  +
             att4   + soc7   + lang4

  # Fix factor variances to 1
  gend_R ~~ 1 * gend_R
  cogn_R ~~ 1 * cogn_R
  semo_R ~~ 1 * semo_R

  # Factors orthogonal
  gend_R ~~ 0 * cogn_R
  gend_R ~~ 0 * semo_R
  cogn_R ~~ 0 * semo_R
'

fit_bifactor <- cfa(
  model_bifactor,
  data      = data_cfa[, -1],
  ordered   = items,
  estimator = "WLSMV",
  std.lv    = TRUE,
  verbose   = TRUE
)

summary(fit_bifactor, fit.measures = TRUE)

#------------------------------------------------------------------------------
# 3.1  Reliability indices
#------------------------------------------------------------------------------
omega <- reliability(fit_bifactor)
print(omega)

## Omega-h (hierarchical omega) per factor
L   <- inspect(fit_bifactor, "std")$lambda
delta <- 1 - rowSums(L^2)
den   <- (sum(L[, "gend_R"]))^2 + sum(delta)

omega_h_gend <- sum(L[, "gend_R"])^2 / den
omega_h_cogn <- sum(L[, "cogn_R"])^2 / den
omega_h_semo <- sum(L[, "semo_R"])^2 / den
cat("Omega-h general:        ", omega_h_gend, "\n")
cat("Omega-h cognitive:      ", omega_h_cogn, "\n")
cat("Omega-h socio-emotional:", omega_h_semo, "\n")

## Explained Common Variance (ECV)
ECV <- sum(L[, "gend_R"]^2) / sum(L^2)
cat("ECV:", ECV, "\n")

## Percent Uncontaminated Correlations (PUC)
p            <- nrow(L)
total_pairs  <- choose(p, 2)
n1           <- sum(L[, "cogn_R"] != 0)
n2           <- sum(L[, "semo_R"] != 0)
contam_pairs <- choose(n1, 2) + choose(n2, 2)
puc          <- 1 - (contam_pairs / total_pairs)
cat("PUC:", puc, "\n")

#------------------------------------------------------------------------------
# 3.2  Factor scores and export
#------------------------------------------------------------------------------
scores_bifactor <- lavPredict(fit_bifactor, method = "EBM")

data_scores_bifactor <- cbind(
  data_cfa[, "id_dfe", drop = FALSE],
  as.data.frame(scores_bifactor)[, c("gend_R", "cogn_R", "semo_R")]
)

write_dta(data_scores_bifactor, "file_path/fs_bifactor.dta")


################################################################################
# SECTION 4 — UNIDIMENSIONAL MODEL
################################################################################

model_unidim <- '
  gend_R =~ write7 + read6  + cal7   + num5   + link6  + num8   +
             write4 + cal6   + read8  + cal4   + read4  + link4  +
             read7  + write6 + num4   + read5  + write5 +
             soc5   + emo5   + soc4   + emo4   + soc6   + emo7   +
             att5   + lang6  + att6   + emo6   + emo8   + lang5  +
             att4   + soc7   + lang4

  gend_R ~~ 1 * gend_R
'

fit_unidim <- cfa(
  model_unidim,
  data      = data_cfa[, -1],
  ordered   = items,
  estimator = "WLSMV",
  std.lv    = TRUE,
  verbose   = TRUE
)

summary(fit_unidim, fit.measures = TRUE)

#------------------------------------------------------------------------------
# 4.1  Factor scores and export
#------------------------------------------------------------------------------
scores_unidim <- lavPredict(fit_unidim, method = "EBM")

data_scores_unidim <- cbind(
  data_cfa[, "id_dfe", drop = FALSE],
  as.data.frame(scores_unidim)[, "gend_R", drop = FALSE]
)

write_dta(data_scores_unidim, "file_path/fs_unifactor.dta")


################################################################################
# CLOSE LOG
################################################################################

sink()

################################################################################
# END OF FILE
################################################################################
