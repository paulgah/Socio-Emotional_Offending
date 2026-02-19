################################################################################
# Project: Socio-emotional characteristics in early childhood and offending 
#          behaviour in adolescence
# Author:  Paul Garcia
# Institution: Institute for Social and Economic Research (ISER), University of Essex
# Fellowship: ADR UK (ESRC) Research Fellowship
# Description: Exploratory Structural Equation Modelling (ESEM) estimated manually
#              via EFA-derived loadings used as starting values in a CFA framework.
#              Uses EYFSP binary indicators to extract cognitive and socio-emotional
#              latent factors. Factor scores are saved for use in subsequent analysis.
################################################################################

## -----------------------------------------------------------------------------
## Set library location (ONS Secure Research Service environment)
## -----------------------------------------------------------------------------
InstallLocation <- "/file_path"
lib_base <- InstallLocation

## Assign custom library path as default for loading and installing packages
assign(x = ".lib.loc",
       value = lib_base,
       envir = environment(.libPaths))

## Enable parallel runs by setting R library path environment variable
Sys.setenv("R_LIBS_USER" = lib_base)

## Clear environment
rm(list = ls())

## -----------------------------------------------------------------------------
## Load required libraries
## -----------------------------------------------------------------------------
library(rlang)
library(dplyr)
library(mnormt)
library(lattice)
library(psych)          # For tetrachoric correlations and EFA
library(GPArotation)    # For factor rotation (geominQ)
library(haven)          # For reading/writing Stata .dta files
library(corrplot)       # Correlation plots
library(readxl)
library(lavaan)         # CFA/SEM estimation
library(polycor)        # Polychoric correlations
library(ggplot2)
library(semTools)       # Reliability and model comparison tools
library(sirt)           # Additional IRT/SEM tools
library(data.table)     # Efficient data manipulation

################################################################################
## ESEM: MANUAL APPROACH
## Step 1: EFA to obtain factor loadings
## Step 2: Use loadings as starting values in a CFA (ESEM-within-CFA approach)
################################################################################

## Redirect output to text file for logging
sink("file_path/ESEM_EYFSP.txt")

## -----------------------------------------------------------------------------
## Load and merge data
## -----------------------------------------------------------------------------

## Main dataset: contains sample selection indicator (missing_vars)
data <- read_dta("file_path/clean_data.dta")
missing <- data[, c("id_dfe", "missing_vars")]

## EYFSP dataset: contains Early Years Foundation Stage Profile indicators
data_eyfsp <- read_dta("file_path/eyfsp_clean.dta")

## Merge on pupil identifier
merge_df <- merge(missing, data_eyfsp, by = "id_dfe")

## -----------------------------------------------------------------------------
## Sample restrictions
## -----------------------------------------------------------------------------

## Keep only observations with complete data and non-zero EYFSP overall score
filtered_df <- subset(merge_df, missing_vars == 0)
filtered_df <- subset(filtered_df, score_eyfsp_overall != 0)

## -----------------------------------------------------------------------------
## Select EYFSP binary indicators for ESEM
## Cognitive items: writing, reading, calculating, number, linking
## Socio-emotional items: social, emotional, attention, language
## -----------------------------------------------------------------------------
data_cfa <- filtered_df[, c("id_dfe",
                            # Cognitive indicators
                            "write7", "read6", "cal7", "num5", "link6", "num8",
                            "write4", "cal6", "read8", "cal4", "read4", "link4",
                            "read7", "write6", "num4", "read5", "write5",
                            # Socio-emotional indicators
                            "soc5", "emo5", "soc4", "emo4", "soc6", "emo7",
                            "att5", "lang6", "att6", "emo6", "emo8", "lang5",
                            "att4", "soc7", "lang4")]
str(data_cfa)

## -----------------------------------------------------------------------------
## Step 1: EFA using tetrachoric correlations
## Tetrachoric correlations are appropriate for binary indicators
## geominQ rotation allows cross-loadings (key feature of ESEM)
## -----------------------------------------------------------------------------
corr_efa <- tetrachoric(data_cfa[, -1])$rho

## EFA with 2 factors: cognitive and socio-emotional
esem_efa <- fa(corr_efa, nfactors = 2, rotate = "geominQ", fm = "minres")
eigenvalues <- esem_efa$e.values

## Extract and format factor loadings (32 items x 2 factors)
esem_loadings <- data.table(matrix(round(esem_efa$loadings, 5), nrow = 32, ncol = 2))
names(esem_loadings) <- c("cogn_esem", "semo_esem")
esem_loadings$item <- colnames(data_cfa[, -1])

## Reshape to long format for use in model syntax function
esem_loadings <- melt(esem_loadings, id.vars = "item", variable.name = "latent")
esem_loadings

## -----------------------------------------------------------------------------
## Step 2: Define anchor items for each factor
## Anchors fix the cross-loading to its EFA value (not freed) to ensure
## factor identification and interpretability
## -----------------------------------------------------------------------------
anchors <- c(cogn_esem = "write7", semo_esem = "soc5")

## -----------------------------------------------------------------------------
## Function to generate lavaan ESEM model syntax
## EFA loadings are used as starting values; anchor cross-loadings are fixed
## -----------------------------------------------------------------------------
esem_model <- function(loadings_dt, anchors) {
  loadings_dt[, is_anchor := 0]
  
  ## Flag cross-loadings of anchor items (to be fixed rather than freed)
  for(l in names(anchors)) {
    loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  }
  
  ## Non-anchor items: use EFA loading as starting value (estimated freely)
  loadings_dt[is_anchor == 0, syntax := paste0("start(", value, ")*", item)]
  
  ## Anchor cross-loadings: fix to EFA loading value
  loadings_dt[is_anchor == 1, syntax := paste0(value, "*", item)]
  
  ## Build lavaan syntax for each latent factor
  each_syntax <- function(l) {
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"), "\n")
  }
  
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

## -----------------------------------------------------------------------------
## Step 3: Estimate ESEM-within-CFA using WLSMV estimator
## WLSMV is appropriate for binary/ordinal indicators
## std.lv = TRUE: fixes factor variances to 1 for identification
## -----------------------------------------------------------------------------
esem_fit <- cfa(esem_model,
                data = data_cfa[, -1],
                ordered = c("write7", "read6", "cal7", "num5", "link6", "num8",
                            "write4", "cal6", "read8", "cal4", "read4", "link4",
                            "read7", "write6", "num4", "read5", "write5",
                            "soc5", "emo5", "soc4", "emo4", "soc6", "emo7",
                            "att5", "lang6", "att6", "emo6", "emo8", "lang5",
                            "att4", "soc7", "lang4"),
                estimator = "WLSMV",
                std.lv = TRUE,
                verbose = TRUE)

## -----------------------------------------------------------------------------
## Measurement error variance for each factor (used in SIMEX correction)
## Based on Empirical Bayes Modal (EBM) scoring
## Note: this is an approximation since indicators are binary
## -----------------------------------------------------------------------------

## Extract model matrices
Lambda <- lavInspect(esem_fit, "est")$lambda   # Factor loadings matrix
Theta  <- lavInspect(esem_fit, "est")$theta    # Residual variance matrix
Psi    <- lavInspect(esem_fit, "est")$psi      # Factor covariance matrix

## Measurement error variance: V(error) = (Psi^{-1} + Lambda' Theta^{-1} Lambda)^{-1}
V_error_ebm <- solve(solve(Psi) + t(Lambda) %*% solve(Theta) %*% Lambda)
me_sd_ebm   <- sqrt(diag(V_error_ebm))
me_sd_ebm

## -----------------------------------------------------------------------------
## Shrinkage matrix (W) and posterior covariance of factor scores
## Used to characterise attenuation from measurement error in SIMEX
## -----------------------------------------------------------------------------
W_ebm       <- Psi %*% t(Lambda) %*% solve(Lambda %*% Psi %*% t(Lambda) + Theta)
Cov_eta_ebm <- W_ebm %*% Psi
Cov_eta_ebm

## -----------------------------------------------------------------------------
## Extract EBM factor scores and model fit summary
## -----------------------------------------------------------------------------
scores <- lavPredict(esem_fit, method = "EBM", se = "standard")
summary(esem_fit, fit.measures = TRUE, standardized = TRUE)

## Merge factor scores back to pupil identifier
data_scores <- cbind(data_cfa, scores)
data_scores <- data_scores[, c("id_dfe", "cogn_esem", "semo_esem")]

## Modification indices (for model diagnostics — not used to re-specify)
modindices(esem_fit, sort. = TRUE, minimum.value = 10)

## -----------------------------------------------------------------------------
## Save factor scores to Stata format for use in subsequent econometric analysis
## -----------------------------------------------------------------------------
write_dta(data_scores, "file_path/fs_esem.dta")

## -----------------------------------------------------------------------------
## Reliability estimates
## -----------------------------------------------------------------------------
omega_values <- compRelSEM(esem_fit)
rel          <- reliability(esem_fit, return.total = FALSE)

## Close sink
sink()