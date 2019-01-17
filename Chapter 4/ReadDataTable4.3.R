################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 4 -- Table 4.3
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Klump et al sample data (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
couples_df <- read.spss("http://davidakenny.net/kkc/c4/table4.3.sav", 
                         to.data.frame=TRUE)

if (!require("nlme")) install.packages("nlme"); library(nlme)

mlm_null     <- gls(future ~ 1, correlation=corCompSymm(form = ~1|dyad),
               data = couples_df)
(fit <- summary(mlm_null))
icc     <- as.data.frame(intervals(mlm_null) ["corStruct"])
round(icc, 3)

mlm_full     <- gls(future ~ contrib*culture, 
                    correlation=corCompSymm(form = ~1|dyad),
                    data = couples_df)
(fit <- summary(mlm_full))

getVarCov(mlm_full)
intervals(mlm_full)
icc     <- as.data.frame(intervals(mlm_full) ["corStruct"])
round(icc, 3)


if (!require("lme4")) install.packages("lme4"); library(lme4)

mlm_full <- lmer(future ~ contrib*culture + (1 | dyad), data = couples_df)
summary(mlm_full)

