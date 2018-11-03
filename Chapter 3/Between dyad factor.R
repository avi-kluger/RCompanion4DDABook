################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.1
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Klump et al sample data (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
Klump_et_al <- read.spss("http://davidakenny.net/kkc/c3/klumpindividual.sav", 
                         to.data.frame=TRUE)

if (!require("nlme")) install.packages("nlme"); library(nlme)

# Null model

mlm     <- gls(total ~ 1, data = Klump_et_al,
           na.action = "na.omit", verbose = TRUE,
           correlation=corCompSymm(form = ~1|dyad))
icc     <- intervals(mlm) ["corStruct"]
round(as.data.frame(icc), 3)

# Between dyad factor (cohort) model
# Make cohort a dummy code
Klump_et_al$cohort <- (Klump_et_al$cohort - 11) / 6

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
group_by(Klump_et_al, cohort) %>% summarize(m = mean(total),
                                            sd = sd(total))

mlm     <- gls(total ~ cohort, data = Klump_et_al,
           na.action = "na.omit", verbose = TRUE,
           correlation=corCompSymm(form = ~1|dyad))
fit <- (summary(mlm))
icc     <- as.data.frame(intervals(mlm) ["corStruct"])
round(icc, 3)

# Calculate d with formulae from p. 57
t      <- as.numeric(fit$tTable["cohort", "t-value"])
ri     <- as.numeric(icc["corStruct.est."])
nDyads <- sqrt(nrow(Klump_et_al)/2)
dD     <- 2*t / nDyads
d      <- dD * sqrt((1+ ri)/ 2)
round(dD, 3)
round(d, 3)
