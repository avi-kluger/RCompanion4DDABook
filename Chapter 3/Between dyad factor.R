################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il; April 2021
#
#                              CHAPTER 3 -- Table 3.1
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Klump et al sample data (in SPSS format) from Kenny's book site
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
Klump_et_al <- haven::read_sav(
               "http://davidakenny.net/kkc/c3/klumpindividual.sav")


# Replicate the means and sd per cohort reported on p. 54
if (!require("sjPlot")) install.packages("sjPlot"); library(sjPlot)
Klump_et_al %>% 
  group_by(cohort) %>% 
  summarize(m = mean(total), sd = sd(total)) %>% 
  tab_df

# The book report analyses with ANOVA. Here I demonstrate obtaining the same
# results (partial ICC, dD and d) with MLM, 
# which, unlike ANOVA, can be used with missing data.

# Between dyad factor (cohort) model
# Make cohort a dummy code
Klump_et_al$cohort <- (Klump_et_al$cohort - 11) / 6

# Obtain both partial ICC and a test for the between dyad predictor with lme4
if (!require("lme4")) install.packages("lme4"); library(lme4)
mlm <- lmer(total ~ cohort + (1 | dyad), data = Klump_et_al)
tab_model(mlm)

# Obtain the above + CI for partial ICC
if (!require("nlme")) install.packages("nlme"); library(nlme)

mlm     <- gls(total ~ cohort, correlation=corCompSymm(form = ~1 | dyad),
               data = Klump_et_al)
fit     <- summary(mlm)
fit
icc     <- as.data.frame(intervals(mlm) ["corStruct"])
round(icc, 3)

# Calculate d with formula from p. 57. The formula in the book is based on F;
# The output here reports t, which with 1 df = sqrt(F)
t      <- as.numeric(fit$tTable["cohort", "t-value"])
ri     <- as.numeric(icc["corStruct.est."])
nDyads <- nrow(Klump_et_al) / 2
dD     <- 2*t / sqrt(nDyads)
d      <- dD * sqrt((1+ ri)/ 2)
round(dD, 3)
round(d, 3)