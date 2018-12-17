################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 7 -- APIM
# TEST APIM with MLM
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
table7.1_df <- read.spss("http://davidakenny.net/kkc/c7/roommate.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)
# The data is already organized as pairwise
head(table7.1_df) 

if (!require("nlme")) install.packages("nlme"); 
suppressMessages(library(nlme))

# Demonstrate mlm without preparing an effect code
mlm         <- gls(SATISFACTION   ~ ACT_HOUSE + PART_HOUSE, 
                   correlation = corCompSymm(form = ~1|Dyad),
                   data = table7.1_df)
summary(mlm)
getVarCov(mlm)
intervals(mlm)
confint (mlm, method= 'profile')

if (!require("lme4")) install.packages("lme4"); 
suppressPackageStartupMessages(library(lme4))

# Run random intercept model
lmerModel <- lmer(SATISFACTION  ~ ACT_HOUSE + PART_HOUSE + 
                (1 | Dyad), data = table7.1_df)
summary(lmerModel)

