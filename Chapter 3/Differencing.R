################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.5
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')

table3.5_df <- read.spss("http://davidakenny.net/kkc/c3/table3.5.sav", 
               to.data.frame = TRUE)
t.test(table3.5_df$DIFF)
fit         <- lm(DIFF ~ GDIFF + 0, data = table3.5_df)
summary(fit)$coefficients

t           <- as.numeric(summary(fit)$coefficients[3])
rd          <- as.numeric(t/sqrt(t^2 + summary(fit)$df[2]))

# Compute ICC
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Transform dyad df into individual df, and keep only the dyad and score
individual_df <- table3.5_df %>% gather(dyadMember, score, H:W) 

if (!require("nlme")) install.packages("nlme"); library(nlme)

mlm         <- gls(score   ~ 1, correlation = corCompSymm(form = ~1|DYAD),
                data = individual_df)
icc         <- intervals(mlm) ["corStruct"]
icc         <- round(as.data.frame(icc), 2)
r           <- rd * sqrt(1 - icc[2])

cat(paste0("\nrD = ", rd, " and adjusted r = ", round(as.numeric(r), 2), "\n
    Note: The sign is revsersed of the results on the bottom of p. 73.
    To get the results of the book, the differencing should be reversed
    (e.g., table3.5_df$DIFF <- table3.5_df$W - table3.5_df$H)."))




