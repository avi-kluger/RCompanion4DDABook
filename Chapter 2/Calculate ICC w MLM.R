################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 2 -- Table 2.1
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

dyad_df <- read.csv(text = "dyad,x,xprime
                            1,8,6
                            2,5,3
                            3,7,2
                            4,8,5
                            5,8,7
                            6,5,6
                            7,3,4
                            8,8,9
                            9,6,7
                            10,2,3", header = TRUE)

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Transform dyad df into individual df, and keep only the dyad and score

Individual_df <- (dyad_df %>% gather(var, score, x:xprime))[, 
                                                            c("dyad", "score")]

# Calculate intra class correlation (ICC) with MLM
# Code for ICC is based on http://davidakenny.net/papers/k&h/MLM_R.pdf

if (!require("nlme")) install.packages("nlme"); library(nlme)
mlm     <- gls(score ~ 1, data = Individual_df,
           na.action = "na.omit", verbose = TRUE,
           correlation=corCompSymm(form = ~1|dyad))
icc     <- intervals(mlm) ["corStruct"]
round(as.data.frame(icc), 3)
