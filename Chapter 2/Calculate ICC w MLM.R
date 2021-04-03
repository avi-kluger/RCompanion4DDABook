################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il, April 2021
#
#                              CHAPTER 2 -- Table 2.1
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

w <- read.csv(text = "dyad, x, xprime
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

# Correlate x and x'; meaningless if dyads are indistinguishable
w %>% dplyr::select(-dyad) %>% cor %>% round(2) 

# Calculate ICC on wide data with ICC function from Psych 
library("psych")
w %>% dplyr::select(-dyad) %>% ICC

# A two-tailed CI, but see the Note in the help of the psych:ICC function 
w %>% dplyr::select(-dyad) %>% ICC(alpha = .025)

# Transform dyad data into individual data &  keep the dyad and score variables
l <- w %>%
  pivot_longer(                           
    cols = !dyad
)
l 

# Calculate intraclass correlation (ICC) with MLM
# Code for ICC is based on http://davidakenny.net/papers/k&h/MLM_R.pdf

if (!require("nlme")) install.packages("nlme"); library(nlme)
mlm     <- gls(value ~ 1, correlation = corCompSymm(form = ~1|dyad), data = l)
icc     <- intervals(mlm) ["corStruct"]
icc     <- round(as.data.frame(icc), 2)
cat(paste0("\nICC = ", icc[2], " [", icc[1], ", ", icc[3], "]"))
