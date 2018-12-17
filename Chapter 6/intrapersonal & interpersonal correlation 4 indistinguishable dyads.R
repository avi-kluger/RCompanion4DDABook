################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 6 -- Pairwise 
# TEST intrapersonal and interpersonal correlation for indistinguishable dyads
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
table6.1_df <- read.spss("http://davidakenny.net/kkc/c6/chapter6.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)
colnames(table6.1_df) <- c(paste(
                           rep(c("closeness", "commitment", "satisfaction"), 2),
                           rep(c("W", "H"), each = 3), sep = "."), 
                           "lengthOfMarriage")

# Reshape Dyad df into Pairwise df 

# Load the *tidyverse* packages
if (!require('tidyverse')) install.packages('tidyverse')
suppressPackageStartupMessages(library('tidyverse'))

wife_df           <- table6.1_df[, grep(".W", colnames(table6.1_df))]
husband_df        <- table6.1_df[, grep(".H", colnames(table6.1_df))]
colnames(wife_df) <- colnames(husband_df) <- c("closeness", "commitment", 
                                               "satisfaction")
wh_df             <- bind_rows(wife_df, husband_df)
hw_df             <- bind_rows(husband_df, wife_df)
pairwise_df       <- bind_cols(wh_df, hw_df)
head(pairwise_df)

if (!require('apaTables')) install.packages('apaTables')
suppressPackageStartupMessages(library('apaTables'))

(corMatrix <- cor(pairwise_df[, 
                             c("commitment", "satisfaction", "satisfaction1")]))

# Instead of the Z values reported on top of p.138, calculate CI. Note that
# sample size needs to be halved because the pairwise doubles the sample size.
if (!require('psychometric')) install.packages("psychometric") 
library(psychometric) 

CIr(r = corMatrix [1, 2], n = nrow(pairwise_df)/ 2, level = .95)
CIr(r = corMatrix [1, 3], n = nrow(pairwise_df)/ 2, level = .95)
