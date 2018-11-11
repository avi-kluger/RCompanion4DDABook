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

