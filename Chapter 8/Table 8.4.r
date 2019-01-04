################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 8 -- SRM
# 
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

Table8.4 <-  "NA  8 5 10
               7 NA 7  6
               8  7 NA 5
               4  5  0 NA"

Table8.4_df <- read.table(textConnection(Table8.4))

# https://cran.r-project.org/web/packages/TripleR/TripleR.pdf
if (!require("TripleR")) install.packages("TripleR"); 
suppressPackageStartupMessages(library(TripleR))

Table8.4_long_df <- matrix2long(Table8.4_df)

fitTable8.4 <- RR(value ~ actor.id*partner.id, data = Table8.4_long_df)
fitTable8.4                        
                 
