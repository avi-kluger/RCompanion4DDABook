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