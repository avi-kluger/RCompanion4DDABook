################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.3
# Data for Table 3.3 is not provided in the book's site.  Alternative dataset
# is provided here.  These data (Lehmann, 2018) are ratings of humility by 
# listeners and speakers (within dyad variable) in a control group of a larger
# experiment.
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Example data DIFFERENT FROM THE BOOK based on Lehmann study of humility

humility_df <- read.csv("https://raw.githubusercontent.com/avi-kluger/RCompanion4DDABook/master/Chapter%203/Lehmann%20Humility%20data.csv")

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')