################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# lme code developed by Limor Borut: limor.borut@mail.huji.ac.il 
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 10 -- one with many SRM 
# 
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read (in SPSS format) from Kenny's book site and replicate Table 9.1
if (!require('foreign')) install.packages('foreign'); library('foreign')
Chapter10_df <- read.spss("http://davidakenny.net/kkc/c10/c10_recip.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)

head(Chapter10_df) 

if (!require("nlme")) install.packages("nlme"); suppressMessages(library(nlme))

mlm <- lme(outcome ~   0 + focalcode + 0 + partcode, 
            random = ~ 0 + focalcode + partcode|focalid/dyadid, 
            data = Chapter10_df)
summary(mlm)
intervals(mlm)
mlmOutput <- VarCorr(mlm)
VarCorr(mlm)

cat(
"Actor   variance = ",   round(as.numeric(VarCorr(mlm)[, "Variance"][3]), 3),
"\nPartner variance = ", round(as.numeric(VarCorr(mlm)[, "Variance"][2]), 3),
"\nGeneralized Reciprocity = ", round(as.numeric(VarCorr(mlm)[, "Corr"][3]), 3),
"\nDyadic Reciprocity = ", round(as.numeric(VarCorr(mlm)[, "Corr"][6]), 3. "\n")
)

# Very Important Note.  The original data coded with 0 the focal person.  
# Therefore the first random variable above is partner variance.  Reversing
# the codes below make the results more intuitive.  I thank David Kenny for
# Clarifying this issue.

Chapter10_df$focalcode <- 1- Chapter10_df$focalcode
Chapter10_df$partcode  <- 1- Chapter10_df$partcode
mlm <- lme(outcome ~   0 + focalcode + 0 + partcode, 
            random = ~ 0 + focalcode + partcode|focalid/dyadid, 
            data = Chapter10_df)
VarCorr(mlm)



