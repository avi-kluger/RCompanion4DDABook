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
figure7.3_df <- read.spss("http://davidakenny.net/kkc/c7/campbell.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)
# The data is already organized as pairwise
head(figure7.3_df) 


if (!require("lavaan")) install.packages("lavaan"); 
suppressPackageStartupMessages(library(lavaan))

APIM_distinguisable <- '

      mdistr  ~ a1*mneuro  # Male actor effect
      fdistr  ~ a2*fneuro  # Female actor effect
      mdistr  ~ p12*fneuro  # Female to male partner effect
      fdistr  ~ p21*mneuro  # Male to female partner effect
      mneuro ~ mx1*1 # Mean for X for men
      fneuro ~ mx2*1 # Mean for X for women
      mdistr ~ iy1*1  # Intercept for Y for women
      fdistr ~ iy2*1 # Intercept for Y for women
      mneuro ~~ vx1*mneuro  # Variance for X for men
      fneuro ~~ vx2*fneuro  # Variance for X for women
      mdistr ~~ ve1*mdistr   # Error variance for Y for men
      fdistr ~~ ve2*fdistr    # Error variance for Y for women
      fneuro ~~ cx*mneuro # Covariance of X between men and women
      fdistr ~~ cy*mdistr  # Covariance of errors between men and women

'

# Estimate the model 
apimDistinguisable <- sem(APIM_distinguisable,
                          data = figure7.3_df)
                         
# Examine the model.
summary(apimDistinguisable, 
        fit.measures = TRUE)

if (!require("semPlot")) install.packages("semPlot"); 
suppressPackageStartupMessages(library(semPlot))


semPaths(apimDistinguisable, 
         "est",
         sizeMan = 15,
         residuals = TRUE,
         intercepts = FALSE, 
         rotation = 2,
         style = "lisrel",
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         sizeInt = 25,
         edge.label.cex = 1.5,
         label.prop = .8,
         edge.label.position = c(0.5,0.5,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5),
         nodeLabels=c("Male\nDistress",   "Female\nDistress",
                      "Male\nNeuroticism","Female\nNeuroticism")
         )


# Examine the model with standardized option -- INCORRECT.
summary(apimDistinguisable, 
        fit.measures = TRUE,
        standardized=TRUE)

# Standardized the data across dyads
neuroMean <- mean(c(figure7.3_df$fneuro, figure7.3_df$mneuro))
neuroSD   <- sd  (c(figure7.3_df$fneuro, figure7.3_df$mneuro))
distrMean <- mean(c(figure7.3_df$fdistr, figure7.3_df$mdistr))
distrSD   <- sd  (c(figure7.3_df$fdistr, figure7.3_df$mdistr))

figure7.3_df$fneuro <- (figure7.3_df$fneuro - neuroMean)/neuroSD 
figure7.3_df$mneuro <- (figure7.3_df$mneuro - neuroMean)/neuroSD 
figure7.3_df$fdistr <- (figure7.3_df$fdistr - distrMean)/distrSD 
figure7.3_df$mdistr <- (figure7.3_df$mdistr - distrMean)/distrSD


# Estimate the proper standardized model 
apimDistinguisableStandardized <- sem(APIM_distinguisable,
                                     data = figure7.3_df)
                         
# Examine that standadized results with correct and incorrect approaches.

parameterEstimates(apimDistinguisableStandardized)
standardizedSolution(apimDistinguisable)