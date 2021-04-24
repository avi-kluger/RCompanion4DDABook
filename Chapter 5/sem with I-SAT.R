################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il April 2021
#
#                              CHAPTER 5 -- Table 5.1
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

library(lavaan)
#  Copy the correlation matrix (lower diagonal form) from p. 115
table5.1.cor <- '
1.000
.380 1.000
.351 .483 1.000
.531 .386 .385 1.000
.411 .161 .142 .228 1.000
.313 .453 .266 .245 .348 1.000
.118 .080 .092 .099 .323 .381 1.000
.214 .148 .129 .357 .403 .431 .256 1.000'

# name the variables and convert to full correlation matrix
table5.1.cor <- getCov(table5.1.cor, names = c(
                "X11", "X21", "X31", "X41", "X12", "X22", "X32", "X42"))

# display the correlations matrix
table5.1.cor

# convert the correlation matrix into a covariance matrix using the SDs
table5.1.cov <- cor2cov(table5.1.cor, sds = c(
             0.579, 0.626, 0.607, 0.653, 0.662, 0.605, 0.615, 0.694))

# display the covariance matrix
table5.1.cov

# specify path model
oneFactorFourIndicators <- '

# factors
        # set X41 and X42 first so they become the marker variables
        F1 =~  X41 + e*X11 + f*X21 + g*X31 
        F2 =~  X42 + e*X12 + f*X22 + g*X32

# intercpets
        X11 ~ a*1
        X21 ~ b*1
        X31 ~ c*1
        X41 ~ d*1
        
        X12 ~ a*1
        X22 ~ b*1
        X32 ~ c*1
        X42 ~ d*1

# error covariances
        X11 ~~ n*X12
        X21 ~~ o*X22
        X31 ~~ p*X32
        X41 ~~ q*X42

# variances
        F1 ~~ h*F1
        F2 ~~ h*F2
        
        X11 ~~ j*X11
        X21 ~~ k*X21
        X31 ~~ l*X31
        X41 ~~ m*X41
        
        X12 ~~ j*X12
        X22 ~~ k*X22
        X32 ~~ l*X32
        X42 ~~ m*X42
'

# fit initial model to data
model <- sem(oneFactorFourIndicators,
             sample.cov  = table5.1.cov,
             sample.nobs = 137, 
             sample.mean = c(3.042, 2.571, 2.903, 3.095, 
                             3.074, 2.474, 2.913, 3.144))

summary(model, 
        fit.measures = TRUE, 
        standardized = TRUE)

I_SAT <- '
# means
     X11 ~ a*1
     X21 ~ b*1
     X31 ~ c*1
     X41 ~ d*1
     X12 ~ a*1
     X22 ~ b*1
     X32 ~ c*1
     X42 ~ d*1

# variances
     X11 ~~ jj*X11
     X21 ~~ kk*X21
     X31 ~~ ll*X31
     X41 ~~ mm*X41
     X12 ~~ jj*X12
     X22 ~~ kk*X22
     X32 ~~ ll*X32
     X42 ~~ mm*X42

# covariances 

# intrapersonal
     X11 ~~ e*X21
     X11 ~~ f*X31
     X11 ~~ g*X41
     X21 ~~ h*X31
     X21 ~~ i*X41
     X31 ~~ j*X41
     
     X12 ~~ e*X22
     X12 ~~ f*X32
     X12 ~~ g*X42
     X22 ~~ h*X32
     X22 ~~ i*X42
     X32 ~~ j*X42

# interpersonal
# The letters for the constraints below follow the matrix on p. 133 in
# Olsen, J. A., & Kenny, D. A. (2006). Structural equation modeling with
# interchangeable dyads. Psychological Methods, 11, 127-141.

     X11 ~~ l*X12
     X11 ~~ p*X22
     X11 ~~ q*X32
     X11 ~~ r*X42
     
     X21 ~~ p*X12
     X21 ~~ m*X22
     X21 ~~ s*X32
     X21 ~~ t*X42
     
     X31 ~~ q*X12
     X31 ~~ s*X22
     X31 ~~ n*X32
     X31 ~~ u*X42
     
     X41 ~~ r*X12
     X41 ~~ t*X22
     X41 ~~ u*X32
     X41 ~~ o*X42
'

baseModel <- sem(I_SAT,
             sample.cov  = table5.1.cov,
             sample.nobs = 137, 
             sample.mean = c(3.042, 2.571, 2.903, 3.095, 
                             3.074, 2.474, 2.913, 3.144))
summary(baseModel, 
        fit.measures = TRUE, 
        standardized = TRUE)

anova(baseModel, model)

# Compute CFI
chiNull    <- fitMeasures(model, "baseline.chisq")
dfNull     <- fitMeasures(model, c("df")) # does not work
dfNull     <- 28 # from lavaan output (36 in the book)

fit        <- as.data.frame(anova(baseModel, model))
deltaNull  <- chiNull - dfNull
chiModel   <- fit['model', "Chisq diff"]
dfModel    <- fit['model', "Df diff"]
deltaModel <- chiModel - dfModel

CFI <- as.numeric((deltaNull - deltaModel)/deltaNull)
CFI

# Compute RMSEA
N     <- as.numeric(fitMeasures(model, c("ntotal")))
RMSEA <- sqrt(((fit['model', "Chisq diff"] / 
                fit['model', "Df diff"]) - 1) / (N- 1))
RMSEA
