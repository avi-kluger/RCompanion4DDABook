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
Chapter10_df <- read.csv("https://www.dropbox.com/s/anigavgnv703lep/Chapter10_df?dl=1")

# Very Important Note.  The original data coded with 0 the focal person.  
# Therefore the first random variable above is partner variance.  Reversing
# the codes below make the results more intuitive.  I thank David Kenny for
# Clarifying this issue.

Chapter10_df$focalcode <- 1- Chapter10_df$focalcode
Chapter10_df$partcode  <- 1- Chapter10_df$partcode
head(Chapter10_df, 20) 

if (!require("nlme")) install.packages("nlme"); suppressMessages(library(nlme))

# Date: Thu, 24 Jan 2019 10:30:19 -0500
# From: Ben Bolker <bbolker@gmail.com>
# To: r-sig-mixed-models@r-project.org
# Subject: Re: [R-sig-ME] Correlations among random variables
# Message-ID: <5b402416-cd19-6f03-bddf-038441f8fc16@gmail.com>
# Content-Type: text/plain; charset="utf-8"
# 
# 
#    Not in lme4:
# http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#setting-residual-variances-to-a-fixed-value-zero-or-other
# 
#   In lme you can set the residual std dev to a fixed small value, but not to exactly zero:
# 
# lme(Reaction~Days,random=~1|Subject,sleepstudy,control=list(sigma=1e-8))
# 
#   If you're trying to test that the covariance is significantly positive, I think getting the standard error is the wrong approach; the Wald (quadratic) approximation is often very bad for random-effects variances and covariances.  I would suggest profile confidence intervals or a likelihood ratio test.
# 



mlm <- lme(outcome ~   0 + focalcode + partcode, 
            random = ~ 0 + focalcode + partcode|focalid/dyadid,
            control=list(sigma= .01),
            data = Chapter10_df)
intervals(mlm)
summary(mlm)
VarCorr(mlm)
mlmOutput <- VarCorr(mlm)

cat(
"Actor   variance = ",   round(as.numeric(VarCorr(mlm)[, "Variance"][3]), 3),
"\nPartner variance = ", round(as.numeric(VarCorr(mlm)[, "Variance"][2]), 3),
"\nGeneralized Reciprocity = ", round(as.numeric(VarCorr(mlm)[, "Corr"][3]), 3),
"\nDyadic Reciprocity = ", round(as.numeric(VarCorr(mlm)[, "Corr"][6]), 3), "\n"
)


df <- read.csv("https://raw.githubusercontent.com/avi-kluger/RCompanion4DDABook/master/Chapter%2010/Chapter10_df.csv")

library(nlme) 

df$focalcode <- 1 - df$focalcode
df$partcode  <- 1 - df$partcode 

### overparamterized model
res1 <- lme(outcome ~ 0 + focalcode + partcode, random = ~ 0 + focalcode + partcode | focalid/dyadid, data = df)
summary(res1)

### contrain sigma to a very small value
res2 <- lme(outcome ~ 0 + focalcode + partcode, 
            random = ~ 0 + focalcode + partcode | focalid/dyadid, 
            data = df, 
            control=list(sigma=1e-8, opt="optim"))
summary(res2)

# Just for fun, I also fitted the same model using '
# metafor'. While it was not really made for analyzing raw 
# data like this, it can be used to fit the same model 
# (with the devel version) and then sigma can be constrained exactly to 0:

devtools::install_github("wviechtb/metafor")
library(metafor)

df$dyadid.in.focalid <- interaction(df$focalid, df$dyadid)
res3 <- rma.mv(outcome ~ 0 + focalcode + partcode, 
               V = 0, 
               random = list(~ 0 + focalcode + partcode | focalid, 
                             ~ 0 + focalcode + partcode | dyadid.in.focalid), 
               struct = "GEN", data = df, sparse = TRUE)
res3

# ci.rho <- confint(res3, rho = 1, verbose = TRUE)
# ci.rho
# ci.phi <- confint(res3, phi = 1, verbose = TRUE)
# ci.phi

# This full run takes about 3 hours!!! if you want specific parameters use
# arguments as above or tau, tau, or gamma 
confint(res3)
# (note that 'focalid/dyadid' doesn't work at the moment, 
# so you have to create the nested factor manually first; 
# also, model fitting can be slow with rma.mv(), 
# so you might have to wait a bit for it to converge)

# The results for res2 and res3 are quite close.

