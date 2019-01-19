################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 10 -- One with many with roles
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read (in SPSS format) from Kenny's book site and replicate Table 9.1
if (!require('foreign')) install.packages('foreign'); library('foreign')
table9.1_df <- read.spss("http://davidakenny.net/kkc/c9/4person.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)

head(table9.1_df) 
round(cor(table9.1_df), 2)

if (!require("lavaan")) install.packages("lavaan"); 
suppressPackageStartupMessages(library(lavaan))


OneWithManyDistinguishable <- '
# Actor effects:
        Actor   =~ 1*mfanx + 1*mcanx + 1*myanx
        
# Partner effects:
        Partner =~ 1*fmanx + 1*cmanx + 1*ymanx
        
# Generalized reciprocity:
        Actor   ~~ Partner
    
# Dyadic reciprocity:
        mfanx  ~~  fmanx
        mcanx  ~~  cmanx
        myanx  ~~  ymanx
'
# Estimate the model 
fitOneWithManyDistinguishable <- sem(OneWithManyDistinguishable, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")
# Examine the model.
summary(fitOneWithManyDistinguishable, 
        fit.measures = TRUE, standardized = TRUE)


OneWithManyIndistinguisableReplicateSPSS <- '
# Actor effects:
    mActor   =~ 1*mfanx + 1*mcanx + 1*myanx
    
# Partner effects:
    mPartner =~ 1*fmanx + 1*cmanx + 1*ymanx
    
# Fix variances to equality with focal person (a) and partner (p)
    mcanx ~~ a* mcanx
    mfanx ~~ a* mfanx
    myanx ~~ a* myanx

    cmanx ~~ p* cmanx
    fmanx ~~ p* fmanx
    ymanx ~~ p* ymanx
    
# Generalized reciprocity:
    mActor   ~~ gr*mPartner
    
# Dyadic reciprocity:
    mfanx  ~~  dr*fmanx 
    mcanx  ~~  dr*cmanx
    myanx  ~~  dr*ymanx
    
# Variance labels
    
    mActor   ~~ Actor  *mActor 
    mPartner ~~ Partner*mPartner 
    
# Fix intercepts to equality within focal person and partner
    mcanx ~ ia * 1
    mfanx ~ ia * 1
    myanx ~ ia * 1

    cmanx ~ ip * 1
    fmanx ~ ip * 1
    ymanx ~ ip * 1
'

# Estimate the model 
fitOneWithManyIndistinguisableReplicateSPSS <- 
                 sem(OneWithManyIndistinguisableReplicateSPSS, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS",
                 estimator ="MLM")
# Examine the model
summary(fitOneWithManyIndistinguisableReplicateSPSS, 
        fit.measures = TRUE, standardized=TRUE)