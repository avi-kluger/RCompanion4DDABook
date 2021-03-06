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
# head(table9.1_df) 
# round(cor(table9.1_df), 2)

if (!require("lavaan")) install.packages("lavaan"); 
suppressPackageStartupMessages(library(lavaan))


# Define an indsitguisable model

OneWithManyIndistinguisableImitateSPSS <- '
# Actor effect:
      Actor   =~ 1*mfanx + 1*mcanx + 1*myanx

# Partner effect:
      Partner =~ 1*fmanx + 1*cmanx + 1*ymanx

# Relationship effects:
      # relationship.cm =~ 1* cmanx
      # relationship.fm =~ 1* fmanx
      # relationship.mc =~ 1* mcanx
      # relationship.mf =~ 1* mfanx
      # relationship.my =~ 1* myanx
      # relationship.ym =~ 1* ymanx

# Relationship :
       myanx ~~ va* myanx
       mfanx ~~ va* mfanx
       mcanx ~~ va* mcanx
       cmanx ~~ vp* cmanx
       fmanx ~~ vp* fmanx
       ymanx ~~ vp* ymanx

# Generalized reciprocity:
      Actor   ~~ Partner

# Dyadic reciprocity:
      mfanx  ~~  dr*fmanx 
      mcanx  ~~  dr*cmanx
      myanx  ~~  dr*ymanx

# Variance labels

      Actor   ~~ Actor 
      Partner ~~ Partner 

      # relationship.mc ~~ rva *  relationship.mc
      # relationship.mf ~~ rva *  relationship.mf
      # relationship.my ~~ rva *  relationship.my
      # relationship.cm ~~ rvp *  relationship.cm
      # relationship.fm ~~ rvp *  relationship.fm
      # relationship.ym ~~ rvp *  relationship.ym

# Intercepts

    mcanx ~ ia * 1
    mfanx ~ ia * 1
    myanx ~ ia * 1

    cmanx ~ ip * 1
    fmanx ~ ip * 1
    ymanx ~ ip * 1
'


# Estimate the model 
fitOneWithManyIndistinguisableImitateSPSS <- 
                 sem(OneWithManyIndistinguisableImitateSPSS, 
                 data       = table9.1_df,  
                 orthogonal = TRUE,
                 mimic      = "EQS")
# Examine the model.
summary(fitOneWithManyIndistinguisableImitateSPSS, fit.measures = TRUE, 
        standardized = TRUE)

OneWithManyIndistinguisableImitate_fSRM <- '
# Actor effects:
    mActor   =~ 1*mfanx + 1*mcanx + 1*myanx
    
# Partner effects:
    mPartner =~ 1*fmanx + 1*cmanx + 1*ymanx
    
# Relationship effects:
    relationship.mc =~ 1* mcanx
    relationship.mf =~ 1* mfanx
    relationship.my =~ 1* myanx

    relationship.cm =~ 1* cmanx
    relationship.fm =~ 1* fmanx
    relationship.ym =~ 1* ymanx
    
# Fix observed variance to zero so it is "transfered" to the latent variables
    mcanx ~~ 0* mcanx
    mfanx ~~ 0* mfanx
    myanx ~~ 0* myanx

    cmanx ~~ 0* cmanx
    fmanx ~~ 0* fmanx
    ymanx ~~ 0* ymanx
    
# Generalized reciprocity:
    mActor   ~~ gr.m*mPartner
    
# Dyadic reciprocity:
    mfanx  ~~  dr*fmanx 
    mcanx  ~~  dr*cmanx
    myanx  ~~  dr*ymanx
    
# Variance labels
    mActor ~~ vam*mActor 
    mPartner ~~ vpm*mPartner 

# Constrain relationship variance within role to equality
    relationship.mc ~~ rv.a *  relationship.mc
    relationship.mf ~~ rv.a *  relationship.mf
    relationship.my ~~ rv.a *  relationship.my

    relationship.cm ~~ rv.p *  relationship.cm
    relationship.fm ~~ rv.p *  relationship.fm
    relationship.ym ~~ rv.p *  relationship.ym
'
# Estimate the model 
fitOneWithManyIndistinguisableImitate_fSRM <- 
                 sem(OneWithManyIndistinguisableImitate_fSRM, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")

# Examine the model.
summary(fitOneWithManyIndistinguisableImitate_fSRM, 
        fit.measures = TRUE, standardized=TRUE)

OneWithManyIndistinguisableModelIntercepts <- '
# Actor effect:
      Actor   =~ 1*mfanx + 1*mcanx + 1*myanx

# Partner effect:
      Partner =~ 1*fmanx + 1*cmanx + 1*ymanx

# Relationship effects:
      relationship.cm =~ 1* cmanx
      relationship.fm =~ 1* fmanx
      relationship.mc =~ 1* mcanx
      relationship.mf =~ 1* mfanx
      relationship.my =~ 1* myanx
      relationship.ym =~ 1* ymanx

# Relationship :
       myanx ~~ va* myanx
       mfanx ~~ va* mfanx
       mcanx ~~ va* mcanx
       cmanx ~~ vp* cmanx
       fmanx ~~ vp* fmanx
       ymanx ~~ vp* ymanx

# Generalized reciprocity:
      Actor   ~~ Partner

# Dyadic reciprocity:
      mfanx  ~~  dr*fmanx 
      mcanx  ~~  dr*cmanx
      myanx  ~~  dr*ymanx

# Variance labels

      Actor   ~~ Actor 
      Partner ~~ Partner 

      relationship.mc ~~ rva *  relationship.mc
      relationship.mf ~~ rva *  relationship.mf
      relationship.my ~~ rva *  relationship.my
      relationship.cm ~~ rvp *  relationship.cm
      relationship.fm ~~ rvp *  relationship.fm
      relationship.ym ~~ rvp *  relationship.ym

# Intercepts

    cmanx ~ ip * 1
    fmanx ~ ip * 1
    mcanx ~ ia * 1
    mfanx ~ ia * 1
    myanx ~ ia * 1
    ymanx ~ ip * 1
'


# Estimate the model 
fitOneWithManyIndistinguisableModelIntercepts <- 
                 sem(OneWithManyIndistinguisableModelIntercepts, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")

# Examine the model.
summary(fitOneWithManyIndistinguisableModelIntercepts, 
        fit.measures = TRUE, standardized=TRUE)

OneWithManyIndistinguisableEqualResiduals <- '
# Actor effect:
      Actor   =~ 1*mfanx + 1*mcanx + 1*myanx

# Partner effect:
      Partner =~ 1*fmanx + 1*cmanx + 1*ymanx

# Relationship effects:
      relationship.cm =~ 1* cmanx
      relationship.fm =~ 1* fmanx
      relationship.mc =~ 1* mcanx
      relationship.mf =~ 1* mfanx
      relationship.my =~ 1* myanx
      relationship.ym =~ 1* ymanx

# Relationship :
       myanx ~~ va* myanx
       mfanx ~~ va* mfanx
       mcanx ~~ va* mcanx
       cmanx ~~ vp* cmanx
       fmanx ~~ vp* fmanx
       ymanx ~~ vp* ymanx

# Generalized reciprocity:
      Actor   ~~ Partner

# Dyadic reciprocity:
      mfanx  ~~  dr*fmanx 
      mcanx  ~~  dr*cmanx
      myanx  ~~  dr*ymanx

# Variance labels

      Actor   ~~ Actor 
      Partner ~~ Partner 

      relationship.mc ~~ rv *  relationship.mc
      relationship.mf ~~ rv *  relationship.mf
      relationship.my ~~ rv *  relationship.my
      relationship.cm ~~ rv *  relationship.cm
      relationship.fm ~~ rv *  relationship.fm
      relationship.ym ~~ rv *  relationship.ym

# Intercepts

    cmanx ~ ip * 1
    fmanx ~ ip * 1
    mcanx ~ ia * 1
    mfanx ~ ia * 1
    myanx ~ ia * 1
    ymanx ~ ip * 1
'  
# Estimate the model 
fitOneWithManyIndistinguisableEqualResiduals <- 
                 sem(OneWithManyIndistinguisableEqualResiduals, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")

# Examine the model.
summary(fitOneWithManyIndistinguisableEqualResiduals, 
        fit.measures = TRUE, standardized=TRUE)
