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


OneWithMany <- '
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
    # Intercepts
    cmanx ~ icm * 1
    fmanx ~ ifm * 1
    mcanx ~ imc * 1
    mfanx ~ imf * 1
    myanx ~ imy * 1
    ymanx ~ iym * 1
'
# Estimate the model 
fitOneWithMany <- sem(OneWithMany, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")
# Examine the model.
summary(fitOneWithMany, fit.measures = TRUE, standardized = TRUE)


OneWithManyLabeled <- '
    # Actor effects:
    Actor   =~ 1*mfanx + 1*mcanx + 1*myanx
    
    # Partner effects:
    Partner =~ 1*fmanx + 1*cmanx + 1*ymanx
    
    # Relationship effects:
    relationship.cm =~ 1* cmanx
    relationship.fm =~ 1* fmanx
    relationship.mc =~ 1* mcanx
    relationship.mf =~ 1* mfanx
    relationship.my =~ 1* myanx
    relationship.ym =~ 1* ymanx
    
    # Fix observed variance to zero so it is "transfered" to the latent variables
    # cmanx ~~ 0* cmanx
    # fmanx ~~ 0* fmanx
    # mcanx ~~ 0* mcanx
    # mfanx ~~ 0* mfanx
    # myanx ~~ 0* myanx
    # ymanx ~~ 0* ymanx
    
    # Generalized reciprocity:
    Actor   ~~ gr*Partner
    
    # Dyadic reciprocity:
    mfanx  ~~  dr.fm*fmanx 
    mcanx  ~~  dr.cm*cmanx
    myanx  ~~  dr.ym*ymanx
    
    # Variance labels
    
    Actor ~~ vam*Actor 
    Partner ~~ vpm*Partner 
    
    relationship.cm ~~ rv.cm *  relationship.cm
    relationship.fm ~~ rv.fm *  relationship.fm
    relationship.mc ~~ rv.mc *  relationship.mc
    relationship.mf ~~ rv.mf *  relationship.mf
    relationship.my ~~ rv.my *  relationship.my
    relationship.ym ~~ rv.ym *  relationship.ym

    # Intercepts
    cmanx ~ icm * 1
    fmanx ~ ifm * 1
    mcanx ~ imc * 1
    mfanx ~ imf * 1
    myanx ~ imy * 1
    ymanx ~ iym * 1
    '

# Estimate the model 
fitOneWithManyLabeled <- sem(OneWithManyLabeled, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")

# Examine the model.
summary(fitOneWithManyLabeled, fit.measures = TRUE)
anova(fitOneWithManyLabeled, fitOneWithMany)

# Define an indsitguisable model

OneWithManyIndistinguisable <- '
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

    # Intercepts
    cmanx ~ ip * 1
    fmanx ~ ip * 1
    mcanx ~ ia * 1
    mfanx ~ ia * 1
    myanx ~ ia * 1
    ymanx ~ ip * 1
'

# Define an indsitguisable model with same relationship variances

OneWithManyIndistinguisableSameRelVar <- '
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

# Fix observed variance to zero so it is "transfered" to the latent variables
    cmanx ~~ 0* cmanx
    fmanx ~~ 0* fmanx
    mcanx ~~ 0* mcanx
    mfanx ~~ 0* mfanx
    myanx ~~ 0* myanx
    ymanx ~~ 0* ymanx

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

# Intercepts
cmanx ~ ip * 1
fmanx ~ ip * 1
mcanx ~ ia * 1
mfanx ~ ia * 1
myanx ~ ia * 1
ymanx ~ ip * 1
'
# Estimate the model 
fitOneWithManyIndistinguisable <- sem(OneWithManyIndistinguisable, 
                 data       = table9.1_df,  
                 orthogonal = TRUE,
                 mimic      = "EQS")
# Examine the model.
summary(fitOneWithManyIndistinguisable, fit.measures = TRUE, standardized = TRUE)



fitOneWithManyIndistinguisableSameRelVar <- 
                                      sem(OneWithManyIndistinguisableSameRelVar, 
                                      data       = table9.1_df,  
                                      orthogonal = TRUE,
                                      mimic      = "EQS")
anova(fitOneWithManyIndistinguisable, fitOneWithManyIndistinguisableSameRelVar)
summary(fitOneWithManyIndistinguisableSameRelVar, fit.measures = TRUE, standardized=TRUE)


family_SRM <- '
    # Family effect:
    # family   =~ 1*mfanx + 1*mcanx + 1*myanx + 1*fmanx + 1*cmanx + 1*ymanx 
    
    # Actor effects:
    mActor   =~ 1*mfanx + 1*mcanx + 1*myanx
    
    # Partner effects:
    mPartner =~ 1*fmanx + 1*cmanx + 1*ymanx
    
    # Relationship effects:
    relationship.cm =~ 1* cmanx
    relationship.fm =~ 1* fmanx
    relationship.mc =~ 1* mcanx
    relationship.mf =~ 1* mfanx
    relationship.my =~ 1* myanx
    relationship.ym =~ 1* ymanx
    
    # Fix observed variance to zero so it is "transfered" to the latent variables
    cmanx ~~ 0* cmanx
    fmanx ~~ 0* fmanx
    mcanx ~~ 0* mcanx
    mfanx ~~ 0* mfanx
    myanx ~~ 0* myanx
    ymanx ~~ 0* ymanx
    
    # Generalized reciprocity:
    mActor   ~~ gr.m*mPartner
    
    # Dyadic reciprocity:
    mfanx  ~~  dr*fmanx 
    mcanx  ~~  dr*cmanx
    myanx  ~~  dr*ymanx
    
    # Variance labels
    # family ~~ vf*family
    
    mActor ~~ vam*mActor 
    
    mPartner ~~ vpm*mPartner 
    
    relationship.cm ~~ rvp *  relationship.cm
    relationship.fm ~~ rvp *  relationship.fm
    relationship.mc ~~ rva *  relationship.mc
    relationship.mf ~~ rva *  relationship.mf
    relationship.my ~~ rva *  relationship.my
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
familySRM <- sem(family_SRM, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS",
                 estimator ="MLM")
# Examine the model.
summary(familySRM, fit.measures = TRUE, standardized=TRUE)
