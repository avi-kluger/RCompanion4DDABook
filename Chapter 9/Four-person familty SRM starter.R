################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 9 -- SRM with roles
# Four-Person Model
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read (in SPSS format) from Kenny's book site and replicate Table 9.1
if (!require('foreign')) install.packages('foreign'); library('foreign')
table9.1_df <- read.spss("http://davidakenny.net/kkc/c9/4person.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)

table9.1_df <- table9.1_df[, -1]
head(table9.1_df) 
round(cor(table9.1_df), 2)
round(colMeans(table9.1_df),2)
round(apply(table9.1_df, 2, sd), 2)

# Transform wide to long format
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

table9.1_df$family.id <- 1:nrow(table9.1_df)
anx_long_df <- gather(table9.1_df, actorPartnerMeasure, anx, -family.id) 

anx_long_df$actor.id   <- substr(anx_long_df$actorPartnerMeasure, 1, 1)
anx_long_df$partner.id <- substr(anx_long_df$actorPartnerMeasure, 2, 2)

if (!require("fSRM")) install.packages("fSRM"); 
suppressPackageStartupMessages(library(fSRM))

SRM <- fSRM(anx ~ actor.id*partner.id|family.id, data = anx_long_df)
SRM
SRMIGSIM <- fSRM(anx ~ actor.id*partner.id|family.id, data = anx_long_df, 
            IGSIM=list(c("m", "f"), c("c", "y")))
SRMIGSIM

cat(SRM$syntax)
cat(SRMIGSIM$syntax)

familySRM <- sem(family_SRM, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")

if (!require("lavaan")) install.packages("lavaan"); 
suppressPackageStartupMessages(library(lavaan))

familySRMIntrageneartionalSimilarity <- update(familySRM, add =
                                                 "# intragenerational similarity:
                                               mActor   ~~ mfActor  *fActor
                                               mPartner ~~ mfPartner*fPartner
                                               cActor   ~~ cyActor  *yActor
                                               cPartner ~~ cyPartner*yPartner")                         
anova(familySRM, familySRMIntrageneartionalSimilarity)


family_SRMminimalLaeling <- '
# Family effect:
family   =~ 1*mfanx + 1*mcanx + 1*myanx + 1*fmanx + 1*fcanx + 1*fyanx + 
1*cmanx + 1*cfanx + 1*cyanx + 1*ymanx + 1*yfanx + 1*ycanx

# Actor effects:
mActor   =~ 1*mfanx + 1*mcanx + 1*myanx
fActor   =~ 1*fmanx + 1*fcanx + 1*fyanx
cActor   =~ 1*cmanx + 1*cfanx + 1*cyanx
yActor   =~ 1*ymanx + 1*yfanx + 1*ycanx

# Partner effects:
mPartner =~ 1*fmanx + 1*cmanx + 1*ymanx
fPartner =~ 1*mfanx + 1*cfanx + 1*yfanx
cPartner =~ 1*mcanx + 1*fcanx + 1*ycanx
yPartner =~ 1*myanx + 1*fyanx + 1*cyanx

# Generalized reciprocity:
mActor   ~~ gr.m*mPartner
fActor   ~~ gr.f*fPartner
cActor   ~~ gr.c*cPartner
yActor   ~~ gr.y*yPartner

# Dyadic reciprocity:
mfanx  ~~  dr.fm*fmanx
mcanx  ~~  dr.cm*cmanx
myanx  ~~  dr.ym*ymanx
fcanx  ~~  dr.cf*cfanx
fyanx  ~~  dr.yf*yfanx
cyanx  ~~  dr.yc*ycanx
'
# Estimate the model 
familySRMminimalLaeling <- sem(family_SRMminimalLaeling, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")
# Examine the model.
summary(familySRMminimalLaeling, fit.measures = TRUE, standardized = TRUE)

family_SRM <- '
# Family effect:
      family   =~ 1*mfanx + 1*mcanx + 1*myanx + 1*fmanx + 1*fcanx + 1*fyanx + 
                  1*cmanx + 1*cfanx + 1*cyanx + 1*ymanx + 1*yfanx + 1*ycanx

# Actor effects:
      mActor   =~ 1*mfanx + 1*mcanx + 1*myanx
      fActor   =~ 1*fmanx + 1*fcanx + 1*fyanx
      cActor   =~ 1*cmanx + 1*cfanx + 1*cyanx
      yActor   =~ 1*ymanx + 1*yfanx + 1*ycanx

# Partner effects:
      mPartner =~ 1*fmanx + 1*cmanx + 1*ymanx
      fPartner =~ 1*mfanx + 1*cfanx + 1*yfanx
      cPartner =~ 1*mcanx + 1*fcanx + 1*ycanx
      yPartner =~ 1*myanx + 1*fyanx + 1*cyanx

# Relationship effects:
      relationship.cf =~ 1* cfanx
      relationship.cm =~ 1* cmanx
      relationship.cy =~ 1* cyanx
      relationship.fc =~ 1* fcanx
      relationship.fm =~ 1* fmanx
      relationship.fy =~ 1* fyanx
      relationship.mc =~ 1* mcanx
      relationship.mf =~ 1* mfanx
      relationship.my =~ 1* myanx
      relationship.yc =~ 1* ycanx
      relationship.yf =~ 1* yfanx
      relationship.ym =~ 1* ymanx

# Relationship :
      cfanx ~~ 0* cfanx
      cmanx ~~ 0* cmanx
      cyanx ~~ 0* cyanx
      fcanx ~~ 0* fcanx
      fmanx ~~ 0* fmanx
      fyanx ~~ 0* fyanx
      mcanx ~~ 0* mcanx
      mfanx ~~ 0* mfanx
      myanx ~~ 0* myanx
      ycanx ~~ 0* ycanx
      yfanx ~~ 0* yfanx
      ymanx ~~ 0* ymanx

# Generalized reciprocity:
      mActor   ~~ gr.m*mPartner
      fActor   ~~ gr.f*fPartner
      cActor   ~~ gr.c*cPartner
      yActor   ~~ gr.y*yPartner

# Dyadic reciprocity:
      mfanx  ~~  dr.fm*fmanx 
      mcanx  ~~  dr.cm*cmanx
      myanx  ~~  dr.ym*ymanx
      fcanx  ~~  dr.cf*cfanx
      fyanx  ~~  dr.yf*yfanx
      cyanx  ~~  dr.yc*ycanx

# Variance labels
      family ~~ vf*family

      mActor ~~ vam*mActor 
      fActor ~~ vaf*fActor 
      cActor ~~ vac*cActor  
      yActor ~~ vay*yActor   

      mPartner ~~ vpm*mPartner 
      fPartner ~~ vpf*fPartner 
      cPartner ~~ vpc*cPartner  
      yPartner ~~ vpy*yPartner

      relationship.cf ~~ rv.cf *  relationship.cf
      relationship.cm ~~ rv.cm *  relationship.cm
      relationship.cy ~~ rv.cy *  relationship.cy
      relationship.fc ~~ rv.fc *  relationship.fc
      relationship.fm ~~ rv.fm *  relationship.fm
      relationship.fy ~~ rv.fy *  relationship.fy
      relationship.mc ~~ rv.mc *  relationship.mc
      relationship.mf ~~ rv.mf *  relationship.mf
      relationship.my ~~ rv.my *  relationship.my
      relationship.yc ~~ rv.yc *  relationship.yc
      relationship.yf ~~ rv.yf *  relationship.yf
      relationship.ym ~~ rv.ym *  relationship.ym

'

# Estimate the model 
familySRM <- sem(family_SRM, 
                 data = table9.1_df,  
                 orthogonal = TRUE,
                 mimic = "EQS")
# Examine the model.
summary(familySRM, fit.measures = TRUE)

familySRMequalActorVariance <- update(familySRM, add = "vam == vaf
                                                        vam == vac
                                                        vam == vay
                                                        vac == vay")                         
anova(familySRM, familySRMequalActorVariance)