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
table7.1_df <- read.spss("http://davidakenny.net/kkc/c7/roommate.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)
# The data is already organized as pairwise
head(table7.1_df) 

if (!require("nlme")) install.packages("nlme"); 
suppressMessages(library(nlme))

# Demonstrate mlm without preparing an effect code
mlm         <- gls(SATISFACTION   ~ ACT_HOUSE + PART_HOUSE, 
                   correlation = corCompSymm(form = ~1|Dyad),
                   data = table7.1_df)
summary(mlm)
getVarCov(mlm)
intervals(mlm)
confint (mlm, method= 'profile')

if (!require("lme4")) install.packages("lme4"); 
suppressPackageStartupMessages(library(lme4))

# Run random intercept model
lmerModel <- lmer(SATISFACTION  ~ ACT_HOUSE + PART_HOUSE + 
                (1 | Dyad), data = table7.1_df)
summary(lmerModel)

head(table7.1_df)
member1_df <- table7.1_df[c(TRUE, FALSE), ]
colnames(member1_df) <- paste0(colnames(member1_df), ".1")
member2_df <- table7.1_df[c(FALSE, TRUE), ]
colnames(member2_df) <- paste0(colnames(member2_df), ".2")
dyad_df    <- cbind(member1_df, member2_df)
head(dyad_df)

if (!require("lavaan")) install.packages("lavaan"); 
suppressPackageStartupMessages(library(lavaan))

APIM_indistinguisable <- '
  SATISFACTION.1   ~ a*ACT_HOUSE.1  # Person 1 actor effect
  SATISFACTION.2   ~ a*ACT_HOUSE.2  # Person 2 actor effect
  SATISFACTION.1   ~ p*ACT_HOUSE.2 # Person 1 partner effect
  SATISFACTION.2   ~ p*ACT_HOUSE.1 # Person 2 partner effect
  ACT_HOUSE.1      ~ mx*1 # Mean for X for Person 1
  ACT_HOUSE.2      ~ mx*1 # Mean for X for Person 2
  SATISFACTION.1   ~ iy*1              # Intercept for Y for Person 1
  SATISFACTION.2   ~ iy*1              # Intercept for Y for Person 2
  ACT_HOUSE.1     ~~ vx*ACT_HOUSE.1    # Variance for X for Person 1
  ACT_HOUSE.2     ~~ vx*ACT_HOUSE.2    # Variance for X for Person 2
  SATISFACTION.1  ~~ ve*SATISFACTION.1 # Error variance for Y for Person 1
  SATISFACTION.2  ~~ ve*SATISFACTION.2 # Error variance for Y for Person 2
  ACT_HOUSE.1     ~~ cx*ACT_HOUSE.2    # Covariance of X between persons
  SATISFACTION.1  ~~ cy*SATISFACTION.2 # Covariance of errors between persons
'

# Estimate the model 
apimIndistinguisable <- sem(APIM_indistinguisable,
                            data = dyad_df,
                            mimic = "EQS")
# Examine the model.
summary(apimIndistinguisable, fit.measures = TRUE)

APIM_indistinguisableActorEqualPartner <- '
  SATISFACTION.1   ~ a*ACT_HOUSE.1  # Person 1 actor effect
  SATISFACTION.2   ~ a*ACT_HOUSE.2  # Person 2 actor effect
  SATISFACTION.1   ~ p*ACT_HOUSE.2 # Person 1 partner effect
  SATISFACTION.2   ~ p*ACT_HOUSE.1 # Person 2 partner effect
  ACT_HOUSE.1      ~ mx*1 # Mean for X for Person 1
  ACT_HOUSE.2      ~ mx*1 # Mean for X for Person 2
  SATISFACTION.1   ~ iy*1              # Intercept for Y for Person 1
  SATISFACTION.2   ~ iy*1              # Intercept for Y for Person 2
  ACT_HOUSE.1     ~~ vx*ACT_HOUSE.1    # Variance for X for Person 1
  ACT_HOUSE.2     ~~ vx*ACT_HOUSE.2    # Variance for X for Person 2
  SATISFACTION.1  ~~ ve*SATISFACTION.1 # Error variance for Y for Person 1
  SATISFACTION.2  ~~ ve*SATISFACTION.2 # Error variance for Y for Person 2
  ACT_HOUSE.1     ~~ cx*ACT_HOUSE.2    # Covariance of X between persons
  SATISFACTION.1  ~~ cy*SATISFACTION.2 # Covariance of errors between persons
   a == p
'

# Estimate the model 
apimIndistinguisableActorEqualPartner <- sem(
                                         APIM_indistinguisableActorEqualPartner,
                                         data = dyad_df,
                                         mimic = "EQS")
# Examine the model.
summary(apimIndistinguisableActorEqualPartner, fit.measures = TRUE)
anova(apimIndistinguisableActorEqualPartner, apimIndistinguisable)



