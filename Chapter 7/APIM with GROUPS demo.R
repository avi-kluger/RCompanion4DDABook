rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
options(scipen = 15)                          # Avoid scientific notation 

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, 
# then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, 
                                        require, character.only = TRUE))
}

ipak (c("lavaan", "semPlot")) 
################################################################################

# loading data 

experimentAPIM_df <- read.csv(
     "https://www.dropbox.com/s/q60hhzcmg77p4jv/demoExperimentAPIM_df.csv?dl=1")

names(experimentAPIM_df)

# Standardized the data across dyads
anxietyMean <- mean(c(experimentAPIM_df$Lanxiety, experimentAPIM_df$Sanxiety))
anxietySD   <- sd  (c(experimentAPIM_df$Lanxiety, experimentAPIM_df$Sanxiety))
spsMean     <- mean(c(experimentAPIM_df$L_SPS, experimentAPIM_df$S_SPS))
spsSD       <- sd  (c(experimentAPIM_df$L_SPS, experimentAPIM_df$S_SPS))

experimentAPIM_df$Lanxiety <- (experimentAPIM_df$Lanxiety - anxietyMean)/
                               anxietySD 
experimentAPIM_df$Sanxiety <- (experimentAPIM_df$Sanxiety - anxietyMean)/
                               anxietySD 
experimentAPIM_df$L_SPS    <- (experimentAPIM_df$L_SPS - spsMean)/spsSD 
experimentAPIM_df$S_SPS    <- (experimentAPIM_df$S_SPS - spsMean)/spsSD


#######################Specify the APIM model###################################

unconstrained <- '
Lanxiety  ~ c(a1d,a1notd)*L_SPS      # Listener actor effect                                                             
Sanxiety  ~ c(a2d,a2notd)*S_SPS      # Speaker actor effect
Lanxiety  ~ c(p12d,p12notd)*S_SPS    # Speaker to Listener partner effect
Sanxiety  ~ c(p21d,p21notd)*L_SPS    # Listener to Speaker partner effect
L_SPS     ~ c(mx1d,mx1notd)*1        # Mean for X for Listener
S_SPS     ~ c(mx2d,mx2notd)*1        # Mean for X for Speaker
Lanxiety  ~ c(iy1d,iy1notd)*1        # Intercept for Y for Listener
Sanxiety  ~ c(iy2d,iy2notd)*1        # Intercept for Y for Speaker
L_SPS    ~~ c(vx1d,vx1notd)*L_SPS    # Variance for X for Listener
S_SPS    ~~ c(vx2d,vx2notd)*S_SPS    # Variance for X for Speaker
Lanxiety ~~ c(ve1d,ve1notd)*Lanxiety # Error variance for Y for Listener
Sanxiety ~~ c(ve2d,ve2notd)*Sanxiety # Error variance for Y for Speaker
S_SPS    ~~ c(cxd,cxnotd)*L_SPS      # Covariance of X between Listener and Speaker
Sanxiety ~~ c(cyd,cynotd)*Lanxiety   # Covariance of errors between Listener and Speaker
'

constrain <- '
Lanxiety  ~ c(a1d,a1notd)*L_SPS      # Listener actor effect                                                             
Sanxiety  ~ c(a2d,a2notd)*S_SPS      # Speaker actor effect
Lanxiety  ~ c(p12d,p12notd)*S_SPS    # Speaker to Listener partner effect
Sanxiety  ~ c(p21d,p21notd)*L_SPS    # Listener to Speaker partner effect
L_SPS     ~ c(mx1d,mx1notd)*1        # Mean for X for Listener
S_SPS     ~ c(mx2d,mx2notd)*1        # Mean for X for Speaker
Lanxiety  ~ c(iy1d,iy1notd)*1        # Intercept for Y for Listener
Sanxiety  ~ c(iy2d,iy2notd)*1        # Intercept for Y for Speaker
L_SPS    ~~ c(vx1d,vx1notd)*L_SPS    # Variance for X for Listener
S_SPS    ~~ c(vx2d,vx2notd)*S_SPS    # Variance for X for Speaker
Lanxiety ~~ c(ve1d,ve1notd)*Lanxiety # Error variance for Y for Listener
Sanxiety ~~ c(ve2d,ve2notd)*Sanxiety # Error variance for Y for Speaker
S_SPS    ~~ c(cxd,cxnotd)*L_SPS      # Covariance of X between Listener and Speaker
Sanxiety ~~ c(cyd,cynotd)*Lanxiety   # Covariance of errors between Listener and Speaker

# SPS should not be affected by condition (d vs. notd)
    mx1d == mx1notd
    mx2d == mx2notd
    vx1d == vx1notd
    vx2d == vx2notd 

# SPS should not be affected by role (listener -1 vs. speaker --2)
    mx1d == mx2d     # mean for SPS d   
    vx1d == vx2d     # SD for SPS d   
'

constrain <- '
Lanxiety  ~ c(a1d,a1notd)*L_SPS      # Listener actor effect                                                             
Sanxiety  ~ c(a2d,a2notd)*S_SPS      # Speaker actor effect
Lanxiety  ~ c(p12d,p12notd)*S_SPS    # Speaker to Listener partner effect
Sanxiety  ~ c(p21d,p21notd)*L_SPS    # Listener to Speaker partner effect
L_SPS     ~ c(m, m)*1        # Mean for X for Listener
S_SPS     ~ c(m, m)*1        # Mean for X for Speaker
Lanxiety  ~ c(iy1d,iy1notd)*1        # Intercept for Y for Listener
Sanxiety  ~ c(iy2d,iy2notd)*1        # Intercept for Y for Speaker
L_SPS    ~~ c(vx, vx)*L_SPS    # Variance for X for Listener
S_SPS    ~~ c(vx, vx)*S_SPS    # Variance for X for Speaker
Lanxiety ~~ c(ve1d,ve1notd)*Lanxiety # Error variance for Y for Listener
Sanxiety ~~ c(ve2d,ve2notd)*Sanxiety # Error variance for Y for Speaker
S_SPS    ~~ c(cxd,cxnotd)*L_SPS      # Covariance of X between Listener and Speaker
Sanxiety ~~ c(cyd,cynotd)*Lanxiety   # Covariance of errors between Listener and Speaker
'

constrainSpeakerEffects <- paste0(constrain, '
                                a2d  == a2notd
                                p12d == p12notd  ')

constrainListnerEffects <- paste0(constrainSpeakerEffects, '
                                a1d  == a1notd
                                p21d == p21notd  ')

################################################################################
#cyd  == cynotd
#cxd  == cxnotd

#ve1notd == ve2notd ## Error variance notd
#ve1d    == ve2d    ## Error variance d
#p12notd  == p21notd  # partner effects notd
#iy1notd  == iy2notd  # Intercept for anxiety notd 
#vx1notd  == vx2notd  # Variance for SPS notd 
#a1notd   == a2notd   # actor effects of SPS on anxiety notd
#p12d     == p21d     # partner effects d
#a2d  == a2notd
#a1d  == a1notd
#mx1d == mx1notd
#mx2d == mx2notd
#p21d == p21notd
#p12d == p12notd
#iy2d == iy2notd
#iy1d == iy1notd
#a1d  == a2d      # actor effects of SPS on anxiety d

#vx1notd  == vx2notd  # Variance for SPS d
#iy1d     == iy2d     # Intercept for anxiety d 


################################################################################
# Estimate the models########################################################### 
MyModel <- function(apimModle){sem(apimModle, 
                                   fixed.x=FALSE, 
                                   data = experimentAPIM_df,    
                                   missing="fiml",
                                   group = "Condition.f")}

fitUnconstrained           <- MyModel(unconstrained)
fitConstrain               <- MyModel(constrain)
fitConstrainSpeakerEffects <- MyModel(constrainSpeakerEffects)
fitConstrainListnerEffects <- MyModel(constrainListnerEffects)

anova(fitUnconstrained, fitConstrain, fitConstrainSpeakerEffects,
      fitConstrainListnerEffects)

################################################################################
# Examine the models############################################################ 
ExamineMyM <- function(EstimateM){summary(EstimateM, 
                                          standardized = FALSE, 
                                          fit.measures = TRUE)}
ExamineMyM(fitUnconstrained)
ExamineMyM(fitConstrain)

anova(fitUnconstrained, fitConstrain)

################################################################################

semPaths(fitUnconstrained, 
         "est",
         sizeMan = 15,
         residuals = FALSE,
         intercepts = FALSE, 
         rotation = 2,
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         sizeInt = 25,
         edge.label.cex = 1.5,
         label.prop = .8,
         edge.label.position = c(0.5,0.5,0.3,0.3,0.5,0.5,0.5,0.5),
         nodeLabels=c("Listener\nanxiety",   "Speaker\nanxiety",
                      "Listener\nSPS",       "Speaker\nSPS")
         )






