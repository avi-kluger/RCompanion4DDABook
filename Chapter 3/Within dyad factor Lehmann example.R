################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.3
# Data for Table 3.3 is not provided in the book's site.  Alternative dataset
# is provided here.  These data (Lehmann, 2018) are ratings of humility by 
# listeners and speakers (within dyad variable) in a control group of a larger
# experiment.
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Example data DIFFERENT FROM THE BOOK based on Lehmann study of humility

urlRemote   <- "https://raw.githubusercontent.com/"
pathGithub  <- "avi-kluger/RCompanion4DDABook/master/Chapter%203/"
fileName    <- "Lehmann%20Humility%20data.csv"

humility_df <- read.csv(paste0(urlRemote, pathGithub, fileName))


if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require("sjPlot")) install.packages("sjPlot"); library(sjPlot)

humility_df %>% 
  summarize(m.l  = mean(Humility.listener),
            sd.l = sd(Humility.listener),
            m.s  = mean(Humility.Speaker),
            sd.s = sd(Humility.Speaker)) %>% 
  tab_df(footnote = paste0("N = ", nrow(humility_df)), show.footnote = TRUE)

# ICC based on Pearson's r
r <- humility_df %>% 
  dplyr::select(Humility.listener, Humility.Speaker) %>% 
  cor %>% 
  round(3)
r <- r[1, 2]
r

# ICC 
if (!require("psych")) install.packages("psych"); library(psych)
icc <- humility_df %>% 
  dplyr::select(Humility.listener, Humility.Speaker) %>% 
  ICC
icc <- icc$results$ICC[1] %>% round(3)
icc

# Repeated measure t-test
fit <- t.test(humility_df$Humility.listener, 
       humility_df$Humility.Speaker, 
       paired=TRUE, 
       conf.level=0.95)
fit
# Formula from p. 64
t   <- as.numeric(fit$statistic)
N   <- nrow(humility_df)
dD  <- 2 * t / sqrt(N)
d   <- dD * sqrt(1 - icc)
dD %>% round(3)
d  %>% round(3)

