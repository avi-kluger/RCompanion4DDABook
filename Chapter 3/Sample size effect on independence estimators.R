################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.5
# COMPUTE ICC in three different ways
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
table3.5_df <- read.spss("http://davidakenny.net/kkc/c3/table3.5.sav", 
               to.data.frame = TRUE)

# Compute ICC
# Using correlation
Pearson.r     <- round(cor(table3.5_df[, c("H", "W")])[1, 2], 2)

# Using ANOVA

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# Transform dyad df into individual df, and keep only the dyad and score
individual_df <- table3.5_df %>% gather(dyadMember, score, H:W) 

fit           <- aov(score ~ factor(dyadMember) + Error(factor(DYAD)), 
                     data = individual_df)
MSdyad        <- summary(fit)[[1]][[1]][[3]]
MSresidual    <- as.data.frame(unlist(summary(fit)[[2]]))["Mean Sq2", ]
ANOVA.ICC     <- (MSdyad - MSresidual) / (MSdyad + MSresidual) 
ANOVA.ICC     <- round(ANOVA.ICC, 2)
ANOVA.ICC == Pearson.r

# Using MLM

if (!require("nlme")) install.packages("nlme"); library(nlme)

mlm     <- gls(score   ~ 1, correlation = corCompSymm(form = ~1|DYAD),
               data = individual_df)
icc     <- intervals(mlm) ["corStruct"]
MLM.ICC <- round(as.data.frame(icc), 2)
MLM.ICC <- as.numeric(MLM.ICC[2])

cat(paste0("
    Pearson.r = ",  Pearson.r,"
    ANOVA.ICC = ",  ANOVA.ICC,"
    MLM.ICC   = ",  MLM.ICC))

# Repeat with sample size DOUBLED

table3.5_df <- rbind(table3.5_df, table3.5_df)

# Compute ICC
# Using correlation
Pearson.r     <- round(cor(table3.5_df[, c("H", "W")])[1, 2], 2)

# Using ANOVA

# Transform dyad df into individual df, and keep only the dyad and score
individual_df <- table3.5_df %>% gather(dyadMember, score, H:W) 
individual_df$DYAD <- rep(1:10, 2)

fit           <- aov(score ~ factor(dyadMember) + Error(factor(DYAD)), 
                     data = individual_df)
MSdyad        <- summary(fit)[[1]][[1]][[3]]
MSresidual    <- as.data.frame(unlist(summary(fit)[[2]]))["Mean Sq2", ]
ANOVA.ICC     <- (MSdyad - MSresidual) / (MSdyad + MSresidual) 
ANOVA.ICC     <- round(ANOVA.ICC, 2)
ANOVA.ICC == Pearson.r

# Using MLM

mlm     <- gls(score   ~ 1, correlation = corCompSymm(form = ~1|DYAD),
               data = individual_df)
icc     <- intervals(mlm) ["corStruct"]
MLM.ICC <- round(as.data.frame(icc), 2)
MLM.ICC <- as.numeric(MLM.ICC[2])

cat(paste0("
    Pearson.r = ",  Pearson.r,"
    ANOVA.ICC = ",  ANOVA.ICC,"
    MLM.ICC   = ",  MLM.ICC))

