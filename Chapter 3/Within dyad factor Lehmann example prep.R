################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.3
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Example data DIFFERENT FROM THE BOOK based on Lehmann study of humility

library(readr)
Humility <- read_csv("Chapter 3/Lehmann Humility experiement.csv") [, -1]

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
group_by(Humility, Condition) %>% summarize(m.l = mean(Humility.listener),
                                            sd.l = sd(Humility.listener),
                                            m.s = mean(Humility.Speaker),
                                            sd.s = sd(Humility.Speaker))

# Non-Distraction condition only
Humility <- Humility[which(Humility$Condition == 1), ]
write.csv(Humility[, 1:2], "Chapter 3/Lehmann Humility data.csv", 
          row.names = FALSE)

# ICC based on Pearson's r

r <- round(cor(Humility[, c("Humility.listener", "Humility.Speaker")]), 2)[1, 2]

fit <- (t.test(Humility$Humility.listener, 
       Humility$Humility.Speaker, 
       paired=TRUE, 
       conf.level=0.95))
dD <- (as.numeric(fit$statistic) * sqrt(2))/sqrt(nrow(Humility))
d <- dD * sqrt(1 - r)
dD
d
