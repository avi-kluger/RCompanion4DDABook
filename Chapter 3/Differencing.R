################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 3 -- Table 3.5
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
table3.5_df <- haven::read_sav("http://davidakenny.net/kkc/c3/table3.5.sav")

t.test(table3.5_df$DIFF)
fit         <- lm(DIFF ~ GDIFF + 0, data = table3.5_df)
summary(fit)$coefficients
fit         <- lm(DIFF2 ~ GDIFF2 + 0, data = table3.5_df)
summary(fit)$coefficients

t           <- as.numeric(summary(fit)$coefficients[3])
rd          <- as.numeric(t/sqrt(t^2 + summary(fit)$df[2]))

# Compute ICC
# Transform dyad df into individual (long) df
l <- table3.5_df %>% 
  select(DYAD:W) %>% 
  pivot_longer(                           
    cols      = !DYAD,
    names_to  = "gender",
    values_to = "score")
# Create an effect code (EC) for gender. Necessary only if user wants to control
# the sign of the regression; otherwise use gender; gls will 
# treat gender as a dummy variable and print the group being used
# as the reference group.
l$genderEC <- ifelse(l$gender == "H", 1, -1)

# Create a dummy code for gender, for obtaining standardized effect
l$genderDC <- ifelse(l$gender == "H", 1, 0)

if (!require("nlme")) install.packages("nlme"); suppressMessages(library(nlme))

# Demonstrate mlm without preparing an effect code
mlm         <- gls(score   ~ gender, 
                   correlation = corCompSymm(form = ~1|DYAD),
                   data = l)
summary(mlm)

# Demonstrate mlm with effect code
mlm         <- gls(score   ~ genderEC, 
                   correlation = corCompSymm(form = ~1|DYAD),
                   data = l)
summary(mlm)

# Demonstrate mlm with standardized varialbes
mlmS        <- gls(scale(score)   ~ scale(genderDC), 
                   correlation = corCompSymm(form = ~1|DYAD),
                   data = l)
summary(mlmS)

# NOTE: ICC in the book is .33, which is an error. ICC = .43
icc         <- intervals(mlm) ["corStruct"]
icc         <- round(as.data.frame(icc), 2)
r           <- rd * sqrt(1 - icc[2])

# Obtain the book's result for r if using erroneous ICC
rd * sqrt(1 - .33)

cat(paste0("\nrD = ", rd, " and adjusted r = ", round(as.numeric(r), 2), "\n"))

if (!require("compute.es")) install.packages("compute.es"); 
suppressPackageStartupMessages(library(compute.es))
# Convert Pearson's r to Cohen's d and other effect sizes
res(as.numeric(r), n = nrow(l))

# d calculation based on Within Dyad formula and ICC based on Pearson's r  
r   <- round(cor(table3.5_df[, c("H", "W")]), 2)[1, 2]
r
fit <- (t.test(table3.5_df$H, table3.5_df$W, 
       paired=TRUE, 
       conf.level=0.95))
dD  <- (as.numeric(fit$statistic) * sqrt(2))/sqrt(nrow(table3.5_df))
d   <- dD * sqrt(1 - r)
dD
d

# Convert Cohen's d to Pearson's r and other effect sizes
des(dD, n.1 = nrow(table3.5_df), n.2 = nrow(table3.5_df))
des(d, n.1 = nrow(table3.5_df), n.2 = nrow(table3.5_df))
