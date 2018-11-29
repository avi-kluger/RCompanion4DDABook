################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 6 -- Figure 6.1
# TEST variance differences
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Read Table 3.5 (in SPSS format) from Kenny's book site
if (!require('foreign')) install.packages('foreign'); library('foreign')
table6.1_df <- read.spss("http://davidakenny.net/kkc/c6/chapter6.sav", 
               to.data.frame = TRUE, use.value.labels = FALSE)
colnames(table6.1_df) <- c(paste(
                           rep(c("closeness", "commitment", "satisfaction"), 2),
                           rep(c("W", "H"), each = 3), sep = "."), 
                           "lengthOfMarriage")
colnames(table6.1_df)

round(cor(table6.1_df), 3)

table6.1_df$satisfactionDiff <- table6.1_df$satisfaction.H - 
                                table6.1_df$satisfaction.W
table6.1_df$satisfactionSum  <- table6.1_df$satisfaction.H + 
                                table6.1_df$satisfaction.W

round(cor(table6.1_df[, c("satisfactionDiff", "satisfactionSum")])[1, 2], 3)

if (!require('psych')) install.packages('psych'); library('psych')
# Calculate correlation, with p value, and With confidence intervals
print(corr.test(table6.1_df[, c("satisfactionDiff", "satisfactionSum")]), 
      short = FALSE) 

if (!require('lavaan')) install.packages('lavann'); library('lavann')

Figure6.1.model <- ' closeness.W    ~~  commitment.W
                     closeness.W    ~~  satisfaction.W
                     commitment.W   ~~  satisfaction.W

                     closeness.H    ~~  commitment.H
                     closeness.H    ~~  satisfaction.H
                     commitment.H   ~~  satisfaction.H

                     closeness.W    ~~  commitment.H
                     closeness.W    ~~  satisfaction.H
                     commitment.W   ~~  satisfaction.H

                     closeness.H    ~~  commitment.W
                     closeness.H    ~~  satisfaction.W
                     commitment.H   ~~  satisfaction.W

                     closeness.H    ~~  closeness.W
                     commitment.H   ~~  commitment.W
                     satisfaction.H ~~  satisfaction.W

                     closeness.H    ~~  v1*closeness.H
                     commitment.H   ~~  v2*commitment.H
                     satisfaction.H ~~  v3*satisfaction.H

                     closeness.W    ~~  closeness.W
                     commitment.W   ~~  commitment.W
                     satisfaction.W ~~  satisfaction.W
'

Figure6.1.model
# Remove statements from core model
Figure6.1.model.no.wife.variance <- stringr::str_replace(Figure6.1.model,
"\\                  closeness.W    ~~  closeness.W
                     commitment.W   ~~  commitment.W
                     satisfaction.W ~~  satisfaction.W", "")
# Replace statements in core model
Figure6.1.model.constrain.variance <- paste(Figure6.1.model.no.wife.variance,"
                     closeness.W    ~~  v1*closeness.W
                     commitment.W   ~~  v2*commitment.W
                     satisfaction.W ~~  v3*satisfaction.W")
  
fit <- sem(Figure6.1.model, data = table6.1_df)
summary(fit, standardized=TRUE)


fitConstrainedVariances <- sem(Figure6.1.model.constrain.variance, 
           data = table6.1_df)
summary(fitConstrainedVariances, standardized=TRUE)
anova(fit, fitConstrainedVariances)
