################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 6 -- Figure 6.3
# TEST relationships differences
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
  
if (!require('lavaan')) install.packages('lavaan'); library('lavaan')
  
Figure6.3.model.unconstrained <- ' 
                       Wife =~ closeness.W + commitment.W +  satisfaction.W
                    Husband =~ closeness.H + commitment.H +  satisfaction.H
                       Wife ~~ Husband
                closeness.W ~~ closeness.H
               commitment.W ~~ commitment.H
             satisfaction.W ~~ satisfaction.H
'
   
fitUnconstrained <- sem(Figure6.3.model.unconstrained, 
                        data = table6.1_df, 
                        mimic = "EQS")
summary(fitUnconstrained, standardized=TRUE)
fitMeasures(fitUnconstrained, c("cfi","rmsea","srmr"))
  
Figure6.3.model.constrained.loadings <- ' 
                       Wife =~ closeness.W + a*commitment.W +  b*satisfaction.W
                    Husband =~ closeness.H + a*commitment.H +  b*satisfaction.H
                       Wife ~~ Husband
                closeness.W ~~ closeness.H
               commitment.W ~~ commitment.H
             satisfaction.W ~~ satisfaction.H
'
   
fit.constrained.loadings <- sem(Figure6.3.model.constrained.loadings, 
                                data = table6.1_df, 
                                mimic = "EQS")
summary(fit.constrained.loadings, standardized=TRUE)
  
fitMeasures(fit.constrained.loadings, c("cfi","rmsea","srmr"))
  
# Constrain with equal latent variances 
Figure6.3.model.constrained.all <- ' 
                       Wife =~ closeness.W + a*commitment.W +  b*satisfaction.W
                    Husband =~ closeness.H + a*commitment.H +  b*satisfaction.H
                       Wife ~~ Husband
                closeness.W ~~ closeness.H
               commitment.W ~~ commitment.H
             satisfaction.W ~~ satisfaction.H
                       Wife ~~ v*Wife
                    Husband ~~ v*Husband
  '
   
fit.constrained.all <- sem(Figure6.3.model.constrained.all, 
                           data = table6.1_df, 
                           mimic = "EQS")
anova(fitUnconstrained, fit.constrained.loadings, fit.constrained.all)