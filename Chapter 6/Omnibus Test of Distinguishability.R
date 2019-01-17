################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 6 -- Figure 6.4
# Omnibus test of distinguisability
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

Figure6.4.model.unconstrained <- ' 
                     closeness.W  ~~  commitment.W
                     closeness.W  ~~  satisfaction.W
                     commitment.W ~~  satisfaction.W

                     closeness.H  ~~  commitment.H
                     closeness.H  ~~  satisfaction.H
                     commitment.H ~~  satisfaction.H

                     closeness.W  ~~  commitment.H
                     closeness.W  ~~  satisfaction.H
                     commitment.W ~~  satisfaction.H

                     closeness.H  ~~  commitment.W
                     closeness.H  ~~  satisfaction.W
                     commitment.H ~~  satisfaction.W

                     closeness.H  ~~  closeness.W
                    commitment.H  ~~  commitment.W
                   satisfaction.H ~~  satisfaction.W

                     closeness.H  ~~  closeness.H
                    commitment.H  ~~  commitment.H
                   satisfaction.H ~~  satisfaction.H

                     closeness.W  ~~  closeness.W
                    commitment.W  ~~  commitment.W
                   satisfaction.W ~~  satisfaction.W
'

fitUnconstrained <- sem(Figure6.4.model.unconstrained, 
                        data = table6.1_df,
                        meanstructure = TRUE,
                        mimic = "EQS")
summary(fitUnconstrained, standardized=TRUE)


# Constrain relationships, variances, and means (intercepts)
Figure6.4.model.constrain.all <- ' 
                     closeness.W  ~~  w*commitment.W
                     closeness.W  ~~  bb*satisfaction.W
                     commitment.W ~~  nn*satisfaction.W

                     closeness.H  ~~  w*commitment.H
                     closeness.H  ~~  bb*satisfaction.H
                     commitment.H ~~  nn*satisfaction.H

                     closeness.W  ~~  gg*commitment.H
                     closeness.W  ~~  qq*satisfaction.H
                     commitment.W ~~  zz*satisfaction.H

                     closeness.H  ~~  gg*commitment.W
                     closeness.H  ~~  qq*satisfaction.W
                     commitment.H ~~  zz*satisfaction.W

                     closeness.H  ~~  closeness.W
                    commitment.H  ~~  commitment.W
                   satisfaction.H ~~  satisfaction.W

                     closeness.H  ~~  v1*closeness.H
                    commitment.H  ~~  v2*commitment.H
                   satisfaction.H ~~  v3*satisfaction.H

                     closeness.W  ~~  v1*closeness.W
                    commitment.W  ~~  v2*commitment.W
                   satisfaction.W ~~  v3*satisfaction.W

                     closeness.H  ~  i1 * 1
                    commitment.H  ~  i2 * 1
                   satisfaction.H ~  i3 * 1

                     closeness.W  ~  i1 * 1
                    commitment.W  ~  i2 * 1
                   satisfaction.W ~  i3 * 1

'

fitConstrainedAll <- sem(Figure6.4.model.constrain.all, 
           data = table6.1_df, 
           mimic = "EQS")
summary(fitConstrainedAll, standardized=TRUE)
anova(fitUnconstrained, fitConstrainedAll)

# Constrain relationships, variances, and free means (intercepts)
Figure6.4.model.free.means <- ' 
                     closeness.W  ~~  w*commitment.W
                     closeness.W  ~~  bb*satisfaction.W
                     commitment.W ~~  nn*satisfaction.W

                     closeness.H  ~~  w*commitment.H
                     closeness.H  ~~  bb*satisfaction.H
                     commitment.H ~~  nn*satisfaction.H

                     closeness.W  ~~  gg*commitment.H
                     closeness.W  ~~  qq*satisfaction.H
                     commitment.W ~~  zz*satisfaction.H

                     closeness.H  ~~  gg*commitment.W
                     closeness.H  ~~  qq*satisfaction.W
                     commitment.H ~~  zz*satisfaction.W

                     closeness.H  ~~  closeness.W
                    commitment.H  ~~  commitment.W
                   satisfaction.H ~~  satisfaction.W

                     closeness.H  ~~  v1*closeness.H
                    commitment.H  ~~  v2*commitment.H
                   satisfaction.H ~~  v3*satisfaction.H

                     closeness.W  ~~  v1*closeness.W
                    commitment.W  ~~  v2*commitment.W
                   satisfaction.W ~~  v3*satisfaction.W

                     closeness.H  ~  i1 * 1
                    commitment.H  ~  i2 * 1
                   satisfaction.H ~  i3 * 1

                     closeness.W  ~  i4 * 1
                    commitment.W  ~  i5 * 1
                   satisfaction.W ~  i6 * 1
'

fitConstrainedNoMeans <- sem(Figure6.4.model.free.means,
                             data = table6.1_df,
                             meanstructure = TRUE, 
                             mimic = "EQS")
summary(fitConstrainedAll, standardized=TRUE)
anova(fitUnconstrained, fitConstrainedAll, fitConstrainedNoMeans)
anova(fitUnconstrained, fitConstrainedAll)
