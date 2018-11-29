################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 6 -- Table 6.2
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

if (!require('lavaan')) install.packages('lavann'); library('lavaan')

Figure6.2.model <- ' 
                     CW =~  closeness.W
                     MW =~  commitment.W
                     SW =~  satisfaction.W

                     CH =~  closeness.H
                     MH =~  commitment.H
                     SH =~  satisfaction.H
'
 
fit <- sem(Figure6.2.model, data = table6.1_df)
summary(fit, standardized=TRUE)


Figure6.2.model.constrain.relationships <- ' 
                     CW =~  closeness.W
                     MW =~  commitment.W
                     SW =~  satisfaction.W

                     CH =~  closeness.H
                     MH =~  commitment.H
                     SH =~  satisfaction.H

                     CW ~~ a1*MW
                     CW ~~ a2*SW
                     SW ~~ a3*MW

                     CH ~~ a1*MH
                     CH ~~ a2*SH
                     SH ~~ a3*MH

                     CW ~~ b1*MH
                     CW ~~ b2*SH
                     SW ~~ b3*MH

                     CH ~~ b1*MW
                     CH ~~ b2*SW
                     SH ~~ b3*MW

'

fitConstrainedRelationships <- sem(Figure6.2.model.constrain.relationships, 
                               data = table6.1_df,  std.lv = TRUE)
summary(fitConstrainedRelationships, standardized=TRUE)
anova(fit, fitConstrainedRelationships)

# Add variance constraints 
Figure6.2.model.constrain.all <- ' 
                     CW =~  v1*closeness.W
                     MW =~  v2*commitment.W
                     SW =~  v3*satisfaction.W

                     CH =~  v1*closeness.H
                     MH =~  v2*commitment.H
                     SH =~  v3*satisfaction.H

                     CW ~~ a1*MW
                     CW ~~ a2*SW
                     SW ~~ a3*MW

                     CH ~~ a1*MH
                     CH ~~ a2*SH
                     SH ~~ a3*MH

                     CW ~~ b1*MH
                     CW ~~ b2*SH
                     SW ~~ b3*MH

                     CH ~~ b1*MW
                     CH ~~ b2*SW
                     SH ~~ b3*MW

'

fitConstrainedAll <- sem(Figure6.2.model.constrain.all, 
                               data = table6.1_df, std.lv = TRUE)
summary(fitConstrainedAll, standardized=TRUE)
anova(fit, fitConstrainedRelationships, fitConstrainedAll)


# Add reliability constraints 
Figure6.2.model.constrain.rel <- ' 
                     CW =~  v1*closeness.W
                     MW =~  v2*commitment.W
                     SW =~  v3*satisfaction.W

                     CH =~  v1*closeness.H
                     MH =~  v2*commitment.H
                     SH =~  v3*satisfaction.H

                     CW ~~ a1*MW
                     CW ~~ a2*SW
                     SW ~~ a3*MW

                     CH ~~ a1*MH
                     CH ~~ a2*SH
                     SH ~~ a3*MH

                     CW ~~ b1*MH
                     CW ~~ b2*SH
                     SW ~~ b3*MH

                     CH ~~ b1*MW
                     CH ~~ b2*SW
                     SH ~~ b3*MW

                     CW ~~ NA*CW
                     MW ~~ NA*MW
                     SW ~~ NA*SW

                     CH ~~ 1*CH
                     MH ~~ 1*MH
                     SH ~~ 1*SH
'

fitConstrainedRel <- sem(Figure6.2.model.constrain.rel, 
                               data = table6.1_df)
summary(fitConstrainedRel, standardized=TRUE)

Figure6.2.model.constrain.rel.null <- ' 
                     CW =~  v1*closeness.W
                     MW =~  v2*commitment.W
                     SW =~  v3*satisfaction.W

                     CH =~  v1*closeness.H
                     MH =~  v2*commitment.H
                     SH =~  v3*satisfaction.H

                     CW ~~ a1*MW
                     CW ~~ a2*SW
                     SW ~~ a3*MW

                     CH ~~ a1*MH
                     CH ~~ a2*SH
                     SH ~~ a3*MH

                     CW ~~ b1*MH
                     CW ~~ b2*SH
                     SW ~~ b3*MH

                     CH ~~ b1*MW
                     CH ~~ b2*SW
                     SH ~~ b3*MW

                     CW ~~ 1*CW
                     MW ~~ 1*MW
                     SW ~~ 1*SW

                     CH ~~ 1*CH
                     MH ~~ 1*MH
                     SH ~~ 1*SH
'

fitConstrainedRelNull <- sem(Figure6.2.model.constrain.rel.null, 
                               data = table6.1_df)

anova(fitConstrainedRel, fitConstrainedRelNull)
