################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 7 -- APIM
# TEST APIM with SEM
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

Table8.9 <-  "26   6766478665
22 85  74766584
17 5375  345345
19 549564  9473
25 65746586  54
22 4265546483  "

Table8.9_df <- read.fwf(textConnection(Table8.9), widths = c(3, rep(1, 12)))
colnames(Table8.9_df) <- c("Age", paste(c("liking", "smiling"), 
                          rep(LETTERS[1:6], each = 2), sep = "_"))

Table8.9_df$id <- as.numeric(rownames(Table8.9_df))

if (!require("TripleR")) install.packages("TripleR"); 
suppressPackageStartupMessages(library(TripleR))

liking_long_df   <- matrix2long(Table8.9_df[, grep("liking", 
                                                 colnames(Table8.9_df))])
smiling_long_df  <- matrix2long(Table8.9_df[, grep("smiling", 
                                                 colnames(Table8.9_df))])
Table8.9_long_df <- cbind(liking_long_df, smiling_long_df[, 3])

colnames(Table8.9_long_df)[3:4] <- c("smiling", "liking")

fitTable8.9 <- RR(liking + smiling ~ actor.id*partner.id, 
                  data = Table8.9_long_df)
fitTable8.9

# Prepare data for correlation with age
if (!require("tidyverse")) install.packages("tidyverse"); 
suppressPackageStartupMessages(library(tidyverse))

srmEffectsWithIndividualDifferences <- 
          cbind(fitTable8.9$univariate[[1]]$effects,
                    fitTable8.9$univariate[[2]]$effects)
srmEffectsWithIndividualDifferences$id <- as.numeric(
                                         srmEffectsWithIndividualDifferences$id)
srmEffectsWithIndividualDifferences <- 
          cbind(srmEffectsWithIndividualDifferences,
          Table8.9_df[, c("Age", "id")])

junk <- grep("id", colnames(srmEffectsWithIndividualDifferences))  

srmEffectsWithIndividualDifferences <- 
                          srmEffectsWithIndividualDifferences[, -junk]
colnames(srmEffectsWithIndividualDifferences) [5] <- "Age"
round(cor(srmEffectsWithIndividualDifferences$Age,
      srmEffectsWithIndividualDifferences[, -5]), 2)

                