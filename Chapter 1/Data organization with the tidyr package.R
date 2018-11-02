################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
# ideas for tighter code were contributed by: Sarit Pery & Michal Lehmann
# advice for cleaner code: Nadav Kluger
#
#                              CHAPTER 1 -- Table 1.3
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Load the *tidyverse* packages
if (!require('tidyverse')) install.packages('tidyverse')
suppressMessages(library('tidyverse'))

# Read the data of the book
individual_df <- read.csv(text = "Dyad Person X Y Z
1 1 5 9 3
1 2 2 8 3
2 1 6 3 7
2 2 4 6 7
3 1 3 6 5
3 2 9 7 5", header = TRUE, sep = " ")
individual_df

# Reshape with *tidyr*, which is in *tidyverse*: https://uc-r.github.io/tidyr 

# 1. Reshape Individual df into Dyad df 
dyad_df <- gather(individual_df, variableNames, allScores, -Dyad, -Person) %>% 
           unite (questionPerson, variableNames, Person) %>% 
           spread(questionPerson, allScores)
dyad_df

# 2. Reshape Dyad df into Individual df 
Individual_recovered_df <- gather(dyad_df, questionPerson, allScores, -Dyad) %>% 
                           separate(questionPerson, 
                           c("variableNames", "Person"), convert = TRUE) %>%  
                           spread(variableNames, allScores)
Individual_recovered_df

# Test that recovered Individual df is identical to the original
all.equal(Individual_recovered_df, individual_df) 

# 3. Reshape Individual df into Pairwise df 
redundantColumns <- grep("Dyad|Person|Z", colnames(individual_df))
pairwise_df      <- bind_cols(individual_df,
                    with(individual_df, 
                    individual_df[order(Dyad, -(Person)), -redundantColumns]))
pairwise_df
