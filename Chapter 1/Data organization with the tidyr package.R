################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
# ideas for tighter code were contributed by: 
#        Sarit Peri
#        Michal Lehmann
#
#                              CHAPTER 1 -- Table 1.3
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Load the *tidyverse* packages: https://uc-r.github.io/tidyr
if (!require('tidyverse')) install.packages('tidyverse')
supp(library('tidyverse'))

# Read the data of the book
Individual <- read.csv(text = "Dyad Person X Y Z
1 1 5 9 3
1 2 2 8 3
2 1 6 3 7
2 2 4 6 7
3 1 3 6 5
3 2 9 7 5", header = TRUE, sep = " ")
Individual

# Reshape with *tidyr*, which is in *tidyverse* 

# 1. Reshape Individual df into Dyad df 
Dyad <- gather(Individual, variableNames, allScores, -Dyad, -Person) %>% 
        unite (questionPerson, variableNames, Person) %>% 
        spread(questionPerson, allScores)
Dyad

# 2. Reshape Dyad df into Individual df 
Individual_recovered <- gather(Dyad, questionPerson, allScores, -Dyad) %>% 
                        separate(questionPerson, c("variableNames", "Person"), 
                                 convert = TRUE) %>%  
                        spread(variableNames, allScores)
Individual_recovered

# Test that recovered Individual df is identical to the original
all.equal(Individual_recovered, Individual) 

# 3. Reshape Individual df into Pairwise df 
redundant   <- grep("Dyad|Person|Z", colnames(Individual))
Pairwise    <- bind_cols(Individual,
               with(Individual, Individual[order(Dyad, -(Person)), -redundant]))
Pairwise
