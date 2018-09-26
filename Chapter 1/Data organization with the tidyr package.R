################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 1 -- Table 1.3
###############################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

Individual <- read.csv(text = "Dyad Person X Y Z
1 1 5 9 3
1 2 2 8 3
2 1 6 3 7
2 2 4 6 7
3 1 3 6 5
3 2 9 7 5", header = TRUE, sep = " ")
Individual

# Reshape Individual df into Dyad df with *tidyverse* packages
# https://uc-r.github.io/tidyr
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

DyadDF <- gather(Individual, variable, value, X, Y, Z) %>%
             unite(var, variable, Person) %>% 
             spread(var, value)
DyadDF

Individual_recovered <- DyadDF %>% gather(var, score, X_1:Z_2) %>% 
                           separate(var, c("var", "Person")) %>%  
                           spread(var, score)
  
all.equal(Individual_recovered, Individual) 
Individual_recovered$Person <- as.numeric(Individual_recovered$Person)
all.equal(Individual_recovered, Individual) 


