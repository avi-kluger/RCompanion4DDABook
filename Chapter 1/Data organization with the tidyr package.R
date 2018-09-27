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

# Reshape with *tidyr* from the *tidyverse* packages
# https://uc-r.github.io/tidyr
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

# 1. Reshape Individual df into Dyad df with *tidyr*
Dyad <- gather(Individual, variable, value, X, Y, Z) %>%
             unite(var, variable, Person) %>% 
             spread(var, value)
Dyad

# 2. Reshape Dyad df into Individual df with *tidyr* package
Individual_recovered <- Dyad %>% gather(var, score, X_1:Z_2) %>% 
                           separate(var, c("var", "Person")) %>%  
                           spread(var, score)
Individual_recovered

# Test that recovered Individual df is identical to the original
all.equal(Individual_recovered, Individual) 
Individual_recovered$Person <- as.numeric(Individual_recovered$Person)
all.equal(Individual_recovered, Individual) 

# 3. Reshape Individual df into Pairwise df with *Base R*
var4pairwise       <- c("X", "Y", "Z")
temp1              <- Individual[c(TRUE, FALSE), var4pairwise]
temp2              <- Individual[c(FALSE, TRUE), var4pairwise]
Pairwise           <- cbind(rbind(temp1, temp2), rbind(temp2, temp1))
Pairwise
colnames(Pairwise) <- paste0(colnames(Pairwise),
                                      rep(1:2, each = (length(Pairwise))/2))
Pairwise           <- cbind(Individual[, c("Dyad", "Person")], Pairwise)
Pairwise