################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
# ideas for tighter code were contributed by: Sarit Pery & Michal Lehmann
#
#                              CHAPTER 1 -- Table 1.3
#                                   March 2021
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

# Individual data = long format, assign to l
# Dyadic data     = wide format, assign to w
# Pairwise data, assign to p

# Load the *tidyverse* packages
if (!require('tidyverse')) install.packages('tidyverse')

# Read the data of the book
l <- read.csv(text = "Dyad Person X Y Z
1 1 5 9 3
1 2 2 8 3
2 1 6 3 7
2 2 4 6 7
3 1 3 6 5
3 2 9 7 5", header = TRUE, sep = " ")
l

# Pivot with *tidyr*, which is in *tidyverse*: 
# https://tidyr.tidyverse.org/reference/pivot_wider.html#examples

# 1. Pivot individual (long) data into dyadic (wide) data
w <- l %>%  
  pivot_wider(
    names_from  = Person, 
    values_from = X:Z
  )
w 

# 2. Pivot dyadic (wide) data into individual (long) data 
lRecovered <- w %>% 
  pivot_longer(                           # Step 1, place all numbers in value 
    cols = !Dyad, 
    names_to = c("var", "Person"),
    names_sep = "_",
  ) %>%  
  pivot_wider(                            # Step 2, use var to create columns 
    names_from  = var, 
    values_from = value
  )
lRecovered

  # Test that recovered long format is identical to the original
  all.equal(lRecovered, l) 
  
  str(l)
  str(lRecovered)  # Note that the pivoting changed Person into a character
  
  # Coerce Person into integer
  lRecovered$Person <- as.integer(lRecovered$Person)
  
  all.equal(lRecovered, l) # Note that differences are in attributes and formats
  
  # Coerce the tibbles into dataframes, stripping their attributes and formats
  all.equal(as.data.frame(lRecovered), as.data.frame(l)) 

# 3. Reshape Individual df into Pairwise df 
partner          <- l %>% arrange(Dyad, -Person) %>% select(-c(Dyad, Person, Z))

library (magrittr) # https://blog.rstudio.com/2014/12/01/magrittr-1-5/
names(partner) %<>% paste0(2)
names(l)[3:4]  %<>% paste0(1)
p                <- bind_cols(l, partner)
p
