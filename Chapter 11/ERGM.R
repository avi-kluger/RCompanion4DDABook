################################################################################
#    **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 11 -- Social Network Analysis
# Four-Person Model
################################################################################
rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

if (!require('statnet')) install.packages('statnet'); library('statnet')

data(florentine) # loads flomarriage and flobusiness data
flomarriage

# ReplicatE Table 11.1
sampson_df            <- matrix(0, nrow = 18, ncol = 18)
row.names(sampson_df) <- colnames(sampson_df) <- LETTERS[1:18]
      sampson_df["A", c("C", "L", "N")] <-  1
      sampson_df["B", c("A", "G", "L")] <-  1
      sampson_df["C", c("A", "M", "Q", "R")] <-  1
      sampson_df["D", c("E", "F", "K")] <-  1
      sampson_df["E", c("D", "I", "K")] <-  1
      sampson_df["F", c("D", "E", "I")] <-  1
      sampson_df["G", c("B", "L", "P")] <-  1
      sampson_df["H", c("D", "F", "I")] <-  1
      sampson_df["I", c("E", "H", "L")] <-  1
      sampson_df["J", c("D", "E", "I", "M")] <-  1
      sampson_df["K", c("E", "H", "N")] <-  1
      sampson_df["L", c("A", "B", "G")] <-  1
      sampson_df["M", c("E", "G", "R")] <-  1
      sampson_df["N", c("A", "L", "O")] <-  1
      sampson_df["O", c("B", "G", "L")] <-  1
      sampson_df["P", c("B", "G", "O")] <-  1
      sampson_df["Q", c("B", "C", "R")] <-  1
      sampson_df["R", c("B", "C", "Q")] <-  1
sampson_df <- as.data.frame(sampson_df)


net <- as.network(x = sampson_df, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)


network.vertex.names(net) <- LETTERS[1:18]

summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)

plot.network(net)

sampson <- ergm(net~edges) # fit model
summary(sampson) # look in more depth


sampson1 <- ergm(net~edges+mutual)
summary(sampson1) # look in more depth
gof(sampson1)
round(sampson1$"est.cov", 4)
data(samplk)
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)
