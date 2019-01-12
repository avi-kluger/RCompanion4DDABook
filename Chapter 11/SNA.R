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

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

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

sampson_df$from <- rownames(sampson_df)
edges           <- sampson_df %>%  gather(to, weight, A:R, -from)
# Create a data frame containing only edges (arrows)
edges           <- edges[which(edges$weight == 1), ]

detach(package:tidyverse)
if (!require('igraph')) install.packages('igraph'); library('igraph')

#Create a graph labeled g
g              <- graph_from_data_frame(edges, directed = TRUE)
as_adjacency_matrix(g)
plot(g)

# Code below is based on a workshop of Katherine Ognyanova, www.kateto.net

# Node degrees
# 'degree' has a mode of 'in' for in-degree, 'out' for out-degree,
# and 'all' or 'total' for total degree. 
plot(g, vertex.size = degree(g, mode="in")*3)
plot(g, vertex.size = degree(g, mode="out")*5)
plot(g, vertex.size = degree(g)*3)


# Degree (number of ties)
degree_df               <- as.data.frame(degree(g, mode="in"))
colnames(degree_df)     <- "Indegree"
degree_df$Monk          <- rownames(degree_df)
degree_df               <- degree_df[order(degree_df$Monk), ]
degree_df               <- degree_df [, c("Monk", "Indegree")] 
Table11.2_df            <- degree_df

closeness_df            <- as.data.frame(closeness(g,  mode="all"))
colnames(closeness_df)  <- "Closeness"
closeness_df$Monk       <- rownames(closeness_df)
closeness_df            <- closeness_df[order(closeness_df$Monk), ]
closeness_df            <- closeness_df [, c("Monk", "Closeness")] 
Table11.2_df$Closeness  <- round(closeness_df$Closeness, 3)
Table11.2_df

betweenness_df          <- as.data.frame(betweenness(g, directed=T, weights=NA))
colnames(betweenness_df)<- "Betweenness"
betweenness_df$Monk     <- rownames(betweenness_df)
betweenness_df          <- betweenness_df[order(betweenness_df$Monk), ]
betweenness_df          <- betweenness_df [, c("Monk", "Betweenness")] 
Table11.2_df$betweenness<- round(betweenness_df$Betweenness, 3)
Table11.2_df
round(cor(Table11.2_df[, 2:4]), 3)

# Reciprocity cf. p.304
# The proportion of reciprocated ties (for a directed network).
reciprocity(g)
dyad_census(g) # Mutual, asymmetric, and null node pairs
2*dyad_census(g)$mut/ecount(g) # Calculating reciprocity

# Transitivity cf. page 305
# global - ratio of triangles (direction disregarded) to connected triples
transitivity(g, type="global")  # g is treated as an undirected network
triad_census(g) # for directed networks
cat("Triad census was defined by David and Leinhardt (see References below). 
Every triple of vertices (A, B, C) are classified into the 16 possible states:
003   A,B,C, the empty graph.
012   A->B, C, the graph with a single directed edge.
102   A<->B, C, the graph with a mutual connection between two vertices.
021D  A<-B->C, the out-star.
021U  A->B<-C, the in-star.
021C  A->B->C, directed line.
111D  A<->B<-C.
111U  A<->B->C.
030T  A->B<-C, A->C.
030C  A<-B<-C, A->C.
201   A<->B<->C.
120D  A<-B->C, A<->C.
120U  A->B<-C, A<->C.
120C  A->B->C, A<->C.
210   A->B<->C, A<->C.
300   A<->B<->C, A<->C, the complete graph.")

cliques(g, min= 3)
# Density
# The proportion of present edges from all possible ties.
edge_density(g, loops=F)


if (!require('IsingFit')) install.packages('IsingFit'); library('IsingFit')
if (!require("qgraph")) install.packages("qgraph"); library("qgraph")

sampsonCorNoNA <- cor(sampson_df[, 1:18], method = "Spearman")
sampsonCorNoNA ["J", ] <- 0
sampsonCorNoNA [, "J"] <- 0
StateWeiAdj <- EBICglasso(sampsonCorNoNA, 
                          nrow(na.omit(sampson_df[, 1:18])))

StateGraph2  <- qgraph(edges, layout = "spring")

StateiGraph2 <- graph_from_adjacency_matrix(abs(StateWeiAdj),'undirected', weighted = TRUE, add.colnames = FALSE)
StateCom <- cluster_walktrap(sampson)
communities (StateCom)

qgraph(StateWeiAdj, layout = "spring",
       cut = .8, groups = communities(StateCom),
       legend = FALSE, label.norm =  "OOO", labels = colnames(StateWeiAdj))

StateCen <- centralityTable(StateiGraph2,
                            standardized = FALSE)
centralityPlot(StateiGraph2, scale = 'raw')



detach(package:tidyverse)
if (!require('statnet')) install.packages('statnet'); library('statnet')
if (!require('sna')) install.packages('sna'); library('sna')
library(statnet)


# Create a network object out of the adjacency matrix
#
sampson <- network(sampson_df[, 1:18],directed=TRUE)
sampson
#
# print out the sociomatrix for the Florentine marriage data
#
sampson[,]

# create a plot of the social network
#
plot(sampson)
#
#
# Use 'data(package = "ergm")' to list the data sets in a
#
#
# Load a network object of the Florentine data
#
data(sampson)
#
# Fit a model where the propensity to form ties between
# families depends on the absolute difference in wealth
#
gest <- ergm(sampson ~ edges)
summary(gest)   

gest <- ergm(sampson~edges+mutual)
summary(gest)   
mcmc.diagnostics(gest)



#
# add terms for the propensity to form 2-stars and triangles
# of families 
#
gest <- ergm(sampson ~ kstar(1:2)  + triangle, directed==TRUE)
summary(gest)

# import synthetic network that looks like a molecule
data(molecule)
# Add a attribute to it to mimic the atomic type
molecule %v% "atomic type" <- c(1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3)
#
# create a plot of the social network
# colored by atomic type
#
plot(molecule, vertex.col="atomic type",vertex.cex=3)

# measure tendency to match within each atomic type
gest <- ergm(molecule ~ edges + kstar(2) + triangle + nodematch("atomic type"),
control=control.ergm(MCMC.samplesize=10000))
summary(gest)

# compare it to differential homophily by atomic type
gest <- ergm(molecule ~ edges + kstar(2) + triangle
                        + nodematch("atomic type",diff=TRUE),
control=control.ergm(MCMC.samplesize=10000))
summary(gest)


# Extract parameter estimates as a numeric vector:
coef(gest)


# Sources of variation in parameter estimates:
vcov(gest, sources="model")
vcov(gest, sources="estimation")
vcov(gest, sources="all") # the default

