rm(list = ls())                               # Clean the Global Environment
cat ("\014")                                  # Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   # Clean Plots

individual <- read.csv(text = "Dyad Person X Y Z
1 1 5 9 3
1 2 2 8 3
2 1 6 3 7
2 2 4 6 7
3 1 3 6 5
3 2 9 7 5", header = TRUE, sep = " ")

# Transform Individual to Dyadic 

Dyadic      <- reshape(individual, idvar = "Dyad", timevar = "Person", 
                        direction = "wide", 
                        new.row.names = 1:(nrow(individual)/2))
Dyadic
# Transform Dyadic to Individual

individual2 <- reshape(Dyadic, idvar = "Dyad", timevar = "Person", 
                        direction = "long")
# Rename columns and rows; reorder
nameString                       <- unlist(strsplit(colnames(individual2), 
                                                    ".", fixed = TRUE))
colnames(individual2)            <- nameString[which(nameString != "1")]
rownames(individual2)            <- NULL
individual2                      <- individual2[order(individual2$Dyad, 
                                                      individual2$Person), ]
all.equal(individual, individual2, check.attributes = FALSE)
 
# Transform Individual to Pairwise

individReverse <- individual[order(individual$Dyad, -(individual$Person)),]
remove         <- c(grep("Dyad", colnames(individual)),
                  grep("Person", colnames(individual)))
Pairwise       <- cbind(individual,individReverse[, -remove])
rownames(Pairwise) <- NULL
Pairwise



