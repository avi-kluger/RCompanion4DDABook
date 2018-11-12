################################################################################
#   **************************** R companion for ************************** 
#
# Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. 
# New York: Guilford Press.
#
# written by Avi Kluger: avik@savion.huji.ac.il
#
#                              CHAPTER 1 -- Table 1.3
################################################################################
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

# 1. Reshape Individual df into Dyad df with *Base R* 
Individual_1 <- Individual[which(Individual$Person == 1), ]
Individual_2 <- Individual[which(Individual$Person == 2), ]

colnames(Individual_1) <- paste0(colnames(Individual_1), 1)
colnames(Individual_2) <- paste0(colnames(Individual_2), 2)

Dyad   <- cbind(Individual_1, Individual_2)
Dyad
remove <- c(grep("Person", colnames(Dyad)),
            grep("Dyad2" , colnames(Dyad)))
Dyad   <- Dyad[, -remove]
rownames(Dyad) <- NULL
Dyad

# 2. Reshape Dyad df into Individual df with  *Base R* 
Person_1              <- Dyad[, c(   grep(1, colnames(Dyad)))]
Person_2              <- Dyad[, c(1, grep(2, colnames(Dyad)))]

colnames(Person_2)    <- colnames(Person_1) <- c("Dyad", "X", "Y", "Z")
Person_1$Person       <- 1
Person_2$Person       <- 2

Individual_recovered  <- rbind (Person_1, Person_2)
Individual_recovered  <- Individual_recovered[, c("Dyad", "Person", 
                                                  "X", "Y", "Z")]
Individual_recovered  <- Individual_recovered[order(Individual_recovered$Dyad, 
                                            Individual_recovered$Person),]
row.names(Individual) <- row.names(Individual_recovered) <- NULL

Individual_recovered; Individual
Individual_recovered == Individual
all.equal(Individual_recovered, Individual)

# 3. Reshape Individual df into Pairwise df with *Base R*
temp1    <- Individual[c(TRUE, FALSE), ]
temp2    <- Individual[c(FALSE, TRUE), ]

Pairwise <- cbind(rbind(temp1, temp2), rbind(temp2, temp1))
Pairwise <- Pairwise[!duplicated(as.list(Pairwise))]
Pairwise <- Pairwise[order(Pairwise$Dyad), ]
Pairwise