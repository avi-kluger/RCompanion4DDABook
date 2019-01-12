df <- read.csv("Chapter10_df.csv")

head(Chapter10_df) 

if (!require("lme4")) install.packages("lme4"); suppressMessages(library(lme4))

mlm <- lmer(outcome   ~ 0 + focalcode + partcode + role +
                       (0 + focalcode + partcode|| focalid/ dyadid),
                       data = Chapter10_df)
summary(mlm)