df <- read.csv("https://raw.githubusercontent.com/avi-kluger/RCompanion4DDABook/master/Chapter%2010/Chapter10_df.csv")

library(nlme) 

df$focalcode <- 1 - df$focalcode
df$partcode  <- 1 - df$partcode 

### overparamterized model
res1 <- lme(outcome ~ 0 + focalcode + partcode, random = ~ 0 + focalcode + partcode | focalid/dyadid, data = df)
summary(res1)

### contrain sigma to a very small value
res2 <- lme(outcome ~ 0 + focalcode + partcode, random = ~ 0 + focalcode + partcode | focalid/dyadid, data = df, control=list(sigma=1e-8, opt="optim"))
summary(res2)

# Just for fun, I also fitted the same model using 'metafor'. While it was not really made for analyzing raw data like this, it can be used to fit the same model (with the devel version) and then sigma can be constrained exactly to 0:

devtools::install_github("wviechtb/metafor")
library(metafor)

df$dyadid.in.focalid <- interaction(df$focalid, df$dyadid)
res3 <- rma.mv(outcome ~ 0 + focalcode + partcode, V=0, random = list(~ 0 + focalcode + partcode | focalid, ~ 0 + focalcode + partcode | dyadid.in.focalid), struct="GEN", data = df, sparse=TRUE)
res3

ci.rho <- confint(res3, rho=1, verbose=TRUE) 
ci.rho 
ci.phi <- confint(res3, phi=1, verbose=TRUE) 
ci.phi
