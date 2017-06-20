install.packages("mlogit")
library("mlogit")
# source("~/Forge/mlogit/chargement.R")
data("Heating", package = "mlogit")

# check 
View(Heating)

# shape of the dataframe ="wide", each row is an observation.
# shape of the dataframe ="long", each row is an alternative.
# varying = column 3 ~ 12, installation cost and operation cost of 5 alternatives 
H <- mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
View(H)

# new model: add alternative-specific constants

# reflevel: the base alternative (the one for which the coefficients of individual-specific variables are normalized to 0)
mc <- mlogit(depvar~ic+oc, H, reflevel = 'hp')
summary(mc)

# apply function: MARGIN = 2, applies over columns
# apply function: FUN = mean, apply mean function to data
# fitted function: outcome=FALSE, predict probabilities for all the alternatives
apply(fitted(mc, outcome = FALSE), 2, mean)

# question a: how well do the estimated probabilities match the shares of customers choosing each alternative?

# answer:

# Results from summary(mc)
# Frequencies of alternatives:
#   hp       ec       er       gc       gr 
# 0.055556 0.071111 0.093333 0.636667 0.143333 
# 
# Results from prediction:
# Frequencies of alternatives:
# hp         ec         er         gc         gr 
# 0.05555556 0.07111111 0.09333333 0.63666667 0.14333333 

# They are the same. Adding alternative-specific constants does alter the choice probabilities
# Alternative-specific constants in a logit model also insure the averagae probabilities equal the observed shares

# question b: calculate Willingness to Pay (wtp) and discount rate r implied by the estimates 
wtp <- coef(mc)["oc"]/coef(mc)["ic"]
wtp
r <- 1/wtp
r

# answer: wtp = 4.563385, r = 0.2191356
# Willing to pay $4.56 on installation for $1 per year on operation costs. Implies discount rate at 22%

# alter the model: 
# model in question a: contains constants for all alternatives ec-er-gc-gr, with the constants for alternative hp normalized to zero
# new model: include constants for alternatives ec-er-gc-hp, with the constant for alternative gr normalized to zero
# question c: what would be the estimated coefficient of the constant for alternative gc? 

# answer: use estimated coefficients of model in question a:
# 1.71097930 (gc) - 0.30826328 (gr) = 1.4027160
# estimated coefficient of the constant for alternative gc is 1.4027160

# verify by altering the model:
update(mc, reflevel="gr")
# result
# 
# Call:
#   mlogit(formula = depvar ~ ic + oc, data = H, reflevel = "gr",     method = "nr", print.level = 0)
# 
# Coefficients:
#   ec:(intercept)  er:(intercept)  gc:(intercept)  hp:(intercept)              ic              oc  
# 1.3505827       1.5451737       1.4027160      -0.3082633      -0.0015332      -0.0069964  

