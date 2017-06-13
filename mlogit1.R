install.packages("mlogit")
library("mlogit")
#source("~/Forge/mlogit/chargement.R")
data("Heating", package = "mlogit")

#check 
View(Heating)

#shape of the dataframe ="wide", each row is an observation.
#shape of the dataframe ="long", each row is an alternative.
#varying = column 3 ~ 12, installation cost and operation cost of 5 alternatives 
H <- mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
View(H)

#estimation by maximum likelihood of the multinomial logit model
#specific variables: installation cost and operation cost
m <- mlogit(depvar~ic+oc|0, H)

summary(m)