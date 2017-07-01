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

# new model: add sociodemographic variables

# question a: replace installation cost variable in the old model with installation cost divided by income
# run the new model and see how estimation of coefficient changes
mi <- mlogit(depvar~oc+I(ic/income), H)
summary(mi)