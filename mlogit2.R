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

# estimation by maximum likelihood of the multinomial logit model
# specific variables: installation cost and operation cost
m <- mlogit(depvar~ic+oc|0, H)

summary(m)

# Question A: Do the estimated coefficients have the expected signs?
# Answer: Yes, result is a negative log-likelihood (-1095.2), indicates when a system price rises, its chance of being installed falls.
# 
# Question B: Are both coefficients significantly different from zero?
# Answer: Yes, t-value great than 1.96 (95% confidence level)
# 
# apply function: MARGIN = 2, applies over columns
# apply function: FUN = mean, apply mean function to data
# fitted function: outcome=FALSE, predict probabilities for all the alternatives
apply(fitted(m, outcome=FALSE), 2, mean)


# Question C: How closely do the average probabilities match the shares of customers choosing each alternative?
# Answer: 
# Results from summary(m)
# Frequencies of alternatives:
#       ec       er       gc       gr       hp 
# 0.071111 0.093333 0.636667 0.143333 0.055556 
# 
# Results from prediction:
# Frequencies of alternatives:
#         ec         er         gc         gr         hp 
# 0.10413057 0.05141477 0.51695653 0.24030898 0.08718915 

# Question D:The ratio of coefficients usually provides economically meaningful information. The
# willingness to pay (wtp) through higher installation cost for a one-dollar reduction in
# operating costs is the ratio of the operating cost coefficient to the installation cost coefficient.
# What is the estimated wtp from this model? Is it reasonable in magnitude?

coef(m)["oc"]/coef(m)["ic"]

# Answer:
# oc 
# 0.7349453 

