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

# Question: Estimate a model of which "Willingness to Pay" (wtp) equals 8.33,
# test the hypothesis that discount rate r equals 0.12

# Answer: Lifecycle cost (LCC) = Installation cost (IC) + Opeartion Cost Over time (OC/r),
# in this case LCC = ic + oc/0.12
H$lcc=H$ic+H$oc/0.12

# estimation by maximum likelihood of the multinomial logit model
# specific variables: lifecycle cost
mlcc <- mlogit(depvar~lcc|0, H)

# For comparison, run estimation model 1 again 
# estimation by maximum likelihood of the multinomial logit model
# specific variables: installation cost and operation cost
m <- mlogit(depvar~ic+oc|0, H)

#Likelihood ratio test
lrtest(m, mlcc)

# Result from likelihood ratio test:
# model 1, lnL = -1095.2
# model 2, lnL = -1248.7
# 2(1248.7 - 1095.2) = 307
# 
# Critical value of chi-squared with 1 degree of freedom, 
# since testing on one restriction in this case 
# 95% confidence level, lower.tail = FALSE testing probability P[X>x]
qchisq(0.05, df = 1, lower.tail = FALSE)

# Critical value 3.841459
# 307>3.841459

# Reject hypothesis that r=0.12
