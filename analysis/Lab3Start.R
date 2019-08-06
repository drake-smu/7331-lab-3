library(arules)
library(arulesViz)
library(Hmisc)
library(dplyr)

data("AdultUCI")
dim(AdultUCI)

#Dropping fnlwgt, education number, capgain/loss
data <- AdultUCI[, -c(3,5, 11:12)]



# colnames(data)[colnames(data)=="capital-gain"] <- "capgain"
# colnames(data)[colnames(data)=="capital-loss"] <- "caploss"

colnames(data)[colnames(data)=="hours-per-week"] <- "hoursperweek"

# 
# data$capgain <- with(data,impute(capgain,median))
# data$caploss <- with(data,impute(caploss,median))

data$age <- cut(data$age, breaks = c(0,15,30,45,65,90), labels =c("Young", "YoungAdult", "MiddleAge", "Senior", "Retired") )
data$hoursperweek <- cut(data$hoursperweek, breaks = c(0,20,40,60,80), labels =c("part-time", "full-time", "hard-working", "need-a-life") )

str(data)

zerules <- apriori(data, parameter = list(minlen=2, supp=0.1, conf = 0.6), appearance = list(rhs=c("income=small", "income=large"), default="lhs"),control = list(verbose=F))
inspect(zerules)