library(arules)
library(arulesViz)
# library(Hmisc)
library(dplyr)

data("AdultUCI")
dim(AdultUCI)

#Dropping fnlwgt, education number, capgain/loss
data <- AdultUCI[, -c(3,5,11:12)]



# colnames(data)[colnames(data)=="capital-gain"] <- "capgain"
# colnames(data)[colnames(data)=="capital-loss"] <- "caploss"
colnames(data)[colnames(data)=="hours-per-week"] <- "hoursperweek"

# 
# data$capgain <- with(data,impute(capgain,median))
# data$caploss <- with(data,impute(caploss,median))

data$age <- cut(data$age, breaks = c(15,25,45,65,100), labels =c("Young", "Middleaged", "Senior", "Retired"))
data$hoursperweek <- cut(data$hoursperweek, breaks = c(0,20,40,60,80), labels =c("part-time", "full-time", "hard-working", "need-a-life") )

str(data)

data <- as(data, "transactions")

summary(data)
as(data, "data.frame")

itemFrequencyPlot(data, support=.2)

zerules <- apriori(data, parameter = list(minlen=2, supp=0.2, conf = 0.3), appearance = list(rhs=c("income=small", "income=large"), default="lhs"),control = list(verbose=F))


redundant <- is.redundant(zerules)
zerules.pruned <- zerules[redundant == FALSE]
rulesorted <- sort(zerules.pruned, by="lift", decreasing = TRUE)
head(quality(rulesorted))
inspect(rulesorted)

#Scatter Plot
plot(zerules.pruned, method = "scatterplot", measure = "confidence", shading = "lift")

#Balloon plot
plot(zerules.pruned, method="graph", control=list(type="items"))

#Parallel plot
plot(zerules.pruned, method="paracoord", control=list(reorder=T))





# For saving plot images
# image(Adult)
# plot(rules1)
# dev.copy(png,filename="rules1-a.png", width=500, height=500);
# dev.off ();
# 
# plot(rules1, measure=c("support","lift"), shading="confidence")
# dev.copy(png,filename="rules1-b.png", width=500, height=500);
# dev.off ();
