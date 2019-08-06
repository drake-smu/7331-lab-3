## @knitr libs
library(arules)
library(arulesViz)
# library(Hmisc)
library(dplyr)
library(plotly)
library(data.table)

## @knitr datadef
data("AdultUCI")
dim(AdultUCI)
AdultUCI

## @knitr cleanup
#Dropping fnlwgt, education number, capgain/loss
data <- AdultUCI[, -c(3,5,11:12)]

# colnames(data)[colnames(data)=="capital-gain"] <- "capgain"
# colnames(data)[colnames(data)=="capital-loss"] <- "caploss"
colnames(data)[colnames(data)=="hours-per-week"] <- "hoursperweek"

# 
# data$capgain <- with(data,impute(capgain,median))
# data$caploss <- with(data,impute(caploss,median))

#Breaking down numerical categories to bins
data$age <- cut(data$age, breaks = c(15,25,45,65,100), labels =c("Young", "Middleaged", "Senior", "Retired"))
data$hoursperweek <- cut(data$hoursperweek, breaks = c(0,20,40,60,80), labels =c("part-time", "full-time", "hard-working", "need-a-life") )

str(data)
##@knitr transact
#Change the dataset to transactional
data
data <- as(data, "transactions")
summary(data)
#Now view it as  as a dataframe
#as(data, "data.frame")

## @knitr rulefreq
#Quick check of rule frequencies
itemFrequencyPlot(data, support=.2)
## @knitr rulemine
#Now applying apriori for rule mining
zerules <- apriori(data, parameter = list(minlen=2, supp=0.2, conf = 0.3), appearance = list(rhs=c("income=small", "income=large"), default="lhs"),control = list(verbose=F)) 
length(zerules)

#remove redundants and sort by lift
redundant <- is.redundant(zerules)
zerules.pruned <- zerules[redundant == FALSE]
rulesorted <- sort(zerules.pruned, by="lift", decreasing = TRUE)
length(rulesorted)

## @knitr quality
(quality(rulesorted))
inspectDT(rulesorted)

## @knitr scatterplot
#Scatter Plot
plot(rulesorted, method = "scatterplot", measure = c("confidence", "support"), shading = "lift", engine = "htmlwidget")
## @knitr baloonplot
#Balloon plot
plot(rulesorted, method="graph", measure= "confidence", shading = "lift", engine = "htmlwidget")
## @knitr plplot
#Parallel plot
plot(rulesorted, method="paracoord", measure= "confidence", shading = "lift", control=list(reorder=T))
## @knitr kplot
#Two-key plot
plot(rulesorted, method="two-key plot", measure = 'confidence', shading='lift', engine = "htmlwidget")
## @knitr gplot
#grouped
plot(rulesorted, method="grouped", measure = 'confidence', shading='lift')



# For saving plot images
# image(Adult)
# plot(rules1)
# dev.copy(png,filename="rules1-a.png", width=500, height=500);
# dev.off ();
# 
# plot(rules1, measure=c("support","lift"), shading="confidence")
# dev.copy(png,filename="rules1-b.png", width=500, height=500);
# dev.off ();

# redux
## @knitr redux

rule2 <- apriori(data, parameter = list(supp=0.01, conf = 0.5), appearance = list(rhs=c("income=small", "income=large"), default="lhs"),control = list(verbose=F)) 
length(rule2)

#remove redundants and sort by lift
redundant <- is.redundant(rule2)
rulep <- rule2[redundant == FALSE]
rulesorted2 <- sort(rulep, by="lift", decreasing = TRUE)
length(rulesorted2)

## @knitr inspec
head(quality(rulesorted2))
inspectDT(rulesorted2)


## @knitr plot2
plot(rulesorted2, method = "scatterplot", measure = c("confidence","support"), shading = "lift", engine = "htmlwidget")
## @knitr bplo2
#Balloon plot
plot(rulesorted2, method="graph", measure= "confidence", shading = "lift", engine = "htmlwidget")
## @knitr pplo2
#Parallel plot
plot(rulesorted2, method="paracoord", measure= "confidence", shading = "lift", control=list(reorder=T))
#Two-key plot
## @knitr kplo2
plot(rulesorted2, method="two-key plot", measure = 'confidence', shading='lift', engine = "htmlwidget")
#grouped
## @knitr gplo2
plot(rulesorted2, method="grouped", measure = 'confidence', shading='lift')
