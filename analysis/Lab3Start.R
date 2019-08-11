## @knitr setup-03
source('scripts/load-packages.R')
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(dev = "tikz")
}
knitr::opts_chunk$set(warning = F)
# knitr::opts_knit$set(root.dir = "..")
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(tidy = T, tidy.opts = list(comment = F))
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(comment = '#>')
knitr::opts_chunk$set(fig.path = 'fig/')
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## @knitr datadef
dim(data)
data

## @knitr transact
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
zerules <- apriori(data, parameter = list(minlen=2, supp=0.2, conf = 0.15), appearance = list(rhs=c("income_bracket=small", "income_bracket=large"), default="lhs"),control = list(verbose=F)) 
length(zerules)

#remove redundants and sort by lift
redundant <- is.redundant(zerules)
zerules.pruned <- zerules[redundant == FALSE]
rulesorted <- sort(zerules.pruned, by="lift", decreasing = TRUE)
length(rulesorted)

## @knitr quality
(quality(rulesorted))
inspectDT(rulesorted, caption="Association Rules")

## @knitr scatterplot
#Scatter Plot
plot(rulesorted, method = "scatterplot", measure = c("confidence", "support"), 
     shading = "lift", engine = "htmlwidget")
## @knitr baloonplot
#Balloon plot
plot(rulesorted, method="graph", measure= "confidence", shading = "lift",
     engine = "htmlwidget")
## @knitr plplot
#Parallel plot
plot(rulesorted, method="paracoord", measure= "confidence", shading = "lift", 
     control=list(reorder=T))
## @knitr kplot
#Two-key plot
plot(rulesorted, method="two-key plot", measure = 'confidence', shading='lift',
     engine = "htmlwidget")
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

rule2 <- apriori(data, 
                 parameter = list(minlen=2, supp=0.01, conf = 0.6), 
                 appearance = list(rhs=c("income_bracket=small", "income_bracket=large"), default="lhs"),
                 control = list(verbose=F)) 
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

## @knitr small_income_rules
#Targeting small income
rule_small <- apriori(data, 
                 parameter = list(minlen=2, supp=0.12, conf = 0.95), 
                 appearance = list(rhs=c("income_bracket=small"), default="lhs"),
                 control = list(verbose=F)) 
length(rule_small)

#remove redundants and sort by lift
redundant <- is.redundant(rule_small)
rulep <- rule_small[redundant == FALSE]
rulesorted_small <- sort(rulep, by="lift", decreasing = TRUE)
length(rulesorted_small)

## @knitr small_inspec
head(quality(rulesorted_small))
inspectDT(rulesorted_small)

## @knitr small_plotscat
plot(rulesorted_small, method = "scatterplot", measure = c("confidence","support"), shading = "lift", engine = "htmlwidget")



## @knitr large_income_rules
#Targeting large income
rule_large <- apriori(data, 
                      parameter = list(minlen=2, supp=0.08, conf = 0.5), 
                      appearance = list(rhs=c("income_bracket=large"), default="lhs"),
                      control = list(verbose=F)) 
length(rule_large)

#remove redundants and sort by lift
redundant <- is.redundant(rule_large)
rulep <- rule_large[redundant == FALSE]
rulesorted_large <- sort(rulep, by="lift", decreasing = TRUE)
length(rulesorted_large)

## @knitr large_inspec
head(quality(rulesorted_large))
inspectDT(rulesorted_large)

## @knitr large_plotscat
plot(rulesorted_large, method = "scatterplot", measure = c("confidence","support"), shading = "lift", engine = "htmlwidget")
