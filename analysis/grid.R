source("setup.R")
transactions <- data

x <- 1:99
conf <- supp <- x/100
init <- data.frame(conf = conf, supp = supp)
params  <- expand.grid(init)


gsearch <- function(sup,con){
  rules <- apriori(transactions, parameter = list(minlen=2, supp=sup, conf = con), appearance = list(rhs=c("income_bracket=small", "income_bracket=large"), default="lhs"),control = list(verbose=F)) 
  redundant <- is.redundant(rules)
  rulePruned <- rules[redundant == FALSE]
  ruleLength  <- length(rules)
  return(c(
                    support = sup,
                    conf = con,
                    nrules = ruleLength
                    ))
}
library(parallel)
gs <- mcmapply(gsearch, params[[1]], params[[2]], mc.cores = 12L)
results <- data.frame(t(gs))

digestible <- results %>% 
  filter(nrules <=25 & nrules >=20) %>%  
  filter(conf > 0.35) %>%
  filter(support >0.35) %>%
  arrange(desc(nrules,support, conf))
nrow(digestible)
digestible
?apriori
thirtyrules <- results %>% filter(nrules == 30) %>%
  filter(conf > 0.1) %>% filter(support > 0.1)
thirtyrules
#    support conf nrules
# 1     0.30 0.11     30
# 2     0.30 0.12     30
# 3     0.30 0.13     30
# 4     0.30 0.14     30
# 5     0.30 0.15     30
# 6     0.30 0.16     30
# 7     0.30 0.17     30
# 8     0.30 0.18     30
# 9     0.30 0.19     30
# 10    0.30 0.20     30
# 11    0.30 0.21     30
# 12    0.30 0.22     30
# 13    0.30 0.23     30
# 14    0.30 0.24     30
# 15    0.30 0.25     30
# 16    0.30 0.26     30
# 17    0.30 0.27     30
# 18    0.30 0.28     30
# 19    0.30 0.29     30
# 20    0.30 0.30     30
# 21    0.30 0.31     30
# 22    0.30 0.32     30
# 23    0.30 0.33     30
# 24    0.30 0.34     30
# 25    0.30 0.35     30
# 26    0.30 0.36     30
# 27    0.30 0.37     30
# 28    0.30 0.38     30
# 29    0.30 0.39     30
# 30    0.30 0.40     30
# 31    0.30 0.41     30
# 32    0.30 0.42     30
# 33    0.30 0.43     30
# 34    0.30 0.44     30
# 35    0.30 0.45     30
# 36    0.30 0.46     30
# 37    0.30 0.47     30
# 38    0.30 0.48     30
# 39    0.30 0.49     30
# 40    0.30 0.50     30
# 41    0.30 0.51     30
# 42    0.30 0.52     30
# 43    0.30 0.53     30
# 44    0.30 0.54     30
# 45    0.30 0.55     30
# 46    0.30 0.56     30
# 47    0.30 0.57     30
# 48    0.30 0.58     30
# 49    0.30 0.59     30
# 50    0.30 0.60     30
# 51    0.30 0.61     30
# 52    0.30 0.62     30
# 53    0.30 0.63     30
# 54    0.30 0.64     30
# 55    0.30 0.65     30
# 56    0.30 0.66     30
# 57    0.30 0.67     30
# 58    0.23 0.79     30
# 59    0.22 0.80     30
# 60    0.20 0.82     30
# 61    0.17 0.86     30
