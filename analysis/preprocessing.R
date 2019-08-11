library(dplyr)
library(forcats)
library(pander)
library(ggplot2)
library(cowplot)
#------Data Specific Cleaning------------

## @knitr dataimport
data <- read.csv("./data/adult-training.csv")

## @knitr dataqual
#Lets look at NA value's first.
NA_sum <- sort(sapply(data, function(x) sum(is.na(x))), decreasing = TRUE)
data.frame((NA_sum))

## @knitr factorclean
# Get the names of the factor columns
GetFactors <- function(df){
  return(
         names(Filter(is.factor,df))
  )
}

# Remove whitespace from factor levels
FixLevels <- function(x){
  levels(x) <- trimws(levels(x))
  return(x)
}

# Remove whitespace from all factors in the data frame
data[GetFactors(data)]  <- lapply(data[GetFactors(data)], FixLevels)
pander(lapply(data[GetFactors(data)], levels))


## @knitr edubin
#Bin Education into 3 bins.
data$education <- fct_collapse(data$education,
        "No Diploma" = c("1st-4th", "5th-6th","7th-8th","9th", "10th", "11th", "12th", "Preschool"),
        Associates = c("Assoc-acdm", "Assoc-voc"),
        Diploma = c("Some-college", "HS-grad")
    )
#Remap income bracket levels

## @knitr incbrack
data$income_bracket <- fct_collapse(data$income_bracket,
                             small = "<=50K",
                             large = ">50K"
)

## @knitr questionmark
#remove ? and replace with other
levels(data$workclass)[levels(data$workclass)=="?"] <- "Other"
levels(data$occupation)[levels(data$occupation)=="?"] <- "Other-service"
levels(data$native_country)[levels(data$native_country)=="?"] <- "Other"

## @knitr preprocessres
levels(data$workclass)
pander(summary(data))

## @knitr EDAGraphs
#Looking at Age
p1 <- ggplot(data, aes(x = age, color = income_bracket, fill = income_bracket)) + 
  geom_density(alpha = 0.9) +
  labs(x = "Age", y = "Density", title = "The older you get, the more you make",
       subtitle = "Density plot")

#Looking at education
p2 <- ggplot(data, aes(x = education, fill = income_bracket, color = income_bracket)) +
  geom_bar(alpha = 0.9, position = "fill") +
  coord_flip() +
  labs(x = "Education", y = "Proportion", title = "Income bias based on Education",
       subtitle = "Stacked bar plot")

#Marital Status
p3 <- ggplot(data, aes(x = marital_status, fill = income_bracket, color = income_bracket)) +
  geom_bar(alpha = 0.9, position = "fill") +
  coord_flip() +
  labs(x = "Marital Status", y = "Proportion", title = "Income bias based on Marital status",
       subtitle = "Stacked bar plot")

#Occupation
p4 <- ggplot(data, aes(x = occupation, fill = income_bracket, color = income_bracket)) +
  geom_bar(alpha = 0.9, position = "fill") +
  coord_flip() +
  labs(x = "Occupation Status", y = "Proportion", title = "Income bias based on Occupation status",
       subtitle = "Stacked bar plot")


# 
# #Occupation
# p4 <- ggplot(data, aes(education)) + 
#   geom_bar(aes(fill=income_bracket), width = 0.5) + 
#   theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
#   labs(title="Histogram on Occupation", 
#        subtitle="Occupation vs income")

p1
p2
p3
p4
