library(dplyr)
library(forcats)
library(pander)
library(ggplot2)
library(cowplot)

## @knitr dataimport
data <- read.csv("./data/adult-training.csv")


#------Data Specific Cleaning------------

## @knitr cleanup
#Dropping fnlwgt, education number, capgain/loss
data <- data[, -c(3,5,11:12)]

#Breaking down numerical categories to bins
data$age <- cut(data$age, breaks = c(15,25,45,65,100), labels =c("Young", "Middleaged", "Senior", "Retired"))
data$hours_per_week <- cut(data$hours_per_week, breaks = c(0,20,40,60,80), labels =c("part-time", "full-time", "hard-working", "need-a-life") )
str(data)

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
  labs(x = "Age", y = "Density", title = "Age Density by Income",
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

#hours per week
p5 <- ggplot(data, aes(x = hours_per_week, color = income_bracket)) +
  labs(x = "Hours per week", title = "Hours per week by Income",
       subtitle = "Density plot")

#Occupation with Education
p6 <- ggplot(data, aes(occupation)) +
  geom_bar(aes(fill=education), width = 0.5) +
  theme(axis.text.x = element_text(angle=60, vjust=0.5)) +
  labs(title="Histogram of occupation with education binning",
       subtitle="Occupation and Educational")

p1
p2
p3
p4
p5
p6