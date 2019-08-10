library(dplyr)
library(forcats)

#------Data Specific Cleaning------------

## @knitr dataclean
data <- read.csv("./data/adult-test.csv")

#Lets look at NA value's first.
NA_sum <- sort(sapply(data, function(x) sum(is.na(x))), decreasing = TRUE)
print(NA_sum)

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




#Bin Education into 3 bins.
data$education <- fct_collapse(data$education,
        "No Diploma" = c("1st-4th", "5th-6th","7th-8th","9th", "10th", "11th", "12th", "Preschool"),
        Associates = c("Assoc-acdm", "Assoc-voc"),
        Diploma = c("Some-college", "HS-grad")
    )
#Remap income bracket levels
data$income_bracket <- fct_collapse(data$income_bracket,
                             small = "<=50K.",
                             large = ">50K."
)

#remove ? and replace with other
levels(data$workclass)[levels(data$workclass)=="?"] <- "Other"
levels(data$occupation)[levels(data$occupation)=="?"] <- "Other-service"
levels(data$native_country)[levels(data$native_country)=="?"] <- "Other"

levels(data$workclass)
summary(data)
# levels(data$education)[levels(data$education)==" HS-grad"] <- "Diploma"

