library(dplyr)
library(forcats)

#------Data Specific Cleaning------------

df <- read.csv("./data/adult-test.csv")

#Lets look at NA value's first.
NA_sum <- sort(sapply(df, function(x) sum(is.na(x))), decreasing = TRUE)
print(NA_sum)

#remove whitespace from education
levels(df$education) <- trimws(levels(df$education))
levels(df$workclass) <- trimws(levels(df$workclass))
levels(df$marital_status) <- trimws(levels(df$marital_status))
levels(df$occupation) <- trimws(levels(df$occupation))
levels(df$relationship) <- trimws(levels(df$relationship))
levels(df$race) <- trimws(levels(df$race))
levels(df$gender) <- trimws(levels(df$gender))
levels(df$native_country) <- trimws(levels(df$native_country))
levels(df$income_bracket) <- trimws(levels(df$income_bracket))




#Bin Education into 3 bins.
df$education <- fct_collapse(df$education,
        "No Diploma" = c("1st-4th", "5th-6th","7th-8th","9th", "10th", "11th", "12th", "Preschool"),
        Associates = c("Assoc-acdm", "Assoc-voc"),
        Diploma = c("Some-college", "HS-grad")
    )
#Remap income bracket levels
df$income_bracket <- fct_collapse(df$income_bracket,
                             small = "<=50K.",
                             large = ">50K."
)

#remove ? and replace with other
levels(df$workclass)[levels(df$workclass)=="?"] <- "Other"
levels(df$occupation)[levels(df$occupation)=="?"] <- "Other-service"
levels(df$native_country)[levels(df$native_country)=="?"] <- "Other"

levels(df$workclass)
summary(df)
# levels(df$education)[levels(df$education)==" HS-grad"] <- "Diploma"

