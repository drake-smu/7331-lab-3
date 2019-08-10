library(dplyr)
library(forcats)

#------Data Specific Cleaning------------

df <- read.csv("./data/adult-test.csv")

#Lets look at NA value's first.
NA_sum <- sort(sapply(df, function(x) sum(is.na(x))), decreasing = TRUE)
print(NA_sum)

#remove whitespace from education
levels(df$education) <- trimws(levels(df$education))


df$education <- fct_collapse(df$education,
        "No Diploma" = c("1st-4th", "5th-6th","7th-8th","9th", "10th", "11th", "12th", "Preschool"),
        Associates = c("Assoc-acdm", "Assoc-voc"),
        Diploma = c("Some-college", "HS-grad")
    )

df$income_bracket <- fct_collapse(df$income_bracket,
                             small = " <=50K.",
                             large = " >50K."
)


levels(df$education)
levels(df$income_bracket)

# levels(df$education)[levels(df$education)==" HS-grad"] <- "Diploma"
