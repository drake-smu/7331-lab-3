library(dplyr)


#------Data Specific Cleaning------------

df <- read.csv("./data/adult-test.csv")

summary(df)

#Lets look at NA value's first.
NA_sum <- sort(sapply(df, function(x) sum(is.na(x))), decreasing = TRUE)
print(NA_sum)

Nodip <- c(" 1st-4th" , " 5th-6th" ," 7th-8th" ," 9th" , " 10th" , " 11th" , " 12th" , " Preschool" )

levels(df$education)[levels(df$education)==" 1st-4th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 5th-6th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 7th-8th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 9th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 10th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 11th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" 12th"] <- "No Diploma"
levels(df$education)[levels(df$education)==" PreSchool"] <- "No Diploma"

levels(df$education)




# c("1st-4th","5th-6th")
# recode_factor(df$education, "1st-4th"="No Diploma",
#                        "5th-6th"="No Diploma",
#                        "7th-8th"="No Diploma",
#                        "9th" = "No Diploma",
#                        "10th" = "No Diploma",
#                        "11th" = "No Diploma",
#                        "12th" = "No Diploma",
#                        "Preschool" = "No Diploma",
#                        "Assoc-admin" = "Associates")

