# Load necessary libraries
library(readxl)
library(readr)
library(dplyr)

# Load the dataset
file_path <- 'clean_data.csv'
df <- read_csv(file_path)

# Display the first few rows of the dataset
head(df)
str(df)
summary(df)

# Create a new data frame with the adjusted counts
df$Student_ID = as.factor(df$Student_ID)
df$Skill = as.factor(df$Skill)

# Fit the initial logistic regression model
log_model <- glm(Success ~ Student_ID + Skill + Opportunity:Skill - 1,
                 family = binomial(),
                 data = df)
summary = summary(log_model)

# result
coefficients = coef(log_model)
AIC = summary(log_model)$aic

# print
model_summary <- capture.output(summary)
writeLines(model_summary, "model_summary_R_raw.txt")
