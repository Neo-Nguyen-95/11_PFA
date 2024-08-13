# Load necessary libraries
library(readxl)
library(readr)
library(dplyr)

# Load the dataset
file_path <- 'data_clean_update.csv'
df <- read_csv(file_path)

# Display the first few rows of the dataset
head(df)
str(df)
summary(df)

# Create a new data frame with the adjusted counts
df$Student_ID = as.factor(df$Student_ID)
df$Skill = as.factor(df$Skill)

####--- Fit the LFA logistic regression model
lfa_model <- glm(Success ~ Student_ID + Skill + Opportunity:Skill - 1,
                 family = binomial(),
                 data = df)
lfa_summary <- summary(lfa_model)

# result
lfa_coefficients = coef(lfa_model)
AIC = summary(lfa_model)$aic

# print
lfa_summary_capture <- capture.output(lfa_summary)
writeLines(lfa_summary_capture, "LFA_summary_R_raw.txt")

####--- Fit the PFA logistic regression model
pfa_model <- glm(Success ~ Skill + Success_opportunity:Skill + Fail_opportunity:Skill -1,
                 family = binomial(),
                 data=df)
pfa_summary <- summary(pfa_model)

# result
pfa_coefficients <- coef(pfa_model)
AIC <- summary(pfa_model)$aic

# print
writeLines(capture.output(pfa_summary), "PFA_summary_R_raw.txt")
