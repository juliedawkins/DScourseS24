# install packages

#------------------------------------------------------------------
# Problem 3
#------------------------------------------------------------------
install.packages("mice")
library(mice)
library(modelsummary)
library(tidyverse)
library(broom)

#------------------------------------------------------------------
# Problem 4
#------------------------------------------------------------------
setwd("~/Desktop/DScourseS24/ProblemSets/PS7")
df <- read_csv("wages.csv") %>% 
  as.data.frame()

#------------------------------------------------------------------
# Problem 5
#------------------------------------------------------------------
df <- df %>% 
  filter(tenure != "NA") %>% 
  filter(hgc != "NA")

#------------------------------------------------------------------
# Problem 6
#------------------------------------------------------------------
summary <- datasummary_skim(df, output = 'latex')
datasummary_skim(df)
print(summary)

#------------------------------------------------------------------
# Problem 7: Deletion
#------------------------------------------------------------------

df_deletion <- df %>% 
  filter(!is.na(logwage))
deletion_regression <- lm(logwage ~ hgc + college + tenure + 
                       tenure^2 + age + married, data = df_deletion)
deletion_summary <- modelsummary(deletion_regression, stars = TRUE)

#------------------------------------------------------------------
# Problem 7: Mean Imputation
#------------------------------------------------------------------

df_mean <- df %>%
  mutate(logwage = ifelse(is.na(logwage), mean(logwage, na.rm = TRUE), logwage))
mean_regression <- lm(logwage ~ hgc + college + tenure + 
                        tenure^2 + age + married, data = df_mean)
mean_summary <- modelsummary(mean_regression, stars = TRUE)

#------------------------------------------------------------------
# Problem 7: Predicted Imputation
#------------------------------------------------------------------

# first, I will get the predicted values from the regression with complete data

df_predicted <- df 
df_predicted$predicted <- predict(deletion_regression, newdata = df_predicted)

df_predicted$logwage <- ifelse(is.na(df_predicted$logwage), 
                               df_predicted$predicted, df_predicted$logwage)

# remove the unncessary variable
df_predicted <- subset(df_predicted, select = -predicted)
predicted_regression <- lm(logwage ~ hgc + college + tenure + 
                             tenure^2 + age + married, data = df_predicted)
predicted_summary <- modelsummary(predicted_regression, stars = TRUE)

#------------------------------------------------------------------
# Problem 7: Mice and Multiple Imputation
#------------------------------------------------------------------

df_mice <- mice(df, m = 5, printFlag = FALSE)
mice_regression <- with(df_mice, lm(logwage ~ hgc + college + tenure + 
                                    tenure^2 + age + married)) 
mice_summary <- modelsummary(mice_regression)
print(mice_summary, stars = TRUE)

#------------------------------------------------------------------
# Problem 7: Printing Summaries
#------------------------------------------------------------------

modelsummary(list("List-Wise Deletion" = deletion_regression, "Mean Imputation" = mean_regression,
                  "Predicted Values" = predicted_regression, 
                  "Mice" = mice_regression), stars = TRUE, output = 'latex')
