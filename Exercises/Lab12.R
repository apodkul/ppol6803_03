## Lab 12
library(dplyr)


### Missing Data Techniques 
admissions <- read.csv("https://github.com/apodkul/ppol6803_03/raw/main/Data/Admissions_missing.csv") %>% 
  mutate(Research = factor(Research), 
         University.Rating = factor(University.Rating)) %>% 
  select(-Serial.No.)

admissions %>% 
  glimpse()

admissions %>% 
  summary()

# Median Imputation 
#install.packages("Hmisc")
admissions2 <- admissions

library(Hmisc)
admissions2$GRE.Score <- impute(admissions$GRE.Score, fun = median)

admissions2$GRE.Score %>% 
  summary()

# KNN
admissions3 <- admissions 

library(caret)
imputation_model <- caret::preProcess(admissions3, method = 'knnImpute')
output <- predict(imputation_model, admissions3)



# Random Forest 
admissions4 <- admissions 
#install.packages("missForest")
library(missForest)
imputation <- missForest(admissions4)
imputation
imputation$ximp

# MICE 
#install.packages("mice")
library(mice)
admissions5 <- admissions 
mice_data <- mice(data = admissions5, m = 5)

#example model 
models <- with(mice_data, lm(Chance.of.Admit~GRE.Score+Research+CGPA))
models

#example pooling
pool(models)


rm(admissions, admissions2, 
   admissions3, admissions4, 
   admissions5)

### SQL in R 
#install.packages("sqldf")
library(sqldf)
wine_data <- read.csv("https://github.com/apodkul/ppol6803_03/raw/main/Data/wine_quality.csv")

wine_data_overview <- data.frame(premium = c(0,1), 
                                 label = c("Not a premium wine", 
                                           "Premium wine"))

## Select all variables 
sqldf('SELECT * 
      FROM wine_data')

## Filter variables 
sqldf('SELECT * 
      FROM wine_data
      WHERE pH > 3.5')


## Select and Filter variables 
sqldf('SELECT pH, chlorides 
      FROM wine_data
      WHERE pH > 3.5')


## Aggregated statistics 
sqldf('SELECT premium, 
      AVG([pH]) as mean_val
      FROM wine_data
      GROUP BY premium')


## Example join
sqldf('SELECT *
      FROM wine_data as t1
      LEFT JOIN wine_data_overview as t2
      ON t1.premium = t2.premium')

