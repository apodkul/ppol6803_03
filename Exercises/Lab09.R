library(dplyr)
library(ggplot2)
library(caret)

####################################################################
# 1. LOOPS LOOPS LOOPS 
####################################################################
dset <- read.csv('https://github.com/apodkul/ppol6803_03/raw/main/Data/tx_county_data.csv')

## Data Diversion: Iteration 

### for
for(bananas in 1:100){
  print(bananas)
}

### while 
j <- FALSE
i <- 1
while(j == F){
  print(i)
  if(i >= 42){
    j <- T
  }
  i <- i + 2
}

### apply
apply(dset[c('Population', 'Unemployment')], 2, median)

#or (incorporating into a piped function)
dset %>% 
  select(Population, Unemployment) %>%
  apply(2, median)


### purrr
library(purrr)

dset %>% 
  select(Population, Unemployment) %>%
  map(.f = median)

dset %>% 
  select(Population, Unemployment) %>%
  map_dbl(.f = median)



####################################################################
# 2. CLASSIFICATION 
####################################################################

### Load data set 
wine <- read.csv(file = 'https://github.com/apodkul/ppol6803_03/raw/main/Data/wine_quality.csv')

wine %>% 
  glimpse

wine <- wine %>% 
  na.omit()

# Split into testing and training (using caret function)
set.seed(1234)

trainingIdx <- createDataPartition(wine$premium, 
                                   p = .8, 
                                   list = F
)
trainingIdx

wine_train <- wine[trainingIdx,]
wine_test <- wine[-trainingIdx,]


####################################################################
# a. Logistic regression  
####################################################################
mod_log <- glm(premium~alcohol+pH, 
               data = wine_train, 
               family = binomial(link = 'logit'))
mod_log 
summary(mod_log)


## Using caret: Logistic regresion
mod_log <- train(
  premium~alcohol+pH, 
  data = wine_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)
#what does the warning tell us? 

wine_train$premium <- factor(wine_train$premium)
wine_train$premium <- factor(wine_train$premium) #why?
mod_log <- train(
  premium~alcohol+pH, 
  data = wine_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)
mod_log

mod_log$finalModel

mod_log$finalModel %>% 
  summary

confusionMatrix(mod_log)
#which data is being used in this confusion matrix? 



####################################################################
# b. Random Forests 
####################################################################
mod_rf <- train(
  premium~., 
  data = wine_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'rf'
)
mod_rf

mod_rf$finalModel

varImp(mod_rf)


## Making predictions 
predict(mod_rf, wine_test)
wine_test$predict_rf_class <- predict(mod_rf, wine_test)
predict(mod_rf, wine_test, type = 'prob')
wine_test$predict_rf_prob <- predict(mod_rf, wine_test, type = 'prob')[,2]




####################################################################
# c. Going a bit deeper with model diagnostics 
####################################################################
#install.packages("pROC")
library(pROC)
roc(data = wine_test, 
    response = premium, 
    predictor= predict_rf_prob, 
    plot= T)
 
#install.packages("predtools")
library(predtools)
wine_test$predict_logit_prob <- predict(mod_log, wine_test, type = 'prob')[,2]

calibration_plot(data = wine_test, 
                 obs = 'premium', 
                 pred = 'predict_logit_prob')



