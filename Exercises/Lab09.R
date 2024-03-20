## Combined Lab 08 and 09
## In-Class Coding Notes 

library(dplyr)
library(caret)
library(ggplot2)

########################################################################
## Background: Splitting our data into training and testing
########################################################################
tx_cty <- read.csv('https://github.com/apodkul/ppol6803_03/raw/main/Data/tx_county_data.csv')


### Training set, hold out set 
set.seed(1234)

trainingIdx <- createDataPartition(tx_cty$FIPS_ST_CN, 
                                   p = .85, 
                                   list = F
)
trainingIdx

tx_cty_train <- tx_cty[trainingIdx,]
tx_cty_test <- tx_cty[-trainingIdx,]



########################################################################
## Part 1: Regression
########################################################################

## Basic Linear Model (R Equivalent of reg y x1 x2)
lm(Unemployment~Uninsured+log(Population), 
   data = tx_cty_train)
mod_lm <- lm(Unemployment~Uninsured+log(Population), 
   data = tx_cty_train)
summary(mod_lm)

tx_cty_train$fitted_values <- predict(mod_lm, tx_cty_train)

ggplot(tx_cty_train, aes(x = Unemployment, y = fitted_values)) + 
  geom_point()

## Using caret: lm
mod_lm <- train(Unemployment~Uninsured+log(Population),
                 data = tx_cty_train, 
                 method = 'lm', 
                 trControl = trainControl(method = 'cv', 
                                          number = 5)
)
mod_lm
mod_lm$finalModel
tx_cty_train$fitted_values <- predict(mod_lm, tx_cty_train)


## Using caret: KNN
mod_knn <- train(Unemployment~Uninsured+log(Population),
                 data      = tx_cty_train,
                 method    = 'knn', 
                 tuneGrid  = data.frame(k=1:10),
                 trControl = trainControl(method = 'cv', 
                                          number = 5))
mod_knn

mod_knn <- train(Unemployment~Uninsured+log(Population),
                 data      = tx_cty_train,
                 method    = 'knn', 
                 metric    = 'Rsquared',
                 tuneGrid  = data.frame(k=1:10),
                 trControl = trainControl(method = 'cv', 
                                          number = 5))
mod_knn


## Using caret: Regression Tree
mod_rt <- train(Uninsured~Population,
                 data      = tx_cty_train,
                 method    = 'rpart', 
                 tuneGrid   = data.frame(cp = seq(.01, .08, by = .01)),
                 trControl = trainControl(method = 'cv', 
                                          number = 5))

plot(mod_rt)

plot(mod_rt$finalModel)
text(mod_rt$finalModel)


## Using caret: Bagging
mod_bag <- train(Uninsured~Population,
                data = tx_cty_train, 
                method = 'treebag', 
                trControl = trainControl(method = 'cv', 
                                         number = 5)
)
mod_bag

########################################################################
## Part 2: Classification
########################################################################

### Loading New Data Set 
heart <- read.csv(file = 'https://github.com/apodkul/ppol6803_03/raw/main/Data/processed_cleveland.csv')

heart %>% 
  glimpse()

heart %>% 
  purrr::modify_if(is.character, as.numeric) %>%
  glimpse()
  
heart <- heart %>% 
  purrr::modify_if(is.character, as.numeric) 

heart <- heart %>% 
  na.omit()



## Basic Logistic Model (R Equivalent of reg y x1 x2)
mod_log <- glm(heart~chol+sex, 
               data = heart, 
               family = binomial(link = 'logit'))

## Using caret: KNN
mod_log <- train(
  heart~chol+sex, 
  data = heart, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)

heart$heart <- factor(heart$heart)
mod_log <- train(
  heart~chol+sex, 
  data = heart, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)

confusionMatrix(mod_log)

## Using caret: Random Forest
mod_rf <- train(
  heart~., 
  data = heart, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'rf'
)
mod_rf

varImp(mod_rf)



## Classification metrics 
heart$predict_rf <- predict(mod_rf, heart)
class_predictions <- predict(mod_rf, heart, type = 'prob')
heart$predict_rf_prob <- class_predictions[,2]

#install.packages("pROC")
library(pROC)

roc(heart$heart~heart$predict_rf_prob, 
    plot = T, 
    print.auc = T)


#install.packages("predtools")
library(predtools)
heart$predict_logit_prob <- predict(mod_log, heart, type = 'prob')[,2]

heart$heart <- as.numeric(as.character(heart$heart))
calibration_plot(data = heart, 
                 obs = 'heart', 
                 pred = 'predict_logit_prob')
