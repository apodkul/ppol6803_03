### Lab 08
library(dplyr)
library(ggplot2)

# Load in familiar data set
dset <- read.csv('https://github.com/apodkul/ppol6803_03/raw/main/Data/tx_county_data.csv')

## Linear Regression
mod <- lm(Unemployment~Uninsured+log(Population), 
          data = dset)
mod
summary(mod)
names(mod)

dset$preds <- predict(mod, 
        newdata = dset)


mod <- lm(Unemployment~Uninsured+I(Uninsured^2)+log(Population), 
          data = dset)
summary(mod)

####################################################################
# cBasic aret functions
####################################################################

## caret 
#install.packages("caret")
library(caret)

### Divide into training/testing set 
set.seed(1234)

trainingIdx <- createDataPartition(dset$Unemployment, 
                                   p = .8, 
                                   list = F
                                   )
trainingIdx

dset_train <- dset[trainingIdx,]
dset_test <- dset[-trainingIdx,]


####################################################################
# 1. KNN
####################################################################
#Example a: using Texas county dataset

# run model with default cross validation
knn_mod <- train(Unemployment~Uninsured,
                 data = dset_train, 
                 method = 'knn', 
                 trControl = trainControl(method = 'cv')
                 )
knn_mod

# run model with 5-fold cross validation, and tuning k by R-squared
knn_mod <- train(Unemployment~Uninsured,
                 data = dset_train, 
                 method = 'knn', 
                 metric = 'Rsquared',
                 tuneGrid=data.frame(k=c(1:40)),
                 trControl = trainControl(method = 'cv', 
                                          number = 5))
knn_mod

# Plot R-squared for each value of K 
names(knn_mod)
knn_mod$results %>%
  ggplot(aes(x = k, y = Rsquared)) + 
  geom_line()

# Make predictions from our model
predict(knn_mod, dset_train)
predict(knn_mod, dset_test)

# Plot Predictions against Actual
dset_test$pred <- predict(knn_mod, dset_test)
ggplot(dset_test) + 
  geom_point(aes(x = pred, y = Unemployment)) + 
  geom_abline(slope = 1, intercept = 0) #reference line


####################################################################
# 2. Regression Tree
####################################################################
#Example a: using life expectancy data set

life_expect <- read.csv('https://github.com/apodkul/ppol670_01/raw/main/Data/life_expect.csv')

trainingIdx <- createDataPartition(life_expect$life_expectancy, 
                                   p = .8, 
                                   list = F
)
trainingIdx

dset_train <- life_expect[trainingIdx,]
dset_test <- life_expect[-trainingIdx,]

dt_mod <- train(life_expectancy~log(GDP_per_capita)+Continent+log(Population),
                 data = dset_train, 
                 method = 'rpart'
)
dt_mod

plot(dt_mod)

plot(dt_mod$finalModel)
text(dt_mod$finalModel)

####################################################################
# 3. Bagging
####################################################################

#Example a: using life expectancy dataset
db_mod <- train(life_expectancy~log(GDP_per_capita)+Continent+log(Population),
                data = dset_train, 
                method = 'treebag'
)
db_mod


dset_test$predict_tree <- predict(dt_mod, dset_test)
dset_test$predict_bag <- predict(db_mod, dset_test)

RMSE(dset_test$predict_tree, dset_test$life_expectancy)
RMSE(dset_test$predict_bag, dset_test$life_expectancy)


varImp(dt_mod)
varImp(db_mod)

####################################################################
# 4. Fitting a basic linear model (OLS)
####################################################################

#Example a: using Sacramento data set, varying cross validation
data("Sacramento")

set.seed(1789)
training_ind <- createDataPartition(Sacramento$price, 
                                    p = .75, 
                                    list = FALSE)

sac_train <- Sacramento[training_ind,] 
sac_test <- Sacramento[-training_ind,]

#5 fold 
trc <- trainControl(
  method = 'cv',
  number = 5
)

#LOOCV
trc2 <- trainControl(
  method = 'LOOCV'
)


fit1 <- train(price ~ beds + baths + sqft + type, 
              data = sac_train, 
              method = 'lm', 
              trControl = trc)
fit1

fit1$finalModel

fit1$finalModel %>%
  summary()

#Fit a Linear Model 
fit2 <- train(price ~ beds + baths + sqft + type, 
              data = sac_train, 
              method = 'lm', 
              trControl = trc2)
fit2

fit2$finalModel

fit2$finalModel %>%
  summary()


####################################################################
# [Time permitting] Testing Loops
####################################################################

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

#or
library(dplyr)
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
