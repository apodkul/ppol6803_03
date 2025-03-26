## Combined Lab 10
## In-Class Coding Notes 

########################################################################
## Dependencies
########################################################################
## Interpretable ML
#install.packages('iml')
library(iml)
library(caret)
library(dplyr)

########################################################################
## Data set #1 (Admissions): Using IML types
########################################################################

### Load Data 
admissions <- read.csv("https://github.com/apodkul/ppol6803_03/raw/main/Data/Admission.csv")

admissions %>% 
  glimpse() 

admissions <- admissions %>% 
  dplyr::select(-Serial.No.) 

### Estimating Base Model 
base_mod <- caret::train(Chance.of.Admit~., 
                         data = admissions, 
                         method = 'rf', 
                         tuneGrid = expand.grid(mtry = c(2,3,5,7)))
base_mod 

varImp(base_mod)

### General Structure of `iml`
## For more information, see documentation at: https://cran.r-project.org/web/packages/iml/iml.pdf

### Partial Dependence Plots 
store_modl <- Predictor$new(base_mod, data = admissions)
store_modl
pdp <- FeatureEffect$new(store_modl, feature = 'GRE.Score', 
                         method = 'pdp', grid.size = 35)
pdp

pdp$results

pdp$plot()

pdp$plot() + 
  theme_dark()

## Two Features 
pdp2 <- FeatureEffect$new(store_modl, feature = c('GRE.Score', 'CGPA'), 
                         method = 'pdp', grid.size = 35)
ggplot(pdp2$results, aes(x = GRE.Score, y = CGPA, fill = .value)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "2D Partial Dependence Plot",
       x = "GRE Score",
       y = "CGPA",
       fill = "Prediction")


### Permutation Feature Importance 
store_modl <- Predictor$new(base_mod, 
                            data = admissions)
pfi <- FeatureImp$new(store_modl, loss = 'rmse')
pfi

plot(pfi)

### Individual Conditional Expectation
ice <- FeatureEffect$new(store_modl, feature = 'CGPA', 
                          method = 'ice', grid.size = 35)
ice
ice$plot()

ice_with <- FeatureEffect$new(store_modl, feature = 'CGPA', 
                         method = 'pdp+ice', grid.size = 35)
ice_with$plot()

ice_with_centered <- FeatureEffect$new(store_modl, feature = 'CGPA', 
                              method = 'pdp+ice', grid.size = 35, 
                              center.at = 0)
ice_with_centered$plot()


########################################################################
## Data set #2 (Housing): Surrogate Models
########################################################################

#New data
housing <- read.csv('https://github.com/apodkul/ppol6803_03/raw/main/Data/BostonHousing.csv')
#Codebook: https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

housing %>% 
  glimpse()

model <- caret::train(medv~., 
                      data = housing, 
                      method = 'rf'
                      )

housing$predict_rf <- predict(model, housing)

#LM
surrogate_model_lm <- lm(predict_rf~., 
                         data = housing %>% 
                           dplyr::select(-medv)
                           )
summary(surrogate_model_lm)

housing$predict_lm <- predict(surrogate_model_lm, 
                              housing)



#RPART
surrogate_model_rpart <- rpart(predict_rf~., 
                         data = housing %>% 
                           dplyr::select(-medv, -predict_lm)
)
rpart.plot::rpart.plot(surrogate_model_rpart)

housing$predict_rpart <- predict(surrogate_model_rpart, 
                                 housing)

#Assess
caret::R2(pred = housing$predict_lm, 
          obs = housing$predict_rf)

caret::R2(pred = housing$predict_rpart, 
          obs = housing$predict_rf)



#or... using `iml`... will likely need to install `partykit`
tmp_mod <- Predictor$new(model, data = housing)
surrogateTree <- TreeSurrogate$new(tmp_mod)

plot(surrogateTree)

surrogateTree$r.squared

