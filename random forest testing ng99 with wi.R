#random forest test run
library(randomForest)
library(haven)
library(dplyr)
library(tidyr)
library(beepr)
Ngrf <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGHR21DT/NGHR21FL.DTA")
Ngwi <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA")

#merge wealth index with HR
Ngwi <- Ngwi%>% 
  select(-wlthindf) %>% 
  rename(hhid = whhid)

Ngrf <- merge(Ngrf, Ngwi, by = "hhid")

Ngrf <- Ngrf %>%
  select(where(~ any(!is.na(.)))) %>%  # Remove columns with all NA values
  mutate(across(where(is.numeric), 
                ~replace_na(., median(., na.rm = TRUE)))) %>% 
  mutate_all(as.numeric) #make all variables numeric

set.seed(1) # make this reproducible

#fit the random forest model with current marital status as the predictor variable
model <- randomForest(
  formula = hv115 ~ .,
  data = Ngrf, na.action=na.exclude
)

#see the model
model
#how many of the 500 trees produce lowest MSE
which.min(model$mse)

#RMSE of "best" model
sqrt(model$mse[which.min(model$mse)]) 
plot(model)
varImpPlot(model) 
