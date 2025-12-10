source('get_file_function.R')
library(dplyr);library(tidyr)
ng_90_cov <- read.csv('/Users/matthewnicholson/DHS/GPS files/Nigeria/1990/NGGC22FL/NGGC22FL.csv')
file_list <- get_file(countries = "Nigeria", years = 1990, surveys = c("HR","BR","PR", "WI"))
ng_90_hr <- readRDS(file_list[[1]]); ng_90_br <- readRDS(file_list[[2]]); ng_90_pr <- readRDS(file_list[[3]]); ng_90_wi <- readRDS(file_list[[4]]) |> rename(hhid=whhid)

# b3
# Date of birth of child (CMC)
# b5
# Child is alive (1 = Yes, 0 = No)
# b7
# Age at death in months (imputed)
# v008
# Date of interview (CMC)
# v005
# Woman’s individual sample weight
ng_90_chmort <- ng_90_br |> 
  select(c("b3",'b5','b7','v008','v005','v001','v002')) |> 
  rename(hv001 = v001,
         hv002 = v002)

ng_90_hr <- left_join(ng_90_hr, ng_90_wi, by = "hhid", relationship = 'many-to-one')

ng_90_comb <- ng_90_hr |> 
  left_join(ng_90_chmort, by = c("hv001","hv002"), relationship = 'one-to-many') |> 
  rename(urban_rural = hv025,
         wealth_factor = wlthindf,
         region = hv024,
         education = hv107_01,
         child_alive = b5,
         child_age_at_death = b7, 
         wts_h = hv005
         ) |> 
  select(urban_rural,wealth_factor,region,education,child_alive,wts_h) |> 
  mutate(urban = ifelse(urban_rural == 1, 1, 0),
         region_se = ifelse(region == 1, 1, 0),
         region_sw = ifelse(region == 2,1,0),
         region_nw = ifelse(region == 3,1,0),
         region_ne = ifelse(region==4,1,0),
         child_dead = ifelse(child_alive == 0, 1, 0)) |> 
  filter(education < 98 | education >= 0) |> 
  drop_na()

glm_test_crude_death <- glm(child_dead ~ urban + region_se + region_sw + region_nw + region_ne + wealth_factor + education, data = ng_90_comb, family = "binomial", weights = wts_h)


set.seed(5)
#Use 50% of dataset as training set and remaining 50% as testing set, random sample
sample <- sample(c(TRUE, FALSE), nrow(ng_90_comb), replace=TRUE, prob=c(0.5,0.5))
train <- ng_90_comb[sample, ]
test <- ng_90_comb[!sample, ]  

#drop incomplete cases

#use glm to fit logistic model
glm_test_crude_death <- glm(child_dead ~ urban + region_se + region_sw + region_nw + wealth_factor + education, 
  data = ng_90_comb, family = "binomial", weights = wts_h)
#dis sci.not
options(scipen = 999)
#see model
summary(glm_test_crude_death)
#check fit
pscl::pR2(glm_test_crude_death)["McFadden"]
#check variable fitness
varImp(glm_test_crude_death)
#check co-linearity
car::vif(glm_test_crude_death)
#co-linearity is a problem


##after removing the moduar variables (too many NA's) there is solid fit, 
#strangely immigration and geog became much less important when adding geography and education

# now we test the model
#define two individuals
new <- data.frame(wealth_factor = 1, urban = 1, education = 10, region_se = 0, region_sw = 0, region_nw = 1)

#predict probability of defaulting
predict(glm_test_crude_death, new, type="response")
predicted <- predict(glm_test_crude_death, test, type="response")



#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$child_dead, predicted)[1]
optimal
#confusionmatrix to check observed vs predicted
confusionMatrix(test$child_dead, predicted)

#calculate sensitivity
sensitivity(test$child_dead, predicted)


#calculate specificity
specificity(test$child_dead, predicted)


#calculate total misclassification error rate
misClassError(test$child_dead, predicted, threshold=optimal)
##miscalc is around 26% which is not amazing

#Lastly, we can plot the ROC (Receiver Operating Characteristic) Curve which 
#displays the percentage of true positives predicted by the model as the prediction 
#probability cutoff is lowered from 1 to 0. The higher the AUC (area under the curve), 
#the more accurately our model is able to predict outcomes:
plotROC(test$child_dead, predicted)
