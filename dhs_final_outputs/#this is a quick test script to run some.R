#this is a quick test script to run some models on the country-year panel data.
lapply(c("dplyr", "ggplot2", "pscl", "caret", "haven",'glmmTMB','InformationValue'), require, character.only = T)


dat_chmort <- chmort_res_list[[1]][[5]]
dat_wealth <- wealth_res_list[[1]][[5]]
dat_gini <- gini_res_list[[1]][[5]]

dat_combine <- dat_chmort |> 
  left_join(dat_wealth, by = c("cluster","year","country","geometry")) |> 
  left_join(dat_gini, by = "cluster") |> 
  mutate(urban = ifelse(urban_rural_code == 1, 1, 0))
#some issue with gini join 
#rename var of interest for regression

#handle NA
# IR <- IR[complete.cases(IR), ]


set.seed(1)
#Use 50% of dataset as training set and remaining 50% as testing set, random sample
sample <- sample(c(TRUE, FALSE), nrow(dat_combine), replace=TRUE, prob=c(0.5,0.5))
train <- dat_combine[sample, ]
test <- dat_combine[!sample, ]  

#drop incomplete cases

#use glm to fit logistic model
log_model <- glm(prob ~ weighted_wealth + urban + cluster, data = train)
#dis sci.not
options(scipen = 999)
#see model
summary(log_model)
#check fit
pscl::pR2(log_model)["McFadden"]
#check variable fitness
varImp(log_model)
#check co-linearity
car::vif(log_model)
#co-linearity is a problem


##after removing the moduar variables (too many NA's) there is solid fit, 
#strangely immigration and geog became much less important when adding geography and education

# now we test the model
#define two individuals
new <- data.frame(weighted_wealth = 2, urban = 1, cluster = 100)

#predict probability of defaulting
predict(log_model, new, type="response")
predicted <- predict(log_model, test, type="response")



#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$prob, predicted)[1]
optimal
#confusionmatrix to check observed vs predicted
confusionMatrix(test$prob, predicted)

#calculate sensitivity
sensitivity(test$prob, predicted)


#calculate specificity
specificity(test$prob, predicted)


#calculate total misclassification error rate
misClassError(test$prob, predicted, threshold=optimal)
##miscalc is around 26% which is not amazing

#Lastly, we can plot the ROC (Receiver Operating Characteristic) Curve which 
#displays the percentage of true positives predicted by the model as the prediction 
#probability cutoff is lowered from 1 to 0. The higher the AUC (area under the curve), 
#the more accurately our model is able to predict outcomes:
plotROC(test$prob, predicted)
#area under curve is 0.78 so this model is okay  at predicting if an individual will have poor literacy
library(moments)
kurtosis(dat_combine$prob)
skewness(dat_combine$prob)
hist(dat_combine$prob)
hist(auto_krig_df$var1.pred)

