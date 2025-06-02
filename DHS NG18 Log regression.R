library(dplyr)
library(ggplot2)
library(pscl)
library(caret)
library(haven)

IR <- read_dta("Chap8/NGIR7BFL.DTA")

#rename var of interest for regression

IR <- IR %>% 
  rename(wealth = v190a, births = v201, region = v024, literacy = v155)

#dummy var for region
#convert region to vector
IR$region <- as.numeric(as.character(IR$region))

nc <- ifelse(IR$region == '1', 1, 0)
ne <- ifelse(IR$region == '2', 1, 0)
nw <- ifelse(IR$region == '3', 1, 0)
se <- ifelse(IR$region == '4', 1, 0)
ss <- ifelse(IR$region == '5', 1, 0)
sw <- ifelse(IR$region == '6', 1, 0)

#dummy var for literacy
#convert literacy to factor
IR$literacy <- as.numeric(IR$literacy)

#set NA for lit
IR$literacy[(IR$literacy %in% 0:2) == FALSE] <- NA
#dummy var  
illit <- ifelse(IR$literacy == '0', 1, 0)
lit <- ifelse(IR$literacy == '2', 1,0)
#create subset of IR
IR <- data.frame(wealth = IR$wealth,
                   births = IR$births,
                   nw = nw,
                   lit = lit)
#handle NA
IR <- IR[complete.cases(IR), ]


set.seed(1)
#Use 50% of dataset as training set and remaining 50% as testing set, random sample
sample <- sample(c(TRUE, FALSE), nrow(IR), replace=TRUE, prob=c(0.5,0.5))
train <- IR[sample, ]
test <- IR[!sample, ]  

#drop incomplete cases

#use glm to fit logistic model
log_model <- glm(lit ~ wealth + births + nw, family = "binomial", data = train)
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
new <- data.frame(wealth = 2, births = 6, nw = 1, lit = c("0", "1"))

#predict probability of defaulting
predict(log_model, new, type="response")
predicted <- predict(log_model, test, type="response")



#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$lit, predicted)[1]
optimal
#confusionmatrix to check observed vs predicted
confusionMatrix(test$lit, predicted)

#calculate sensitivity
sensitivity(test$lit, predicted)


#calculate specificity
specificity(test$lit, predicted)


#calculate total misclassification error rate
misClassError(test$lit, predicted, threshold=optimal)
##miscalc is around 26% which is not amazing

#Lastly, we can plot the ROC (Receiver Operating Characteristic) Curve which 
#displays the percentage of true positives predicted by the model as the prediction 
#probability cutoff is lowered from 1 to 0. The higher the AUC (area under the curve), 
#the more accurately our model is able to predict outcomes:
plotROC(test$lit, predicted)
#area under curve is 0.78 so this model is okay  at predicting if an individual will have poor literacy
