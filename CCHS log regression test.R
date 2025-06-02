library(dplyr)
library(ggplot2)
library(pscl)
library(caret)
library(mice)
library(InformationValue)
library(survey)
#load in the files
CCHS <- read.csv('/Users/matthewnicholson/Downloads/2019-2020 CCHS_Public available_CSV/Data_Donn‚es/pumf_cchs.csv')
#rename var, need to go one at a time hence comments
#health overall self rated
CCHS <- CCHS %>% 
  rename(hlthy = GENDVHDI, cntry = SDCDGCB, vismin = SDCDVFLA, income = INCDGHH, 
         food = FSCDVHF2, school = EHG2DVH3,satisf = GENDVSWL, geog = GEOGPRV, wt = WTS_M)
##categorical, may need to dummy var it

#need to clean variables based on scores that mean NA
CCHS$vismin[(CCHS$vismin %in% 1:2) == FALSE] <- NA
CCHS$income[(CCHS$income %in% 1:5) == FALSE] <- NA
CCHS$food[(CCHS$food %in% 0:3) == FALSE] <- NA
CCHS$school[(CCHS$school %in% 1:3) == FALSE] <- NA
CCHS$satisf[(CCHS$satisf %in% 1:5) == FALSE] <- NA

#geog needs to be categorized into dummy vars as it is categorical



#create dummy variables for imm due to co-linearity
imm <- ifelse(CCHS$cntry == '2', 1, 0)
# subset the df
CCHS <- data.frame(hlthy = CCHS$hlthy,
                   visvim = CCHS$vismin,
                   income = CCHS$income,
                   food = CCHS$food,
                   school = CCHS$school,
                   satisf = CCHS$satisf,
                   geog = CCHS$geog,
                   imm = imm,
                   wt = CCHS$wt)
                     
#convert hlthy to binary response
CCHS$hlthy <- cut(CCHS$hlthy, 2, labels=c("very unhealthy", "very healthy"))
CCHS$hlthy <- ifelse(CCHS$hlthy == "very unhealthy", 0, 1)

#handle NA values
CCHS <- CCHS[complete.cases(CCHS), ]


set.seed(1)
#Use 50% of dataset as training set and remaining 50% as testing set, random sample
sample <- sample(c(TRUE, FALSE), nrow(CCHS), replace=TRUE, prob=c(0.5,0.5))
train <- CCHS[sample, ]
test <- CCHS[!sample, ]  

#drop incomplete cases

#use glm to fit logistic model
log_model <- svyglm(hlthy ~ imm + income + visvim + food + school + satisf,  
                    family = quasibinomial(),
                    design = svydesign(id = ~1, weight = ~wt, data = train))
#having some serious problems with this GLM
log_model <- glm(hlthy ~ imm + income + visvim + food + school + satisf + 
                   geog, family = "binomial", data = train)
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
new <- data.frame(imm = 0, income = 0, visvim = 1, school = 1, food = 0, 
                  satisf = 3, hlthy = c("0", "1"))

#predict probability of defaulting
predict(log_model, new, type="response")
predicted <- predict(log_model, test, type="response")



#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$hlthy, predicted)[1]
optimal
#confusionmatrix to check observed vs predicted
confusionMatrix(test$hlthy, predicted)

#calculate sensitivity
sensitivity(test$hlthy, predicted)


#calculate specificity
specificity(test$hlthy, predicted)


#calculate total misclassification error rate
misClassError(test$hlthy, predicted, threshold=optimal)

#Lastly, we can plot the ROC (Receiver Operating Characteristic) Curve which 
#displays the percentage of true positives predicted by the model as the prediction 
#probability cutoff is lowered from 1 to 0. The higher the AUC (area under the curve), 
#the more accurately our model is able to predict outcomes:
plotROC(test$hlthy, predicted)
#area under curve is 0.82 so this model is pretty good at predicting if an individual will have poor health