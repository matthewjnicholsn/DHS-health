
#load in the files
CCHS <- read.csv('/Users/matthewnicholson/Downloads/2019-2020 CCHS_Public available_CSV/Data_Donn‚es/pumf_cchs.csv')
View(CCHS$GEN_005)
#rename var, need to go one at a time hence comments
CCHS <- CCHS %>% 
 # rename(hlth = GEN_005)
  #rename(cntry = SDCDGCB)
  #rename(vismin = SDCDVFLA)
  rename(imm = SDCDVIMM)

#now subset the df

CCHS <- CCHS[ , c('hlth', 'cntry', 'imm', 'vismin')]
#convert hlth to binary response
CCHS$hlth <- cut(CCHS$hlth, 2, labels=c("very unhealthy", "very healthy"))
CCHS$hlth <- ifelse(CCHS$hlth == "very unhealthy", 0, 1)

set.seed(1)
#Use 50% of dataset as training set and remaining 50% as testing set, random sample
sample <- sample(c(TRUE, FALSE), nrow(CCHS), replace=TRUE, prob=c(0.5,0.5))
train <- CCHS[sample, ]
test <- CCHS[!sample, ]  

#use glm to fit logistic model
log_model <- glm(hlth ~ imm + cntry +vismin, family = "binomial", data = train)
#dis sci.not
options(scipen = 999)
#see model
summary(log_model)