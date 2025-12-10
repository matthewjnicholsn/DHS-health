library(dplyr)
library(DHS.rates)

source('get_file_function.R')
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")
years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

br_file_list <- vector('list',length(countries))
wi_file_list <- vector('list',length(countries))
ir_file_list <- vector('list',length(countries))
pr_file_list <- vector('list',length(countries))
chmort_results <- vector('list',length(countries))

for(i in seq_along(countries)){

  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "BR") 
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI") 
  ir_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "IR") 
  pr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "PR") 
}

for(i in seq_along(countries)){
  message('processing country: ',countries[[i]])
  for(j in seq_along(years[[i]])){
    message('processing year: ',years[[i]][j])
    br <- readRDS(br_file_list[[i]][j])
    if(!"v190" %in% names(br)){
      wi <- readRDS(wi_file_list[[i]][j]) |> 
        rename(v190 = wlthind5,
               hhid = whhid)
      br <- br |> 
        mutate(hhid = substr(caseid,1,12)) |> 
        left_join(wi, by = "hhid", relationship = "many-to-one")

    chmort_results[[i]][[j]] <- chmort(br)
      
    }
    else{
      chmort_results[[i]][[j]] <-chmort(br)
      
    }
  }
}

library(survey)
br <- mutate(br, v005 = v005/1000000)
design_obj <- svydesign(ids = ~v021,  data = br, weights = ~v005)
options(survey.lonely.psu = "adjust")
  svytable(formula =, design = design_obj)

br$b5 <- as.numeric(br$b5)
br$wealthy <- ifelse(br$v190.x >= 3, 1, 0)
br$poor <- ifelse(br$v190.x < 3, 1, 0)
glm <- glm(b5 ~ b4 + v106 + v191, data = br, 
  weights = v005, 
  family = "binomial", na.action = na.exclude)

#weighted
# Call:
# glm(formula = b5 ~ b4 + v106 + v191, family = "binomial", data = br, 
#     weights = v005, na.action = na.exclude)

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 1.226e+00  5.409e-02  22.667  < 2e-16 ***
# b4          8.365e-02  3.125e-02   2.677  0.00743 ** 
# v106        2.780e-01  2.324e-02  11.960  < 2e-16 ***
# v191        1.547e-06  2.125e-07   7.280 3.33e-13 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 26942  on 29547  degrees of freedom
# Residual deviance: 26564  on 29544  degrees of freedom
# AIC: 25465

# Number of Fisher Scoring iterations: 4

# #unweighted
# Call:
# glm(formula = b5 ~ b4 + v106 + v191, family = "binomial", data = br, 
#     na.action = na.exclude)

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 1.196e+00  5.377e-02  22.247  < 2e-16 ***
# b4          1.451e-01  3.121e-02   4.649 3.34e-06 ***
# v106        1.986e-01  2.334e-02   8.509  < 2e-16 ***
# v191        2.231e-06  2.132e-07  10.464  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 26988  on 29547  degrees of freedom
# Residual deviance: 26596  on 29544  degrees of freedom
# AIC: 26604

# Number of Fisher Scoring iterations: 4

svyglm <- svyglm(b5 ~ b4 + v106 + v191, design = design_obj, family = stats::quasibinomial, na.action = na.omit)

