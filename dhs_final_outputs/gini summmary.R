rm(list=ls())
library(dplyr)
library(sf) # Include sf for consistency in your overall workflow
source('get_file_function.R')
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")
years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))
br_file_list <- vector('list',length(countries))
hr_file_list <- vector('list',length(countries))
wi_file_list <- vector('list',length(countries))
pr_file_list <- vector('list',length(countries))
for(i in seq_along(countries)){
  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]),  surveys = "BR")
  hr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "HR")
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI")
  pr_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "PR")
}
pr_file_list[[1]][5] <- c('/Users/matthewnicholson/DHS/NGPR2018.RDS')
ph_gini_results <- vector('list',length(countries))
calc_gini <- function(Data.Name, Class = NULL){

  if (is.null(Class)){
    
    Dat <- Data.Name
# Keep one case per household (e.g. hvidx=1 or hv101=1 or hvidx=hv003) in households with at least one de jure member
    Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
  mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members

Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
MIN <- min(Dat$hv271)

# IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 

Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000

ASSETS <- Dat %>% 
  group_by(category) %>% 
  summarise(assets = sum(assets, na.rm=TRUE)) 

Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)

# Note: some categories may be empty; best to renumber them; must be certain they are in sequence
Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))

Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)

# Calculate cumulative proportions for cases and assets. 
Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                     Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)

ncats = (2:max(Dat_cat$category))
for (i in (ncats)) {

  Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
  Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
  Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
  
  }

Gini=abs(1-sum(Dat_cat[["term"]]))

RESULTS <- matrix(0, nrow = 1, ncol = 2)
dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
RESULTS <- as.data.frame(RESULTS)
RESULTS[1, ] <- c("National", round(Gini, 3))

list(RESULTS)[[1]]

  } else {

  Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
  Data.Name$DomID  <- c(as.numeric(Data.Name[[Class]]))
    
  RESULTS <- matrix(0, nrow = max(as.numeric(Data.Name$DomID)), ncol = 2)
  dimnames(RESULTS) <- list(NULL, c("Class", "Gini"))
  RESULTS <- as.data.frame(RESULTS)
  
  for (j in 1:(max(as.numeric(Data.Name$DomID)))) {
    
    Dat = Data.Name[Data.Name$DomID == j, ]

    Dat <- Dat %>% filter(hv101==1 & hv012>0) %>%
      mutate(cases=hv012*hv005/1000000)   # Each household has hv012 de jure members
    
    Dat$category=1+as.integer(99*(Dat$hv271 - min(Dat$hv271))/(max(Dat$hv271)-min(Dat$hv271)))
    MIN <- min(Dat$hv271)
    
    # IMPORTANT! The assets for the household are weighted by hv005 but not multiplied by the number of household members
    CASES <- Dat %>% group_by(category) %>% summarise(cases = sum(cases, na.rm=TRUE)) 
    
    Dat$assets <- (Dat$hv271-MIN)*Dat$hv005/1000000
    
    ASSETS <- Dat %>% 
      group_by(category) %>% 
      summarise(assets = sum(assets, na.rm=TRUE)) 
    
    Dat_cat <- merge(ASSETS, CASES, by = c("category"), all.x = TRUE, all.y = TRUE)
    
    # Note: some categories may be empty; best to renumber them; must be certain they are in sequence
    Dat_cat <- transform(Dat_cat, category = as.numeric(factor(category)))
    
    Dat_cat$cases_prop= Dat_cat$cases/sum(Dat_cat$cases)
    Dat_cat$assets_prop= Dat_cat$assets/sum(Dat_cat$assets)
    
    # Calculate cumulative proportions for cases and assets. 
    Dat_cat[["cases_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["cases_prop"]], 0)
    Dat_cat[["assets_cumprop"]] <- ifelse(Dat_cat[["category"]]==1, Dat_cat[["assets_prop"]], 0)
    Dat_cat[["term"]] <- ifelse(Dat_cat[["category"]]==1, 
                                Dat_cat[["cases_prop"]]*Dat_cat[["assets_cumprop"]], 0)
    
    ncats = (2:max(Dat_cat$category))
    for (i in (ncats)) {
      
      Dat_cat[["cases_cumprop"]][i] <- Dat_cat[["cases_cumprop"]][i-1] + Dat_cat[["cases_prop"]][i]
      Dat_cat[["assets_cumprop"]][i] <- Dat_cat[["assets_cumprop"]][i-1] + Dat_cat[["assets_prop"]][i]
      Dat_cat[["term"]][i] <- Dat_cat[["cases_prop"]][i]*(Dat_cat[["assets_cumprop"]][i-1]+Dat_cat[["assets_cumprop"]][i])
      
    }
    
    Gini=abs(1-sum(Dat_cat[["term"]]))
    
    RESULTS[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(Gini, 3))
    
  }
  list(RESULTS)[[1]]
    
}
  
}
for( i in seq_along(countries)){
  for(j in seq_along(years[[i]])){
    PRdata <- readRDS(pr_file_list[[i]][j])
    if(!"hv270" %in% names(PRdata)){
      wi <- readRDS(wi_file_list[[i]][j]) |> 
        rename(hv270 = wlthind5,
               hv271 = wlthindf,
               hhid = whhid)
      PRdata <- PRdata |> 
        left_join(wi, by = "hhid", relationship = 'many-to-one')
      PRdata_gini <- select(PRdata,hv005, hv012, hv024, hv025, hv101, hv270, hv271)
    }
    else{
      PRdata <- readRDS(pr_file_list[[i]][j])
      PRdata_gini <- c()
PRdata_gini <- select(PRdata,hv005, hv012, hv024, hv025, hv101, hv270, hv271)
}

ph_gini_results[[i]][[j]] <- as.data.frame(rbind(calc_gini(PRdata_gini),
                            calc_gini(PRdata_gini,Class="hv025"),
                            calc_gini(PRdata_gini,Class="hv024")))

}
}

  
# tryCatch({
#   pr_list <- vector('list',length(countries))
#   for(i in seq_along(countries)){
#     message("processing country ",countries[[i]])
#     for(j in seq_along(years[[i]])){
#       message("year ",years[[i]][j])
#       pr_list[[i]][[j]] <- readRDS(pr_file_list[[i]][j])
    
#     }
#   }
# },
# error = function(e){
#   message("error in ",countries[[i]],"year ",years[[i]][j])
# })