

calc_gini <- function(Data.Name, Class){
  
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



for(j in seq_along(years[[i]])){
  tryCatch({
  gini_data <- readRDS(pr_file_list[[i]][j])
  if(!"hv270" %in% names(gini_data)){
    wi_data <- readRDS(wi_file_list[[i]][j]) |> 
      rename(hv270 = wlthind5,
             hv271 = wlthindf,
             hhid = whhid)
    gini_data <- gini_data |> 
      left_join(wi_data, by = "hhid", relationship = "many-to-one")
  }
  },
  error = function(e){
    message("Failed to load data for gini calculations", countries[[i]], " ", years[[i]][j])
  }
)

  gini_data <- gini_data |> 
    select(hv001,hv005, hv012, hv024, hv025, hv101, hv270, hv271)

  if(is.null(gini_data$hv001) == T){
    message("hv001 is null")

  }

gini_results <- as.data.frame(rbind(calc_gini(gini_data,Class="hv001") |> 
  rename(cluster = Class)
  # ,calc_gini(gini_data,Class="hv024")
  ))

  #write files to a file list

file_name <- paste0("gini_clust_", countries[[i]], "_", years[[i]][j], "_.csv")
    gini_results_file_list[[i]][j] <- file_name
  gini_res_list[[i]][[j]] <- gini_results
    write.csv(gini_results, file = file_name, row.names = FALSE)
  
}
