rm(list = ls(all = TRUE))
library(haven)
library(dplyr)
library(openxlsx)
library(writexl)
library(naniar)#for replace_with_na fx
library(sjlabelled) #for fx set_label
library(DHS.rates) #for functions like chmort
library(data.table) #for tables from mort calculations
library(beepr)
 
 #laod WI data for 1990 DHS
#laod WI data for 1990 DHS
WI <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA")
WI <- WI %>% 
  select(-wlthindf) %>% 
  rename(whhid, hhid = whhid) %>% 
  mutate(hhid = as.numeric(gsub(" ", "", hhid)))

BR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGBR21DT/NGBR21FL.dta")
BR <- BR %>% 
  mutate(gsub('.{2}$', '', BR$caseid)) %>% 
  rename(`gsub(".{2}$", "", BR$caseid)`, hhid = `gsub(".{2}$", "", BR$caseid)`) %>% 
  mutate(hhid = as.numeric(gsub(" ", "", hhid))) %>% 
  left_join(WI %>% distinct(hhid, wlthind5), join_by(hhid), 
            relationship = "many-to-many") %>% 
  rename(v190 = wlthind5)
  

  #set filepaths for all files in used in the loop
  file_paths <- c(
    "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGBR21DT/NGBR21FL.dta",
    "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2003_DHS_04072025_2113_219655/NGBR4BDT/NGBR4BFL.dta",
    "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGBR53DT/NGBR53FL.DTA",
    # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2010_MIS_04072025_2114_219655/NGBR61DT/NGBR61FL.DTA",#MIS excluded as missing birth size data
    "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2013_DHS_04072025_2114_219655/NGBR6ADT/NGBR6AFL.DTA", 
    "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGBR7BDT/NGBR7BFL.DTA" #only this DHS uses "SHSTATE" notation
  )
  
  #begin the for loop
  
  for (i in seq_along(file_paths)) {
    
    #for ith iteration need to read "BR" instead of BRData 
    if(i == 1){
      BRdata <- BR
      } else{
        #forget objects used to calculate mortality each loop
        rm(BRdata, BRdata_CMORT, BRdata_CMORT1, BRdata_CMORT2, 
           BRdata_CMORT_wealth, resn1, resn2, resn3, resc, res_wealth,
           CHMORT, state_var)
    #read the household file
    BRdata <- read_dta(file_paths[i])
      }
    
    BRdata <- BRdata %>%
      mutate(child_sex = b4) %>%
      mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
      mutate(months_age = b3-v011) %>%
      mutate(mo_age_at_birth =
               case_when(
                 months_age < 20*12   ~ 1 ,
                 months_age >= 20*12 & months_age < 30*12 ~ 2,
                 months_age >= 30*12 & months_age < 40*12 ~ 3,
                 months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
      mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
      mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
      mutate(birth_order =
               case_when(
                 bord == 1  ~ 1,
                 bord >= 2 & bord <= 3 ~ 2,
                 bord >= 4 & bord <= 6 ~ 3,
                 bord >= 7  ~ 4,
                 bord == NA ~ 99)) %>%
      replace_with_na(replace = list(birth_order = c(99))) %>%
      mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
      mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
      mutate(prev_bint =
               case_when(
                 b11 <= 23 ~ 1,
                 b11 >= 24 & b11 <= 35 ~ 2,
                 b11 >= 36 & b11 <= 47 ~ 3,
                 b11 >= 48 ~ 4)) %>%
      mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))  %>%
      mutate(birth_size =
               case_when(
                 m18 >= 4 & m18 <= 5 ~ 1,
                 m18 <= 3 ~ 2,
                 m18 > 5 ~ 99)) %>%
      mutate(birth_size = set_label(birth_size, label = "Birth size")) 
    
    BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
    BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
    
    BRdata <- BRdata %>%
      mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
      mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
    
    ##################################################################################
    # MORTALITY RATES ################################################################
    ##################################################################################
    state_var <- if (i > 1) {
      "sstate"
    }
    else {
      NULL
    }
    
    BRdata_CMORT <- (BRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", 
                                "b3", "b7", "v106", "v190", "child_sex", "mo_age_at_birth", 
                                "birth_order", "prev_bint","birth_size", state_var)]) #account for deprecated naming conventions
    
    # NNMR, PNNMR, IMR, CMR & U5MR
    # TABLES 8.1, 8.2 and 8.3
    
    # Different datasets for period ends: 5-9 and 10-14
    BRdata_CMORT1 <- BRdata_CMORT
    BRdata_CMORT2 <- BRdata_CMORT
    BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * (5)
    BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * (10)
    
    resn1 <- as.data.frame(chmort(BRdata_CMORT))
    resn1$period <- "0-4"
    
    resn2 <- as.data.frame(chmort(BRdata_CMORT1))
    resn2$period <- "5-9"
    resn3 <- as.data.frame(chmort(BRdata_CMORT2))
    resn3$period <- "10-14"
    
    resc <- as.data.frame(rbind(chmort(BRdata_CMORT, Class="v024",Period = 120),
                                chmort(BRdata_CMORT, Class="v025",Period = 120),
                                chmort(BRdata_CMORT, Class="v106",Period = 120),
                                chmort(BRdata_CMORT, Class="v190",Period = 120),
                                chmort(BRdata_CMORT, Class="child_sex",Period = 120),
                                chmort(BRdata_CMORT, Class="mo_age_at_birth",Period = 120),
                                chmort(BRdata_CMORT, Class="birth_order",Period = 120),
                                chmort(BRdata_CMORT, Class="prev_bint",Period = 120),
                                chmort(BRdata_CMORT, Class="birth_size",Period = 120)))
    
    resc$period <- "0-9"
    
    CHMORT <- vector("list", 4)
    CHMORT <- rbindlist(list(resn1,resn2,resn3,resc), fill = TRUE)
    
    CHMORT[["Class"]] <- ifelse(is.na(CHMORT[["Class"]]), "National", CHMORT[["Class"]])
    CHMORT <- CHMORT[CHMORT[["Class"]]!="missing",] 
    
    # adjustment for row names
    num_rows <- nrow(CHMORT)
    num_repeats <- ceiling(num_rows / length(row.names(resn1)))  # Calculate how many times to repeat
    rownames(CHMORT) <- paste(seq(1:num_rows), rep(row.names(resn1), num_repeats)[1:num_rows])
    
    write.xlsx(CHMORT, paste0("Tables_child_mort_", 0 + i, ".xlsx"), asTable = TRUE, append = TRUE, overwrite = FALSE)
    
    ########################################################################################
    
    #Convert the wealth index to a factor
    BRdata <- BRdata %>%
      mutate(wealth_index = as_factor(v190))  # Convert to factor directly
    
    #Create a subset of the data for mortality calculations
    BRdata_CMORT_wealth <- BRdata %>%
      select(v021, v022, v024, v025, v005, v008, v011, 
             b3, b7, v106, v190, child_sex, mo_age_at_birth, 
             birth_order, prev_bint, birth_size, wealth_index) %>%
      #Remove rows with NA values in key columns
      filter(!is.na(wealth_index) & !is.na(v021) & !is.na(v022) & !is.na(v024) & !is.na(v025))
    
    # Calculate child mortality rates by wealth index
    res_wealth <- as.data.frame(chmort(BRdata_CMORT_wealth, Class = "wealth_index", Period = 120))
    
    # Export to Excel
    write.xlsx(res_wealth, paste0("Tables_child_mort_by_wealth", 0 + i, ".xlsx"), asTable = T, append = T, overwrite = F)
   
     ########################################################################################
    #write chmort tables by state (only works 2003 onwards, different var naming)

    # Proceed with the logic that requires state_var
    if (!is.null(state_var)) {
      # Ensure that state_var is a valid column name in BRdata
      if (state_var %in% names(BRdata)) {
        BRdata <- BRdata %>%
          mutate(state = as_factor(get(state_var)))  # Use get() to dynamically reference the variable
      } else {
        warning(paste("Column", state_var, "does not exist in BRdata."))
      }
      
      # Check state is factor
      BRdata_CMORT_state <- BRdata %>% 
        select(v021, v022, v024, v025, v005, v008, v011, # Subset
               b3, b7, v106, v190, child_sex, mo_age_at_birth, 
               birth_order, prev_bint, birth_size, state) %>%
        filter(!is.na(state) & !is.na(v021) & !is.na(v022) & !is.na(v024) & !is.na(v025)) # Remove NA
      
      res_state <- as.data.frame(chmort(BRdata_CMORT_state, Class = "state", Period = 120)) # Create chmort table by state
      
      write.xlsx(res_state, paste0("Tables_child_mort_by_state", 0 + i, ".xlsx"), asTable = TRUE, append = TRUE, overwrite = FALSE) # Write to Excel
    }
    else {
      # If state_var is NULL, skip to the next iteration
      warning("state_var is NULL. Skipping iteration.")
      next  # Skip to the next iteration of the loop
    }
    beep(0) #play a random beep after each iteration
  }
