## This a-script is a gonna be one a spicy meatball! (it will be a loop that runs mort calcs on every cycle of Ng DHS)
## load libraries
rm(list = ls())
package_list <- c("haven", "dplyr", "tidyr", "stringr", "survey", "openxlsx")
lapply(package_list, require, character.only = T)

#load in the function scripts
source("/Users/matthewnicholson/DHS/admort_scripts/simplified_admort_func.R")
source("/Users/matthewnicholson/DHS/admort_scripts/debug_helper_funs_loop.R")
source("/Users/matthewnicholson/DHS/admort_scripts/DataPrepareGrpM_func.R")
AAGFR <- DHS.rates:::AAGFR
DataPrepareM_GFR <- DHS.rates:::DataPrepareM_GFR
#blank list for results
admort_results <- list()
#file list
file_list <- c("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGIR21DT/NGIR21FL.DTA", 
               "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2003_DHS_04072025_2113_219655/NGIR4BDT/NGIR4BFL.DTA", 
               "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGBR53DT/NGBR53FL.DTA", 
               "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2013_DHS_04072025_2114_219655/NGIR6ADT/NGIR6AFL.DTA", 
               "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGIR7BDT/NGIR7BFL.DTA")
#create the results list
admort_results <- vector("list", 5)
#list indicators
varlist <- c("asmr", "aamr", "asprmr", "aaprmr", "prmr")



for(i in seq_along(file_list)){
  ir_data_file <- file_list[i]
  ir_data <- read_dta(ir_data_file) 
  
  TFR7 <- as.data.frame(fert(ir_data,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]
  #runs admort
  for (i in seq_along(varlist)) {
    
    admort_results[[i]]  <- as.data.frame(simplified_admort_func(ir_data,Indicator=varlist[i]))
    
  }
  
  admort_results[[basename(ir_data_file)]] <- admort_results 
  admort_results_final <- do.call(rbind, admort_results)
  
  }

