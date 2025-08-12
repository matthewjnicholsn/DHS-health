rm(list = ls())
#list of libraries to require
lib_list <- c("haven", "dplyr", "stringr", "survey", "beepr")
#apply require to list 
lapply(lib_list, require, character.only = T)
#list of files to apply mortality function to
file_list <- c("/Users/matthewnicholson/DHS/DHS_surveys_rds_organized/DRC_DHS/2013-14/CDIR61DT/DRC_DHS_CD_2013-14_DHS_08072025_1920_219655_CDIR61DT_CDIR61FL.Rds"
)
#load the mortality function
source("/Users/matthewnicholson/DHS/admort_scripts/simplified_admort_func.R")
#define indicators and run debug function
indicator_list <- c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                    "aaprmr", "prmr", "mmr" 
                    # ,"aagfr"
)

source("/Users/matthewnicholson/DHS/patch_dhsrate_functions.R")
patch_dhsrate_functions(str_to_upper(indicator_list))


#source a few more indicators (ones not needing debug)
add_indicators <- c("AAGFR", "DataPrepareM_GFR", "fert")
for(i in add_indicators){
  assign(i, get(i, envir = asNamespace("DHS.rates")), envir = .GlobalEnv)
}

#emty list for results
all_results <- list()
#intiate loop
for(i in seq_along(file_list)){
  ir_data <- readRDS(file = file_list[i])
  # TFR7 <- as.data.frame(fert(ir_data, Indicator = "tfr", Period = 84, EverMW = "Yes",
  #                            AWFact = "awfactt"))[1]
  admort_results <- list()
  varlist <- c("asmr", "aamr" 
               # , "asprmr", "aaprmr", "prmr"
  )
  
  for(j in seq_along(varlist)){
    indicator = varlist[j]
    admort_results[[indicator]] <- as_tibble(simplified_admort_func(ir_data, 
                                                                    Indicator = indicator))
    
    # Create a unique filename for each indicator
    output_file <- paste0("admort_results_", basename(file_list[i]), "_", indicator, ".csv")
    write.csv(admort_results[[indicator]], file = output_file, row.names = FALSE)
  }
  
  # Store results for the current file in the all_results list
  all_results[[basename(file_list[i])]] <- admort_results
}
beep()