rm(list = ls())
#list of libraries to require
lib_list <- c("haven", "dplyr", "stringr", "survey", "beepr")
#apply require to list 
lapply(lib_list, require, character.only = T)
#list of files to apply mortality function to
file_list <- c(
  # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGIR21DT/NGIR21FL.DTA", 
  # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2003_DHS_04072025_2113_219655/NGIR4BDT/NGIR4BFL.DTA", 
  # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGBR53DT/NGBR53FL.DTA", 
  # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2013_DHS_04072025_2114_219655/NGIR6ADT/NGIR6AFL.DTA", 
  # "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGIR7BDT/NGIR7BFL.DTA",
  # '/Users/matthewnicholson/DHS/Ethiopia DHS/ET_2016_DHS_07292025_1528_219655/ETIR71DT/ETIR71FL.DTA',
  # '/Users/matthewnicholson/DHS/Ethiopia DHS/ET_2019_INTERIMDHS_07292025_1529_219655/ETIR81DT/ETIR81FL.DTA',
  '/Users/matthewnicholson/DHS/Kenya DHS/KE_2014_DHS_07292025_1529_219655/KEIR72DT/KEIR72FL.DTA',
  '/Users/matthewnicholson/DHS/Kenya DHS/KE_2022_DHS_07292025_1530_219655/KEIR8CDT/KEIR8CFL.DTA',
  '/Users/matthewnicholson/DHS/Ghana DHS/GH_2014_DHS_07292025_1527_219655/GHIR72DT/GHIR72FL.DTA',
  '/Users/matthewnicholson/DHS/Ghana DHS/GH_2022_DHS_07292025_1528_219655/GHIR8CDT/GHIR8CFL.DTA'
)
#load the mortality function
source("/Users/matthewnicholson/DHS/admort_scripts/simplified_admort_func.R")
#define indicators and run debug function
indicator_list <- c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                    "aaprmr", "prmr", "mmr" 
                    # ,"aagfr"
)

source("patch_dhsrate_functions.R")
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
  ir_data <- read_dta(file_list[i])
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