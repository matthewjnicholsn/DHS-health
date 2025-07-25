rm(list = ls())
#list of libraries to require
lib_list <- c("haven", "dplyr", "stringr", "survey")
#apply require to list 
lapply(lib_list, require, character.only = T)
#list of files to apply mortality function to
file_list <- c("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGIR21DT/NGIR21FL.DTA", 
                "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2003_DHS_04072025_2113_219655/NGIR4BDT/NGIR4BFL.DTA", 
                "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2008_DHS_04072025_2113_219655/NGBR53DT/NGBR53FL.DTA", 
                "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2013_DHS_04072025_2114_219655/NGIR6ADT/NGIR6AFL.DTA", 
                "/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGIR7BDT/NGIR7BFL.DTA")
#load the mortality function
source("/Users/matthewnicholson/DHS/admort_scripts/simplified_admort_func.R")
#define indicators and run debug function
indicator_list <- c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                    "aaprmr", "prmr", "mmr" 
                    # ,"aagfr"
) %>% str_to_upper()

for (i in seq_along(indicator_list)) {
  
  
  indicator <- indicator_list[[i]]
  # get the function for each indicator
  fun <- get(indicator, envir = asNamespace("DHS.rates"))
  
  # Convert function body to text
  fun_lines <- deparse(body(fun))
  
  # Find the line starting with 'dstrat'
  dstrat_line <- grep("^\\s*dstrat", fun_lines)[1]
  
  # The line to insert
  insert_line <- "DeathEx <- subset(DeathEx, !is.na(v021) & !is.na(v022) & !is.na(rweight))"
  
  # Insert your line before the dstrat line
  if (!is.na(dstrat_line)) {
    fun_lines <- append(fun_lines, insert_line, after = dstrat_line - 1)
  } else {
    warning(paste("No dstrat line found in", indicator))
  }
  # reconstruct the function with the same arguments as the original
  # get the arguments from the original function
  args <- paste(names(formals(fun)), collapse = ", ")
  fun_def <- sprintf("function(%s) {\n%s\n}", args, paste(fun_lines, collapse = "\n"))
  new_fun <- eval(parse(text = fun_def))
  
  # assign the new function to the environment with the same name as the indicator 
  assign(indicator, new_fun, envir = .GlobalEnv)
}
#source a few more indicators (ones not needing debug)
AAGFR <- DHS.rates:::AAGFR
DataPrepareM_GFR <- DHS.rates:::DataPrepareM_GFR

for(i in seq_along(file_list)){
  ir_data <- read_dta(file_list[i])
  TFR7 <- as.data.frame(fert(ir_data,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]
  
  admort_results <- vector("list", 5)
  varlist <- c("asmr", "aamr" 
               # , "asprmr", "aaprmr", "prmr"
               )
  for(i in seq_along(varlist)){
    admort_results[[i]] <- as_tibble(simplified_admort_func(ir_data, Indicator = varlist[[i]]))
    write.csv(admort_results[[i]], file = paste("admort_results", basename(file_list[[i]])))
  }
}