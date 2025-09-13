rm(list = ls())
library(haven)
library(openxlsx)
library(DHS.rates)
library(survey)


IRdata <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGIR7BDT/NGIR7BFL.DTA")


IRdata_AMORT <- IRdata

TFR7 <- as.data.frame(fert(IRdata_AMORT,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]

#then source admort func

#then run through helper funs debug script
AAGFR <- DHS.rates:::AAGFR
DataPrepareM_GFR <- DHS.rates:::DataPrepareM_GFR

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
  
  # find the line starting with 'dstrat'
  dstrat_line <- grep("^\\s*dstrat", fun_lines)[1]
  
  # insert this line
  insert_line <- "DeathEx <- subset(DeathEx, !is.na(v021) & !is.na(v022) & !is.na(rweight))"
  
  # insert before dstrat
  if (!is.na(dstrat_line)) {
    fun_lines <- append(fun_lines, insert_line, after = dstrat_line - 1)
  } else {
    warning(paste("No dstrat line found in", indicator))
  }
  
  # Reconstruct the function with the same arguments as the original
  # Get the arguments from the original function
  args <- paste(names(formals(fun)), collapse = ", ")
  fun_def <- sprintf("function(%s) {\n%s\n}", args, paste(fun_lines, collapse = "\n"))
  new_fun <- eval(parse(text = fun_def))
  
  # Assign the new function to the environment with the same name as the indicator 
  assign(indicator, new_fun, envir = .GlobalEnv)
}



IRdata_AMORT <- IRdata

#runs admort func
for (i in seq_along(varlist)) {
  
  ADMORT[[i]]  <- as.data.frame(simplified_admort_func(IRdata_AMORT,Indicator=varlist[i]))
  
}
