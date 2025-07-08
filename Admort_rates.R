# ******************************************************************************
# Program: 			  AM_rates.R
# Purpose: 		    Produce adult and maternal mortality rates   
# Data inputs: 		IR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified:  December 09 2021 by Mahmoud Elkasabi
# ******************************************************************************
# the following indicators are produced ========================================
# Adult mortality - women & men ------------------------------------------------
# ASMR "Age Specific Mortality Rate" (ASMR)"
# AAMR "Age Adjusted Mortality Rate"
# q_15_to_50	(aka 15q30)	"probability of dying between ages 15 and 50"

# Pregnancy related - women-----------------------------------------------------
# ASPRMR "Age Specific Pregnancy Related Mortality Rate"
# AAPRMR "Age Adjusted Pregnancy Related Mortality Rate" 
# PRMRatio "Pregnancy Related Mortality Ratio"
# PMDF_PRMR	"proportions pregnancy related among deaths of females of reproductive age"
# prLTR "lifetime risk of pregnancy-related death"

# Maternal - women--------------------------------------------------------------
# ASMMR "Age Specific Maternal Mortality Rate" 
# AAMMR "Age Adjusted Maternal Mortality Rate"
# MMRatio "Maternal Mortality Ratio"
# PMDF_MMR	"proportions maternal among deaths of females of reproductive age"
# mLTR	"lifetime risk of maternal death"
# ----------------------------------------------------------------------------*/

## IMPORTANT: NEED TO RUN THIS BEFORE RUNNING ANYTHING IN THIS FILE
library(dplyr)
library(stringr)
library(DHS.rates)
library(haven)
library(survey)
library(openxlsx)

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
  
  # Reconstruct the function with the same arguments as the original
  # Get the arguments from the original function
  args <- paste(names(formals(fun)), collapse = ", ")
  fun_def <- sprintf("function(%s) {\n%s\n}", args, paste(fun_lines, collapse = "\n"))
  new_fun <- eval(parse(text = fun_def))
  
  # Assign the new function to the environment with the same name as the indicator 
  assign(indicator, new_fun, envir = .GlobalEnv)
}



IRdata_AMORT <- IRdata

TFR7 <- as.data.frame(fert(IRdata_AMORT,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]

##################################################################################
# ADULT MORTALITY RATES ##########################################################
##################################################################################

# Note: this part produces pregnancy-related death: mm9 is 2, 3, 5, or 6, and mm16 is not used

ADMORT <- vector("list", 5)

varlist <- c("asmr", "aamr", "asprmr", "aaprmr", "prmr")

for (i in seq_along(varlist)) {
  
  ADMORT[[i]]  <- as.data.frame(simplified_admort_func(IRdata_AMORT,Indicator=varlist[i]))
  
}

worksheet_names <- c("ASMR", "AAMR", "ASPRMR", "PMDF_PRMR", "AAPRMR", 
                     "PRMRatio", "q_15_to_50", "prLTR", "ASMMR", "PMDF_MMR", "AAMMR", 
                     "MMRatio", "mLTR")
rm(admort_wb)
admort_wb <- createWorkbook()
for(i in seq_along(worksheet_names)){
  ith_name <- worksheet_names[[i]]
  addWorksheet(admort_wb, sheetName = ith_name)
  
  if(i < 4) {
    writeData(admort_wb, sheet = ith_name, x = ADMORT[[i]])
  }
  
  if(i == 4){
    writeData(admort_wb, sheet = ith_name, x = ADMORT[[3]][,1:2])
  }
  if(i > 4) {
    writeData(admort_wb, sheet = ith_name, x = ADMORT[[i - 1]])
  }
  if(i > 5){break}
}

saveWorkbook(admort_wb, "ADMORT_results.xlsx")

 #need to fix the line below, sheetName can only be length of 1 
addWorksheet(admort_wb, sheetName = "ASMR", "AAMR", "ASPRMR", "PMDF_PRMR", "AAPRMR", 
             "PRMRatio", "q_15_to_50", "prLTR", "ASMMR", "PMDF_MMR", "AAMMR", 
             "MMRatio", "mLTR")
writeData(admort_wb, sheet = "ASMR",    x = ADMORT[[1]])
writeData(admort_wb, sheet = "AAMR",    x = ADMORT[[2]])
writeData(admort_wb, sheet = "ASPRMR",  x = ADMORT[[3]])  # pregnancy-related death
writeData(admort_wb, sheet = "PMDF_PRMR", x = ADMORT[[3]][,1:2])  # pregnancy-related death
writeData(admort_wb, sheet = "AAPRMR",  x = ADMORT[[4]])  # pregnancy-related death
writeData(admort_wb, sheet = "PRMRatio", x = ADMORT[[5]])  # pregnancy-related death

saveWorkbook(admort_wb, "ADMORT.xlsx")


# probability of dying between ages 15 and 50 ##################################
mx <- c(ADMORT[[1]]$ASMR[1:7]/1000,ADMORT[[1]]$ASMR[8:14]/1000)
q5 <- 5*mx / (1+2.4*mx)
q_15 <- c(1000*(1- (prod(1 -q5[1:7], na.rm = FALSE))),1000*(1- (prod(1 -q5[8:14], na.rm = FALSE))))

RESULTSq <- matrix(0, nrow = 1, ncol = 2)
dimnames(RESULTSq) <- list(NULL, c("q_15_to_50_women", "q_15_to_50_men") )
RESULTSq[1, ] <- c(as.numeric(q_15[1]), as.numeric(q_15[2]))

write.xlsx(RESULTSq, admort_wb, sheetName = "q_15_to_50",append=TRUE)


# lifetime risk of pregnancy-related death #####################################
prLTR <- 1 - (1- ADMORT[[5]][,2]/100000)^TFR7[1]

RESULTS <- matrix(0, nrow = 1, ncol = 3)
dimnames(RESULTS) <- list(NULL, c("TFR", "PRMRatio", "prLTR") )
RESULTS[1, ] <- c(as.numeric(TFR7), as.numeric(ADMORT[[5]][,2]), as.numeric(prLTR))

write.xlsx(RESULTS, admort_wb, sheetName = "prLTR",append=TRUE)


################################################################################
#maternal death needs same treatment as previous for debugging
# maternal death ###############################################################
# Note: this part produces maternal death: mm9 is 2, 3, or 5,    and mm16=0 
# check if mm16: exist
if ("TRUE" %in% (!("mm16_01" %in% names(IRdata_AMORT))))
  IRdata_AMORT [[paste("mm16_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata_AMORT$mm16_01)))
{ check <- 0} else { check <- 1}

if (check==1) {
  
  ADMORT2 <- vector("list", 3)
  
  varlist <- c("asmmr", "aammr", "mmr")
  
  for (i in seq_along(varlist)) {
    
    ADMORT2[[i]]  <- as.data.frame(simplified_admort_func(IRdata_AMORT,Indicator=varlist[i]))
    
  }
  
  write.xlsx(ADMORT2[[1]], admort_wb, sheetName = "ASMMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[1]][,1:2], admort_wb, sheetName = "PMDF_MMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[2]], admort_wb, sheetName = "AAMMR",append=TRUE) # maternal death
  write.xlsx(ADMORT2[[3]], admort_wb, sheetName = "MMRatio",append=TRUE) # maternal death
  
  # lifetime risk of maternal death
  mLTR <- 1 - (1- ADMORT2[[3]][,2]/100000)^TFR7[1]
  
  RESULTS <- matrix(0, nrow = 1, ncol = 3)
  dimnames(RESULTS) <- list(NULL, c("TFR", "MMRatio", "mLTR") )
  RESULTS[1, ] <- c(as.numeric(TFR7), as.numeric(ADMORT2[[3]][,2]), as.numeric(mLTR))
  
  write.xlsx(RESULTS, admort_wb, sheetName = "mLTR",append=TRUE)
  
}