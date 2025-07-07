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

##calculate adult moratlity measures for 2018
rm(list = ls())
library(haven)
library(openxlsx)
library(DHS.rates)
library(survey)


IRdata <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_2018_DHS_04072025_2116_219655/NGIR7BDT/NGIR7BFL.DTA")


IRdata_AMORT <- IRdata

TFR7 <- as.data.frame(fert(IRdata_AMORT,Indicator="tfr", Period = 84, EverMW = "Yes", AWFact = "awfactt"))[1]

##################################################################################
# ADULT MORTALITY RATES ##########################################################
##################################################################################

# Note: this part produces pregnancy-related death: mm9 is 2, 3, 5, or 6, and mm16 is not used

ADMORT <- vector("list", 1)

varlist <- c("asmr")

# get the function admort 
# getAnywhere(admort)

# name our own admort function

admort <- function(IRdata_AMORT, Indicator, JK = NULL, CL = NULL, Strata = NULL, 
                    Cluster = NULL, Weight = NULL, Date_of_interview = NULL, 
                    PeriodEnd = NULL, Period = NULL) 
{
  if (!Indicator %in% c("asmr")) 
    stop("Please specify a valid adult mortality indicator, such as asmr, aamr, asmmr, aammr, asprmr, mmr, prmr or aagfr")
  if (!is.null(Strata)) {
    IRdata_AMORT$strata = IRdata_AMORT[[Strata]]
    IRdata_AMORT$v022 = NULL
    names(IRdata_AMORT)[names(IRdata_AMORT) == c("strata")] <- c("v022")
  }
  if (!is.null(Cluster)) {
    IRdata_AMORT$cluster = IRdata_AMORT[[Cluster]]
    IRdata_AMORT$v021 = NULL
    names(IRdata_AMORT)[names(IRdata_AMORT) == c("cluster")] <- c("v021")
  }
  if (!is.null(Weight)) {
    IRdata_AMORT$weight = IRdata_AMORT[[Weight]]
    IRdata_AMORT$v005 = NULL
    names(IRdata_AMORT)[names(IRdata_AMORT) == c("weight")] <- c("v005")
  }
  if (!is.null(Date_of_interview)) {
    IRdata_AMORT$DOI = IRdata_AMORT[[Date_of_interview]]
    IRdata_AMORT$v008 = NULL
    names(IRdata_AMORT)[names(IRdata_AMORT) == c("DOI")] <- c("v008")
  }
  if (!("v021" %in% names(IRdata_AMORT))) 
    stop({
      message("Error: v021/Primary-sampling-unit is missing")
    })
  if (!("v005" %in% names(IRdata_AMORT))) 
    stop({
      message("Error: v005/Sample-weight is missing")
    })
  if (!("v008" %in% names(IRdata_AMORT))) 
    stop({
      message("Error: v008/Date-of-Interview is missing")
    })
  if (!("v022" %in% names(IRdata_AMORT))) 
    stop({
      message("Error: v022/Sample-strata is missing")
    })
  else {
    if (("TRUE" %in% (!(paste("mm1_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm1_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm1_01:mm1_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm2_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm2_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm2_01:mm2_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm4_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm4_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm4_01:mm4_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm8_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm8_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm8_01:mm8_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm9_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm9_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm9_01:mm9_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm12_0", 1:9, sep = "") %in% 
                        names(IRdata_AMORT)))) | ("TRUE" %in% (!(paste("mm12_", 
                                                                    10:20, sep = "") %in% names(IRdata_AMORT))))) {
      warning("Siblings variables mm12_01:mm12_20 are not complete; the missing variables were created")
    }
  }
  for (i in 1:9) {
    if ("TRUE" %in% (!(paste("mm1_0", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm1_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm2_0", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm2_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm4_0", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm4_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm8_0", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm8_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm9_0", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm9_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm12_0", i, sep = "") %in% 
                       names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm12_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% 
                       names(IRdata_AMORT)))) 
      IRdata_AMORT$PRMMRT <- NA
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% 
                       names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm16_0", i, sep = "")]] <- NA
  }
  for (i in 10:20) {
    if ("TRUE" %in% (!(paste("mm1_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm1_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm2_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm2_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm4_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm4_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm8_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm8_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm9_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm9_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm12_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm12_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm16_", i, sep = "") %in% names(IRdata_AMORT)))) 
      IRdata_AMORT[[paste("mm16_", i, sep = "")]] <- NA
  }
  IRdata_AMORT <- as.data.frame(IRdata_AMORT)
  if (Indicator == "asmr") {
    getFromNamespace("ASMR", "DHS.rates")(IRdata_AMORT, CL, PeriodEnd, Period)[[1]]
  }
}

#define ASMR function to bypass the DHSrates function
ASMR <- function (IRdata_AMORT, CL = NULL, PeriodEnd = NULL, Period = NULL) 
{
  IRdata_AMORT <- IRdata_AMORT[!IRdata_AMORT$v005 == 0, ]
  IRdata_AMORT$ID <- seq.int(nrow(IRdata_AMORT))
  if (is.null(CL)) {
    Z <- stats::qnorm(0.025, lower.tail = FALSE)
  }
  else {
    Z <- stats::qnorm((100 - CL)/200, lower.tail = FALSE)
  }
  if (is.null(Period)) {
    Periodmsg = 84
  }
  else {
    Periodmsg = Period
  }
  if (is.null(PeriodEnd)) {
    PeriodEndy_ <- as.integer((mean(IRdata_AMORT$v008) - 1)/12) + 
      1900
    PeriodEndm_ <- round(mean(IRdata_AMORT$v008) - ((PeriodEndy_ - 
                                                    1900) * 12), 0)
    PeriodEndm_m <- round(min(IRdata_AMORT$v008) - ((PeriodEndy_ - 
                                                    1900) * 12), 0)
    PeriodEndm_x <- round(max(IRdata_AMORT$v008) - ((PeriodEndy_ - 
                                                    1900) * 12), 0)
  }
  else {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))
    if (PeriodEndm_ >= round(mean(IRdata_AMORT$v008) - (((as.integer((mean(IRdata_AMORT$v008) - 
                                                                   1)/12) + 1900) - 1900) * 12), 0) & PeriodEndy_ >= 
        as.integer((mean(IRdata_AMORT$v008) - 1)/12) + 1900) 
      message(crayon::bold("Note:", "\n", "You specified a reference period that ends after the survey fieldwork dates....."), 
              "\n", "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", 
              "\n", "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", 
              "\n", "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")
  }
  if (is.null(PeriodEnd)) {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"), 
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), 
        "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), 
        crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12, 
                                                       digits = 2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), 
        "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), 
        crayon::red$bold$underline(PeriodEndy_), "\n", crayon::white$bold$bgBlue("The average reference period is"), 
        crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12) - 
                                           (Periodmsg/24), digits = 2)), "\n")
  }
  else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"), 
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), 
        "\n", crayon::white$bold$bgBlue("The reference period ended in"), 
        crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12, 
                                                       digits = 2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), 
        crayon::red$bold$underline(PeriodEndy_), "\n", crayon::white$bold$bgBlue("The average reference period is"), 
        crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12) - 
                                           (Periodmsg/24), digits = 2)), "\n")
  }
  IRdata_AMORT$allwoment = 1
  DeathEx <- DataPrepareM(IRdata_AMORT, PeriodEnd, Period)
  options(survey.lonely.psu = "adjust")
  vars_needed <- c("v021", "v022", "rweight")
  for (v in vars_needed) {
    if (!(v %in% names(DeathEx))) stop(paste("Missing variable:", v))
  }
  
  DeathEx <- DeathEx[complete.cases(DeathEx[, vars_needed]), ]
  cat("Rows after NA removal: ", nrow(DeathEx), "\n")
  print(summary(DeathEx[, vars_needed]))
  
  dstrat <- survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = DeathEx)
  dstrat <- survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, 
                              data = DeathEx)
  AGE <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
           "45-49")
  SEX <- c(rep("FEMALES", 7), rep("MALES", 7))
  WN <- (survey::svyby(~exposure, by = ~agegrp + sex, design = dstrat, 
                       survey::svytotal))$exposure
  asmr <- (survey::svyby(~death, by = ~agegrp + sex, denominator = ~exposure, 
                         design = dstrat, survey::svyratio))[, 3]
  deaths <- WN * (asmr/1000)
  SE <- (survey::svyby(~death, by = ~agegrp + sex, denominator = ~exposure, 
                       design = dstrat, survey::svyratio))[, 4]
  N <- stats::aggregate(DeathEx$exposure, list(DeathEx$agegrp, 
                                               DeathEx$sex), sum)$x
  DEFT <- sqrt(survey::svyby(~death, by = ~agegrp + sex, denominator = ~exposure, 
                             design = dstrat, deff = "replace", survey::svyratio)$DEff)
  RSE <- survey::cv(survey::svyby(~death, by = ~agegrp + sex, 
                                  denominator = ~exposure, design = dstrat, survey::svyratio))
  LCI = asmr - (Z * SE)
  LCI[LCI <= 0] = 0
  UCI <- asmr + (Z * SE)
  RESULTS <- cbind.data.frame(AGE, SEX, round(deaths, 0), round(WN, 
                                                                0), round(asmr, 3), round(SE, 3), round(N, 0), round(DEFT, 
                                                                                                                     3), round(RSE, 3), round(LCI, 3), round(UCI, 3), row.names = NULL)
  names(RESULTS) <- c("AGE", "SEX", "Deaths", "Exposure_years", 
                      "ASMR", "SE", "N", "DEFT", "RSE", "LCI", "UCI")
  list(RESULTS)
}

for (i in seq_along(varlist)) {
  
  ADMORT[[i]]  <- as.data.frame(admort(IRdata_AMORT,Indicator=varlist[i]))
  
}

write.xlsx(ADMORT[[1]], "Tables_AM.xlsx", sheetName = "ASMR",append=TRUE)


# probability of dying between ages 15 and 50 ##################################
mx <- c(ADMORT[[1]]$ASMR[1:7]/1000,ADMORT[[1]]$ASMR[8:14]/1000)
q5 <- 5*mx / (1+2.4*mx)
q_15 <- c(1000*(1- (prod(1 -q5[1:7], na.rm = FALSE))),1000*(1- (prod(1 -q5[8:14], na.rm = FALSE))))

RESULTSq <- matrix(0, nrow = 1, ncol = 2)
dimnames(RESULTSq) <- list(NULL, c("q_15_to_50_women", "q_15_to_50_men") )
RESULTSq[1, ] <- c(as.numeric(q_15[1]), as.numeric(q_15[2]))

write.xlsx(RESULTSq, "Tables_AM.xlsx", sheetName = "q_15_to_50",append=TRUE)




