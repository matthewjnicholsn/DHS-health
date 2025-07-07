function (Data.Name, Indicator, JK = NULL, CL = NULL, Strata = NULL, 
          Cluster = NULL, Weight = NULL, Date_of_interview = NULL, 
          PeriodEnd = NULL, Period = NULL) 
{
  if (!Indicator %in% c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                        "aaprmr", "prmr", "mmr", "aagfr")) 
    stop("Please specify a valid adult mortality indicator, such as asmr, aamr, asmmr, aammr, asprmr, mmr, prmr or aagfr")
  if (!is.null(Strata)) {
    Data.Name$strata = Data.Name[[Strata]]
    Data.Name$v022 = NULL
    names(Data.Name)[names(Data.Name) == c("strata")] <- c("v022")
  }
  if (!is.null(Cluster)) {
    Data.Name$cluster = Data.Name[[Cluster]]
    Data.Name$v021 = NULL
    names(Data.Name)[names(Data.Name) == c("cluster")] <- c("v021")
  }
  if (!is.null(Weight)) {
    Data.Name$weight = Data.Name[[Weight]]
    Data.Name$v005 = NULL
    names(Data.Name)[names(Data.Name) == c("weight")] <- c("v005")
  }
  if (!is.null(Date_of_interview)) {
    Data.Name$DOI = Data.Name[[Date_of_interview]]
    Data.Name$v008 = NULL
    names(Data.Name)[names(Data.Name) == c("DOI")] <- c("v008")
  }
  if (!("v021" %in% names(Data.Name))) 
    stop({
      message("Error: v021/Primary-sampling-unit is missing")
    })
  if (!("v005" %in% names(Data.Name))) 
    stop({
      message("Error: v005/Sample-weight is missing")
    })
  if (!("v008" %in% names(Data.Name))) 
    stop({
      message("Error: v008/Date-of-Interview is missing")
    })
  if (!("v022" %in% names(Data.Name))) 
    stop({
      message("Error: v022/Sample-strata is missing")
    })
  else {
    if (("TRUE" %in% (!(paste("mm1_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm1_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm1_01:mm1_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm2_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm2_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm2_01:mm2_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm4_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm4_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm4_01:mm4_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm8_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm8_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm8_01:mm8_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm9_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm9_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm9_01:mm9_20 are not complete; the missing variables were created")
    }
    if (("TRUE" %in% (!(paste("mm12_0", 1:9, sep = "") %in% 
                        names(Data.Name)))) | ("TRUE" %in% (!(paste("mm12_", 
                                                                    10:20, sep = "") %in% names(Data.Name))))) {
      warning("Siblings variables mm12_01:mm12_20 are not complete; the missing variables were created")
    }
  }
  for (i in 1:9) {
    if ("TRUE" %in% (!(paste("mm1_0", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm1_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm2_0", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm2_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm4_0", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm4_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm8_0", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm8_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm9_0", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm9_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm12_0", i, sep = "") %in% 
                       names(Data.Name)))) 
      Data.Name[[paste("mm12_0", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% 
                       names(Data.Name)))) 
      Data.Name$PRMMRT <- NA
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% 
                       names(Data.Name)))) 
      Data.Name[[paste("mm16_0", i, sep = "")]] <- NA
  }
  for (i in 10:20) {
    if ("TRUE" %in% (!(paste("mm1_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm1_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm2_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm2_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm4_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm4_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm8_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm8_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm9_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm9_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm12_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm12_", i, sep = "")]] <- NA
    if ("TRUE" %in% (!(paste("mm16_", i, sep = "") %in% names(Data.Name)))) 
      Data.Name[[paste("mm16_", i, sep = "")]] <- NA
  }
  Data.Name <- as.data.frame(Data.Name)
  if (Indicator == "asmr") {
    ASMR(Data.Name, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "aamr") {
    AAMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "asmmr") {
    ASMMR(Data.Name, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "aammr") {
    AAMMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "asprmr") {
    ASPRMR(Data.Name, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "aaprmr") {
    AAPRMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "prmr") {
    PRMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "mmr") {
    MMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
  }
  else if (Indicator == "aagfr") {
    AAGFR(Data.Name, PeriodEnd, Period)[[1]]
  }
}