
DataPrepareM <- DHS.rates:::DataPrepareM
# 
# 


simplified_admort_func <- function(
    Data.Name, Indicator, JK = NULL, CL = NULL, Strata = NULL, 
    Cluster = NULL, Weight = NULL, Date_of_interview = NULL, 
    PeriodEnd = NULL, Period = NULL
) {
  # Check valid indicator
  valid_inds <- c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                  "aaprmr", "prmr", "mmr", "aagfr")
  if (!Indicator %in% valid_inds) 
    stop("Invalid indicator: choose from ", paste(valid_inds, collapse = ", "))
  
  # Rename columns if alternate names provided
  if (!is.null(Strata)) Data.Name$v022 <- Data.Name[[Strata]]
  if (!is.null(Cluster)) Data.Name$v021 <- Data.Name[[Cluster]]
  if (!is.null(Weight)) Data.Name$v005 <- Data.Name[[Weight]]
  if (!is.null(Date_of_interview)) Data.Name$v008 <- Data.Name[[Date_of_interview]]
  
  # Check for required columns
  required_cols <- c("v021", "v022", "v005", "v008")
  missing <- setdiff(required_cols, names(Data.Name))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  # Sibling variables: mm1, mm2, mm4, mm8, mm9, mm12, mm16
  sibling_vars <- c("mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16")
  for (var in sibling_vars) {
    for (i in 1:20) {
      colname <- if (i < 10) sprintf("%s_0%d", var, i) else sprintf("%s_%d", var, i)
      if (!colname %in% names(Data.Name)) Data.Name[[colname]] <- NA
    }
  }
  
  # Call the appropriate indicator function
  fun_list <- list(
    asmr = ASMR, aamr = AAMR, asmmr = ASMMR, aammr = AAMMR, 
    asprmr = ASPRMR, aaprmr = AAPRMR, prmr = PRMR, mmr = MMR, aagfr = AAGFR
  )
  args_list <- list(
    asmr = list(Data.Name, CL, PeriodEnd, Period),
    aamr = list(Data.Name, JK, CL, PeriodEnd, Period),
    asmmr = list(Data.Name, CL, PeriodEnd, Period),
    aammr = list(Data.Name, JK, CL, PeriodEnd, Period),
    asprmr = list(Data.Name, CL, PeriodEnd, Period),
    aaprmr = list(Data.Name, JK, CL, PeriodEnd, Period),
    prmr = list(Data.Name, JK, CL, PeriodEnd, Period),
    mmr = list(Data.Name, JK, CL, PeriodEnd, Period),
    aagfr = list(Data.Name, PeriodEnd, Period)
  )
  # Dispatch to the selected function
  result <- do.call(fun_list[[Indicator]], args_list[[Indicator]])
  return(result[[1]])
}

