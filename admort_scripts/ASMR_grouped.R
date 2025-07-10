
ASMR_grouped <- function(Data.Name, group_var = NULL, CL = NULL, PeriodEnd = NULL, Period = NULL) {
  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))
  if (is.null(CL)) {
    Z <- stats::qnorm(0.025, lower.tail = FALSE)
  } else {å
    Z <- stats::qnorm((100 - CL)/200, lower.tail = FALSE)
  }
  if (is.null(Period)) {
    Periodmsg = 84
  } else {
    Periodmsg = Period
  }
  if (is.null(PeriodEnd)) {
    PeriodEndy_ <- as.integer((mean(Data.Name$v008) - 1)/12) + 1900
    PeriodEndm_ <- round(mean(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12), 0)
    PeriodEndm_m <- round(min(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12), 0)
    PeriodEndm_x <- round(max(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12), 0)
  } else {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))
    if (PeriodEndm_ >= round(mean(Data.Name$v008) - (((as.integer((mean(Data.Name$v008) - 1)/12) + 1900) - 1900) * 12), 0) & PeriodEndy_ >= 
        as.integer((mean(Data.Name$v008) - 1)/12) + 1900) 
      message(crayon::bold("Note:", "\n", "You specified a reference period that ends after the survey fieldwork dates....."), 
              "\n", "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", 
              "\n", "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", 
              "\n", "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")
  }
  if (is.null(PeriodEnd)) {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"), 
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), 
        "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), 
        crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12, digits = 2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), 
        "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), 
        crayon::red$bold$underline(PeriodEndy_), "\n", crayon::white$bold$bgBlue("The average reference period is"), 
        crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12) - (Periodmsg/24), digits = 2)), "\n")
  } else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"), 
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), 
        "\n", crayon::white$bold$bgBlue("The reference period ended in"), 
        crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12, digits = 2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), 
        crayon::red$bold$underline(PeriodEndy_), "\n", crayon::white$bold$bgBlue("The average reference period is"), 
        crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12) - (Periodmsg/24), digits = 2)), "\n")
  }
  Data.Name$allwoment = 1
  DeathEx <- DataPrepareGrpM(Data.Name, PeriodEnd, Period, group_var)
  options(survey.lonely.psu = "adjust")
  DeathEx <- subset(DeathEx, !is.na(v021) & !is.na(v022) & !is.na(rweight))
  View(DeathEx)
  
  # flexible grouping
  group_formula <- as.formula(paste("~", paste(group_var, collapse = "+")))
  
  dstrat <- survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = DeathEx)
  
  # Weighted exposure (denominator) and deaths (numerator)
  WN <- (survey::svyby(~exposure, by = group_formula, design = dstrat, survey::svytotal))$exposure
  ratio <- survey::svyby(~death, by = group_formula, denominator = ~exposure, design = dstrat, survey::svyratio)
  
  # asmr is always the last result column before SE
  asmr_col <- which(names(ratio) == "death")
  se_col <- which(names(ratio) == "se")
  asmr <- ratio[, asmr_col]
  SE <- ratio[, se_col]
  deaths <- WN * (asmr/1000)
  
  # N is total exposure by group
  agg <- stats::aggregate(DeathEx$exposure, DeathEx[, group_var, drop = FALSE], sum)
  N <- agg$x
  
  # Compute confidence intervals
  LCI = asmr - (Z * SE)
  LCI[LCI <= 0] = 0
  UCI <- asmr + (Z * SE)
  
  # Output
  group_cols <- as.data.frame(ratio[, group_var, drop = FALSE])
  
  RESULTS <- cbind.data.frame(
    group_cols,
    Deaths = round(deaths, 0),
    Exposure_years = round(WN, 0),
    ASMR = round(asmr, 3),
    SE = round(SE, 3),
    N = round(N, 0),
    LCI = round(LCI, 3),
    UCI = round(UCI, 3),
    row.names = NULL
  )
  return(RESULTS)
}
