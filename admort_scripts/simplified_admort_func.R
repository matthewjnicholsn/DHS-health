DataPrepareM <- function(Dat, PeriodEnd = NULL, Period = NULL) {
  # Helper as previously
  build_full_df <- function(dat, col_names) {
    full_list <- lapply(col_names, function(col) {
      if (col %in% names(dat)) dat[[col]] else rep(NA, nrow(dat))
    })
    names(full_list) <- col_names
    as.data.frame(full_list, stringsAsFactors = FALSE)
  }
  
  Dat$rweight <- Dat$v005 / 1e6
  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }
  
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight")
  myvars <- c(
    myvarsid,
    paste("mmidx_0", 1:9, sep = ""),
    paste("mmidx_", 10:20, sep = ""),
    paste("mm1_0", 1:9, sep = ""),
    paste("mm1_", 10:20, sep = ""),
    paste("mm2_0", 1:9, sep = ""),
    paste("mm2_", 10:20, sep = ""),
    paste("mm4_0", 1:9, sep = ""),
    paste("mm4_", 10:20, sep = ""),
    paste("mm8_0", 1:9, sep = ""),
    paste("mm8_", 10:20, sep = ""),
    paste("mm9_0", 1:9, sep = ""),
    paste("mm9_", 10:20, sep = ""),
    paste("mm12_0", 1:9, sep = ""),
    paste("mm12_", 10:20, sep = ""),
    paste("mm16_0", 1:9, sep = ""),
    paste("mm16_", 10:20, sep = "")
  )
  
  Dat <- build_full_df(Dat, myvars)
  def <- stats::reshape(
    Dat,
    direction = "long",
    varying = list(
      mmidx = 7:26,
      mm1 = 27:46,
      mm2 = 47:66,
      mm4 = 67:86,
      mm8 = 87:106,
      mm9 = 107:126,
      mm12 = 127:146,
      mm16 = 147:166
    ),
    v.names = c("mmidx", "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16"),
    timevar = "alt"
  )
  
  # Robust filtering: only filter if column isn't all NA
  for (colname in c("mmidx", "mm1", "mm2")) {
    if (!all(is.na(def[[colname]]))) {
      def <- def[stats::complete.cases(def[[colname]]), ]
    }
  }
  # Only filter if column isn't all NA
  if (!all(is.na(def$mm1)) & !all(is.na(def$mm2))) {
    def <- def[!def$mm1 %in% c(8, 9) & !def$mm2 %in% c(8, 9), ]
  }
  
  # Now check if def has rows before assigning
  if (nrow(def) == 0) return(def)
  
  if (is.null(PeriodEnd)) {
    def$periodend <- def$v008
  } else {
    def$periodend <- PeriodEndcmc
  }
  if (is.null(Period)) {
    def$period <- 84
  } else {
    def$period <- Period
  }
  
  def$upplim <- ifelse(def$mm2 == 0, def$mm8, def$v008 - 1)
  def$lowlim <- def$v008 - def$period
  def$exposure <- ifelse(def$upplim - def$lowlim + 1 < 0, 0, def$upplim - def$lowlim + 1)
  def$agegrp1 <- as.integer((def$upplim - def$mm4) / 60)
  def$expo1 <- ifelse(def$exposure < def$upplim - (def$mm4 + def$agegrp1 * 60) + 1,
                      def$exposure, def$upplim - (def$mm4 + def$agegrp1 * 60) + 1)
  def$death1 <- ifelse(def$mm2 == 0 & def$expo1 > 0, 1, 0)
  def$exposure <- def$exposure - def$expo1
  def$agegrp2 <- def$agegrp1 - 1
  def$expo2 <- ifelse(def$exposure < 60, def$exposure, 60)
  def$death2 <- 0
  def$exposure <- def$exposure - def$expo2
  def$agegrp3 <- def$agegrp2 - 1
  def$expo3 <- def$exposure
  def$death3 <- 0
  
  myvarsid <- c(
    "ID", "v021", "v005", "v008", "v022", "rweight",
    "mmidx", "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16"
  )
  myvars <- c(
    myvarsid,
    paste("agegrp", 1:3, sep = ""),
    paste("expo", 1:3, sep = ""),
    paste("death", 1:3, sep = "")
  )
  
  def <- build_full_df(def, myvars)
  
  rdef <- stats::reshape(
    def,
    direction = "long",
    varying = list(
      agegrp = 15:17,
      expo = 18:20,
      death = 21:23
    ),
    v.names = c("agegrp", "expo", "death"),
    timevar = "alt"
  )
  
  DeathEx <- rdef[rdef$agegrp >= 3 & rdef$agegrp <= 9, ]
  if (nrow(DeathEx) == 0) return(DeathEx)
  DeathEx$sex <- 2 - DeathEx$mm1
  DeathEx$exposure <- DeathEx$expo / 12
  DeathEx$death <- DeathEx$death * 1000
  DeathEx$id <- as.factor(DeathEx$v021)
  DeathEx <- DeathEx %>% 
    filter(exposure > 0)
  return(DeathEx)
}

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

