library(dplyr)
library(stringr)

# DataPrepareM_GFR <- DHS.rates:::DataPrepareM_GFR

indicator_list <- c("asmr", "aamr", "asmmr", "aammr", "asprmr", 
                    "aaprmr", "prmr", "mmr" 
                     ,"aagfr"
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
  insert_line <- "DeathEx <- subset(DeathEx, !is.na(id))" 
  # !is.na(v021) & !is.na(v022) & !is.na(rweight))"
  
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

AAGFR <- function (Data.Name, PeriodEnd = NULL, Period = NULL) 
{
  v013 <- rweight <- age5 <- birth <- exposure <- NULL
  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))
  Data.Name$id <- c(as.factor(Data.Name$v021))
  Data.Name$rweight = Data.Name$v005/1e+06
  BirthEx <- DataPrepareM_GFR(Data.Name, PeriodEnd, Period)
  options(dplyr.summarise.inform = FALSE)
  AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)
  ASFR <- (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(birth * 
                                                                  rweight)))$x/(dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(exposure * 
                                                                                                                                       rweight)))$x
  gfr <- ceiling(sum(ASFR[1:7] * AGEDIST))
  list(gfr)[[1]]
}

DataPrepareM <- function (Dat, PeriodEnd = NULL, Period = NULL) 
{
  Dat$rweight = Dat$v005/1e+06
  # if (!is.null(PeriodEnd)) {
  #   dates <- paste(PeriodEnd, "01", sep = "-")
  #   PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
  #   PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
  #   PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  # }
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight")
 
  myvars <- c(myvarsid, paste("mmidx_0", 1:9, sep = ""), paste("mmidx_", 
                                                               10:20, sep = ""), paste("mm1_0", 1:9, sep = ""), paste("mm1_", 
                                                                                                                      10:20, sep = ""), paste("mm2_0", 1:9, sep = ""), paste("mm2_", 
                                                                                                                                                                             10:20, sep = ""), paste("mm4_0", 1:9, sep = ""), paste("mm4_", 
                                                                                                                                                                                                                                    10:20, sep = ""), paste("mm8_0", 1:9, sep = ""), paste("mm8_", 
                                                                                                                                                                                                                                                                                           10:20, sep = ""), paste("mm9_0", 1:9, sep = ""), paste("mm9_", 
                                                                                                                                                                                                                                                                                                                                                  10:20, sep = ""), paste("mm12_0", 1:9, sep = ""), paste("mm12_", 
                                                                                                                                                                                                                                                                                                                                                                                                          10:20, sep = ""), paste("mm16_0", 1:9, sep = ""), paste("mm16_", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                             10:20, sep = ""))
  return(myvars)   
  str(Dat)
  Dat <- as_tibble(Dat[myvars])
  def <- stats::reshape(Dat, direction = "long", varying = list(mmidx = 7:26, 
                                                                mm1 = 27:46, mm2 = 47:66, mm4 = 67:86, mm8 = 87:106, 
                                                                mm9 = 107:126, mm12 = 127:146, mm16 = 147:166), v.names = c("mmidx", 
                                                                                                                            "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16"), 
                        timevar = "alt")
  def <- def[stats::complete.cases(def$mmidx), ]
  def <- def[stats::complete.cases(def$mm1), ]
  def <- def[stats::complete.cases(def$mm2), ]
  def <- def[!def$mm1 == 8 & !def$mm1 == 9 & !def$mm2 == 8 & 
               !def$mm2 == 9, ]
  if (is.null(PeriodEnd)) {
    def$periodend = def$v008
  }
  else {
    def$periodend = PeriodEndcmc
  }
  if (is.null(Period)) {
    def$period = 84
  }
  else {
    def$period = Period
  }
  def$upplim <- ifelse(def$mm2 == 0, def$mm8, def$v008 - 1)
  def$lowlim = def$v008 - def$period
  def$exposure <- ifelse(def$upplim - def$lowlim + 1 < 0, 
                         0, def$upplim - def$lowlim + 1)
  def$agegrp1 = as.integer((def$upplim - def$mm4)/60)
  def$expo1 = ifelse(def$exposure < def$upplim - (def$mm4 + 
                                                    def$agegrp1 * 60) + 1, def$exposure, def$upplim - (def$mm4 + 
                                                                                                         def$agegrp1 * 60) + 1)
  def$death1 = ifelse(def$mm2 == 0 & def$expo1 > 0, 1, 0)
  def$exposure = def$exposure - def$expo1
  def$agegrp2 = def$agegrp1 - 1
  def$expo2 = ifelse(def$exposure < 60, def$exposure, 60)
  def$death2 = 0
  def$exposure = def$exposure - def$expo2
  def$agegrp3 = def$agegrp2 - 1
  def$expo3 = def$exposure
  def$death3 = 0
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight", 
                "mmidx", "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", 
                "mm16")
  myvars <- c(myvarsid, paste("agegrp", 1:3, sep = ""), paste("expo", 
                                                              1:3, sep = ""), paste("death", 1:3, sep = ""))
  def <- as.data.frame(def[myvars])
  rdef <- stats::reshape(def, direction = "long", varying = list(agegrp = 15:17, 
                                                                 expo = 18:20, death = 21:23), v.names = c("agegrp", 
                                                                                                           "expo", "death"), timevar = "alt")
  DeathEx <- rdef[rdef$agegrp >= 3 & rdef$agegrp <= 9, ]
  DeathEx$sex = 2 - DeathEx$mm1
  DeathEx$exposure = DeathEx$expo/12
  DeathEx$death = DeathEx$death * 1000
  DeathEx$id <- c(as.factor(DeathEx$v021))
  return(DeathEx)
}


DataPrepareM_GFR <- function (Dat, PeriodEnd = NULL, Period = NULL) 
{
  Dat$rweight = Dat$v005/1e+06
  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }
  myvars <- c(paste("ID"), paste("v021"), paste("v005"), paste("v008"), 
              paste("v011"), paste("v022"), paste("rweight"), paste("b3_0", 
                                                                    1:9, sep = ""), paste("b3_", 10:20, sep = ""))
  def <- reshape::melt(as.data.frame(Dat[myvars]), id = c("ID", 
                                                          "v021", "v005", "v008", "v011", "v022", "rweight"))
  names(def)[names(def) == c("value")] <- c("B3")
  def$variable <- NULL
  if (is.null(PeriodEnd)) {
    def$periodend = def$v008
  }
  else {
    def$periodend = PeriodEndcmc
  }
  if (is.null(Period)) {
    def$period = 84
  }
  else {
    def$period = Period
  }
  def$age5 = as.integer((def$B3 - def$v011)/60) - 3
  def$birth <- 0
  def$birth[def$periodend - def$B3 >= 0 & def$periodend - def$B3 <= 
              def$period & def$age5 >= 0] <- 1
  def$B3 <- NULL
  def$exposure = 0
  def$exposureg = 0
  def <- def[stats::complete.cases(def$age5), ]
  def$birth <- ifelse(def$age5 < 0, 0, def$birth)
  newdata <- c("ID", "v021", "v005", "v008", "v011", "v022", 
               "rweight")
  def2 <- Dat[newdata]
  if (is.null(PeriodEnd)) {
    def2$periodend = def2$v008
  }
  else {
    def2$periodend = PeriodEndcmc
  }
  if (is.null(Period)) {
    def2$period = 84
  }
  else {
    def2$period = Period
  }
  def2$agem = def2$periodend - def2$v011 - 1
  def2$age5 = as.integer(def2$agem/60)
  def2$higexp = def2$agem - (def2$age5 * 60) + 1
  def2$higexp <- ifelse(def2$higexp >= def2$period, def2$period, 
                        def2$higexp)
  def2$age5 = def2$age5 - 3
  def2 <- def2[def2$age5 >= 0, ]
  def2$lowexp <- ifelse(def2$higexp < def2$period & def2$age5 >= 
                          1, def2$period - def2$higexp, 0)
  def2$birth = 0
  def2$agem <- NULL
  def2l <- def2
  def2$lowexp <- NULL
  def2l$higexp <- NULL
  names(def2)[names(def2) == c("higexp")] <- c("exposure")
  names(def2l)[names(def2l) == c("lowexp")] <- c("exposure")
  def2l$age5 = def2l$age5 - 1
  def3 <- rbind(def2, def2l)
  def3$exposure = def3$exposure/12
  def3$exposureg <- ifelse(def3$age5 == 6, 0, def3$exposure)
  def4 <- rbind(def, def3)
  def4$birth = def4$birth * 1000
  BirthEx <- merge(stats::aggregate(list(def4$birth, def4$exposure, 
                                         def4$exposureg), list(def4$ID, def4$v021, def4$v022, 
                                                               def4$age5), sum), stats::aggregate(def4$rweight, list(def4$ID), 
                                                                                                  mean), by = "Group.1")
  names(BirthEx) <- c("ID", "v021", "v022", "age5", "birth", 
                      "exposure", "exposureg", "rweight")
  BirthEx <- BirthEx[BirthEx$birth != 0 | BirthEx$exposure != 
                       0, ]
  BirthEx$id <- c(as.factor(BirthEx$v021))
  return(BirthEx)
}
