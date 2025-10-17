library(dplyr)
library(haven)
library(naniar)
library(sjlabelled)
library(DHS.rates)
library(data.table)  # For rbindlist and fwrite

source("get_file_function.R")

# Define the year list to include all desired years
year_list <- c(1990, 2003, 2008, 2013, 2018)

# Initialize lists for storing data and results
br_data <- list()
chmort_results <- list()

# Loop through years to process data
for (i in seq_along(year_list)) {
  # Get the file paths for the given year
  year <- year_list[i]
  message("Processing year: ", year)
  tryCatch({
    br_data[[i]] <- get_file(years = year, countries = "Nigeria_DHS", surveys = "BR")
    if (is.null(br_data[[i]]) || length(br_data[[i]]) == 0) {
      warning("No data returned for year ", year)
      next
    }
    
    # Read the data and perform transformations
    BRdata <- readRDS(br_data[[i]]) %>%
      mutate(child_sex = b4) %>%
      mutate(child_sex = set_label(child_sex, label = "Sex of child")) %>%
      mutate(months_age = b3 - v011) %>%
      mutate(mo_age_at_birth = case_when(
        months_age < 20 * 12 ~ 1,
        months_age >= 20 * 12 & months_age < 30 * 12 ~ 2,
        months_age >= 30 * 12 & months_age < 40 * 12 ~ 3,
        months_age >= 40 * 12 & months_age < 50 * 12 ~ 4
      )) %>%
      mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1, 2, 3, 4), labels = c(
        "Mother's age at birth < 20", "Mother's age at birth 20-29",
        "Mother's age at birth 30-39", "Mother's age at birth 40+"
      ))) %>%
      mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
      mutate(birth_order = case_when(
        bord == 1 ~ 1,
        bord >= 2 & bord <= 3 ~ 2,
        bord >= 4 & bord <= 6 ~ 3,
        bord >= 7 ~ 4,
        is.na(bord) ~ 99
      )) %>%
      replace_with_na(replace = list(birth_order = c(99))) %>%
      mutate(birth_order = factor(birth_order, levels = c(1, 2, 3, 4), labels = c(
        "Birth order:1", "Birth order:2-3", "Birth order:4-6", "Birth order:7+"
      ))) %>%
      mutate(birth_order = set_label(birth_order, label = "Birth order")) %>%
      mutate(prev_bint = case_when(
        b11 <= 23 ~ 1,
        b11 >= 24 & b11 <= 35 ~ 2,
        b11 >= 36 & b11 <= 47 ~ 3,
        b11 >= 48 ~ 4
      )) %>%
      mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval")) %>%
      mutate(birth_size = case_when(
        m18 >= 4 & m18 <= 5 ~ 1,
        m18 <= 3 ~ 2,
        m18 > 5 ~ 99
      )) %>%
      mutate(birth_size = set_label(birth_size, label = "Birth size"))

    # Select relevant columns for child mortality calculations
    BRdata_CMORT <- BRdata %>%
      select(
        v007, v021, v022, v024, v025, v005, v008, v011,
        b3, b7, v106, v190, child_sex, mo_age_at_birth, birth_order, prev_bint, birth_size
      )

    # Adjust datasets for child mortality calculations (period offsets)
    BRdata_CMORT1 <- BRdata_CMORT
    BRdata_CMORT2 <- BRdata_CMORT
    BRdata_CMORT1$v008 <- BRdata_CMORT$v008 - 12 * 5
    BRdata_CMORT2$v008 <- BRdata_CMORT$v008 - 12 * 10

# --- helper to tag chmort output with measures ---
measure_tags <- c("NNMR", "PNNMR", "IMR", "CMR", "U5MR")
measure_labels <- c(
  NNMR = "Neonatal mortality rate (0-1 month)",
  PNNMR = "Post-neonatal mortality rate (1-11 months)",
  IMR = "Infant mortality rate (<1 year)",
  CMR = "Child mortality rate (1-4 years)",
  U5MR = "Under-five mortality rate (0-59 months)"
)

tag_chmort <- function(df) {
  # if chmort already returns an 'Indicator' or 'Measure' column, prefer it
  if ("Indicator" %in% names(df)) {
    df$measure <- as.character(df$Indicator)
  } else if ("Measure" %in% names(df)) {
    df$measure <- as.character(df$Measure)
  } else if (nrow(df) == length(measure_tags)) {
    # assume standard order (NNMR, PNNMR, IMR, CMR, U5MR)
    df$measure <- measure_tags
  } else {
    # fallback: make an NA measure column and warn
    df$measure <- NA_character_
    warning("tag_chmort(): unexpected number of rows (", nrow(df),
            "); measure tags not assigned automatically.")
  }
  # add human-readable label where possible
  df$measure_label <- measure_labels[df$measure]
  # ensure character columns (avoid factors)
  df$measure <- as.character(df$measure)
  df$measure_label <- as.character(df$measure_label)
  return(df)
}

# --- compute national chmort and tag measures ---
resn1 <- as.data.frame(chmort(BRdata_CMORT))   # current / 0-4
resn1$period <- "0-4"
resn1 <- tag_chmort(resn1)

resn2 <- as.data.frame(chmort(BRdata_CMORT1))  # 5-9
resn2$period <- "5-9"
resn2 <- tag_chmort(resn2)

resn3 <- as.data.frame(chmort(BRdata_CMORT2))  # 10-14
resn3$period <- "10-14"
resn3 <- tag_chmort(resn3)

# Combine the three national-period results for this survey
CHMORT <- rbindlist(list(resn1, resn2, resn3), fill = TRUE)

# Force national label and remove any rows labelled "missing"
CHMORT[["Class"]] <- "National"
CHMORT <- CHMORT[CHMORT[["Class"]] != "missing", ]

# Tag with the survey year (the loop year) so we can combine across surveys
CHMORT$survey_year <- year

# store per-year results as before
chmort_results[[length(chmort_results) + 1]] <- CHMORT

  }, error = function(e) {
    warning("An error occurred while processing year ", year, ": ", e$message)
  })
}

# Combine all years and write a single CSV
if (length(chmort_results) > 0) {
  all_chmort <- rbindlist(chmort_results, fill = TRUE)
  # Optional: reorder columns so survey_year is first
  setcolorder(all_chmort, c("survey_year", setdiff(names(all_chmort), "survey_year")))
  fwrite(all_chmort, "Tables_child_mort_ng_national_1990_2018.csv")
  message("Wrote national-level child mortality for all years to Tables_child_mort_ng_national_1990_2018.csv")
} else {
  warning("No results collected; nothing to write.")
}