library(haven)
library(fs)
library(stringr)

input_root <- "/Users/matthewnicholson/DHS/DHS_surveys"
output_root <- "/Users/matthewnicholson/DHS/DHS_surveys_rds"
dir_create(output_root)

# get .dta files
dta_files <- dir_ls(input_root, recurse = TRUE, glob = "*.DTA")

for (dta_file in dta_files) {
  # Split the path to extract components
  path_parts <- str_split(path_rel(dta_file, input_root), "/")[[1]]
  # Example: path_parts = c("Kenya_DHS", "KE_2022_DHS_08072025_1928_219655", "KEIR8CDT", "KEIR8CFL.DTA")
  
  if(length(path_parts) >= 4) {
    country <- path_parts[1]
    year_dir <- path_parts[2]
    survey_dir <- path_parts[3]
    dta_basename <- path_ext_remove(path_parts[4])
    
    # You can extract year from year_dir if needed, e.g.:
    # year <- str_extract(year_dir, "\\d{4}")
    
    # Create new file name for the RDS output
    out_filename <- paste(country, year_dir, survey_dir, dta_basename, sep = "_")
    out_file <- path(output_root, paste0(out_filename, ".Rds"))
    
    # Read and save
    df <- read_dta(dta_file)
    saveRDS(df, out_file)
    cat("Saved:", out_file, "\n")
  }
}


#oops now need to handle the outputs. Dumb!
input_root <- "/Users/matthewnicholson/DHS/DHS_surveys_rds"
output_root <- "/Users/matthewnicholson/DHS/DHS_surveys_rds_organized"
dir_create(output_root)

# List all Rds files in the flat output folder
rds_files <- dir_ls(input_root, regexp = "\\.Rds$", recurse = FALSE)

for (rds_file in rds_files) {
  # Remove the file extension and split by underscore
  base <- path_ext_remove(path_file(rds_file))
  parts <- str_split(base, "_")[[1]]
  
  country <- paste(parts[1:2], collapse = "_")
  year <- parts[4]
  survey <- parts[9]
  
  # Create the output directory structure
  out_dir <- path(output_root, country, year, survey)
  dir_create(out_dir, recurse = TRUE)
  
  # Copy the file
  file_copy(rds_file, path(out_dir, path_file(rds_file)), overwrite = TRUE)
  cat("Moved:", rds_file, "->", path(out_dir, path_file(rds_file)), "\n")
}