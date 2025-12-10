#function supposed to fnd object for survey design iteratively and work in country[[i]]{years[[i]][j]} loop

find_col_lab <- function(df,objects){
  df <- as_tibble(df)
  objects <- objects
  column_names <- colnames(df)
  for (col_name in column_names) {
    if(!objects[[col_name]] %in% c('ids','strata','data','weights')){
      message('object is not of accepted class')
    }
    if(objects[[col_name]] == 'ids'){
      objects[[col_name]] <- "primary sampling unit"
    }
    if(objects[[col_name]] == 'strata'){
      objects[[col_name]] <- 'sample stratum'
    }
    if(objects[[col_name]]== ' data'){
      message('object of class data is the data frame, you do not need this function')
    }
    if(objects[[col_name]] == 'weights' & attr(df[[4]], 'label') == "household number"){
      objects[[col_name]] <- 'sample weight'
      message('using individual sample weights')
    }
    if(objects[[col_name]] == 'weights' & attr(df[[4]], 'label') == 'cluster number'){
      objects[[col_name]] <- 'sample weight'
      message('using household sample weights')
    }
      column_label <- attr(df[[col_name]], "label")
      if (!is.null(column_label) && grepl(object, column_label, ignore.case = TRUE)) {
          strata_variable_name <- col_name
          message(paste0("Found 'sample stratum' in label for coded variable: ", col_name))
          break 
      }
  }
  if (is.null(strata_variable_name)) {
      message("No variable found with 'sample stratum' in its label attribute.")
  } else {
      message(paste0("The coded 'sample stratum' variable is: ", strata_variable_name))
  }
  return(objects)
}

find_col_name(df = br, objects = c('ids','weights'))

#test case
country <- "Nigeria"
year <- 1990
wi <- readRDS(get_file(countries = "Nigeria", years = 1990, surveys = "WI")) |> 
  rename(hhid = whhid,wealth = wlthind5,wealth_factor =wlthindf)
br <- readRDS(get_file(countries = "Nigeria", year = 1990, survey = "BR")) |> 
  mutate(hhid = substr(caseid,1,12)) |> 
  left_join(wi, by = 'hhid', relationship = 'many-to-one') |> 
  select(b5,b4,bord,wealth,v107,v025,v024,v108,m18,b11,v012,v005,v021) |> 
  rename(education = v107,
         birthint = b11,
         mat_age = v012) |> 
  mutate(child_alive = ifelse(b5 == 1,1,0),
         child_male = ifelse(b4 == 1,1,0),
         first_born = ifelse(bord == 1,1,0),
         sec_born = ifelse(bord == 2,1,0),
         third_bord = ifelse(bord == 3,1,0),
         fourth_born = ifelse(bord == 4,1,0),
         fifth_born = ifelse(bord == 5, 1, 0),
         later_than_fifth_born = ifelse(bord > 5,1,0),
         poor = ifelse(wealth <= 2, 1, 0),
         rural = ifelse(v025 == 2,1,0),
         south = ifelse(v024 == 1 | v024 == 2, 1, 0),
         illiterate = ifelse(v108 == 3, 1, 0),
         small_at_birth = ifelse(m18 > 3, 1, 0)
        )

ng_90_design <- svydesign(data = br, ids = ~v021, weights = ~v005)

ng_90_chmort_glm <- svyglm(child_alive ~ child_male + small_at_birth + first_born + sec_born + third_bord + fourth_born +
  fifth_born + later_than_fifth_born + poor + rural + south + illiterate + mat_age, design = ng_90_design, 
family = stats::quasibinomial, na.action = na.exclude)

summary(ng_90_chmort_glm)
attributes(ng_90_chmort_glm)

#desired use case
for(i in seq_along(countries)){
  for(j in seq_along(years[[i]])){
    br <- readRDS(br_file_list[[i]][j])
    svy_objects <- find_col_lab(df = br, objects = 'ids','weights','stratum')
    svy_design_list[[i]][[j]] <- svydesign(ids = svy_objects[[1]], strata = svy_objects[[3]], 
      weights = svy_objects[[2]], data = br)
      #possible vars for regression
    svy_glm_list[[i]][[j]] <- svyglm(b4 ~ poor + r1 + r2 + r3 + education + urbanrural + vax + birthweight + birthorder + childsex + maternalage + precedingbirthint, )
  }
}
