rm(list=ls())
lapply(c("dplyr","survey"),require,character.only=T)

source("get_file_function.R")
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")

years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

br_file_list <- list()

wi_file_list <- list()

model_list <- vector('list',length(countries))

for(i in seq_along(countries)){
  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]),  surveys = "BR")
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI")
}
#messed this chunk up, the first code block should check for v108, as that is the old name, the new name is v155 and it has the different scales
for(i in seq_along(countries)){
  message("processing ",countries[[i]])
  for(j in seq_along(years[[i]])){
    
    message(years[[i]][j])
    br <- readRDS(br_file_list[[i]][j])
    if(!"v190" %in% names(br) | !"v108" %in% names(br) | "v155" %in% names(br) == T){#for case where no wealth index AND different literacy var
      message('wealth index not present, reading wealth index file ',years[[i]][j])
      wi <- readRDS(wi_file_list[[i]][j]) |> 
        rename(hhid = whhid,wealth = wlthind5,wealth_factor =wlthindf)
       br <- readRDS(br_file_list[[i]][j]) |> 
      mutate(hhid = substr(caseid,1,12)) |> 
      left_join(wi, by = 'hhid', relationship = 'many-to-one') |>  
      select(b5,b4,bord,wealth,v155,v025,v024,v108,m18,b11,v012,v005,v021) |> 
      rename(education = v150,
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
            illiterate = ifelse(v155 == 0, 1, 0),
            small_at_birth = ifelse(m18 > 3, 1, 0),
            v005 = v005/1000000
            )

    survey_design <- svydesign(data = br, ids = ~v021, weights = ~v005)

    model_list[[i]][[j]] <- svyglm(child_alive ~ child_male + small_at_birth + first_born + sec_born + third_bord + fourth_born +
      fifth_born + later_than_fifth_born + poor + rural + south + illiterate + mat_age, design = survey_design, 
    family = stats::quasibinomial, na.action = na.exclude)
    }
    else{
    br <- readRDS(br_file_list[[i]][j]) |>  
      select(b5,b4,bord,v190,v107,v025,v024,v108,m18,b11,v012,v005,v021) |> 
      rename(education = v107,
            birthint = b11,
            mat_age = v012,
            wealth = v190) |> 
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

    survey_design <- svydesign(data = br, ids = ~v021, weights = ~v005)

    model_list[[i]][[j]] <- svyglm(child_alive ~ child_male + small_at_birth + first_born + sec_born + third_bord + fourth_born +
      fifth_born + later_than_fifth_born + poor + rural + south + illiterate + mat_age, design = survey_design, 
    family = stats::quasibinomial, na.action = na.exclude)
      
    }
    }
}
