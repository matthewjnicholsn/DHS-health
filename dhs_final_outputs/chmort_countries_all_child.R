chmortp_prepare <- function(BRdata){
  BRdata <- BRdata %>%
    mutate(child_sex = b4) %>%
    mutate(child_sex = set_label(child_sex, label = "Sex of child"))  %>%
    mutate(months_age = b3-v011) %>%
    mutate(mo_age_at_birth =
            case_when(
              months_age < 20*12   ~ 1 ,
              months_age >= 20*12 & months_age < 30*12 ~ 2,
              months_age >= 30*12 & months_age < 40*12 ~ 3,
              months_age >= 40*12 & months_age < 50*12 ~ 4)) %>%
    mutate(mo_age_at_birth = factor(mo_age_at_birth, levels = c(1,2,3,4), labels = c("Mother's age at birth < 20", "Mother's age at birth 20-29", "Mother's age at birth 30-39","Mother's age at birth 40-49"))) %>%
    mutate(mo_age_at_birth = set_label(mo_age_at_birth, label = "Mother's age at birth")) %>%
    mutate(birth_order =
            case_when(
              bord == 1  ~ 1,
              bord >= 2 & bord <= 3 ~ 2,
              bord >= 4 & bord <= 6 ~ 3,
              bord >= 7  ~ 4,
              bord == NA ~ 99)) %>%
    mutate(birth_order = factor(birth_order, levels = c(1,2,3,4), labels = c("Birth order:1", "Birth order:2-3", "Birth order:4-6","Birth order:7+"))) %>%
    mutate(birth_order = set_label(birth_order, label = "Birth order"))  %>%
    mutate(prev_bint =
            case_when(
              b11 <= 23 ~ 1,
              b11 >= 24 & b11 <= 35 ~ 2,
              b11 >= 36 & b11 <= 47 ~ 3,
              b11 >= 48 ~ 4)) %>%
    mutate(prev_bint = set_label(prev_bint, label = "Preceding birth interval"))  %>%
    mutate(birth_size =
            case_when(
              m18 >= 4 & m18 <= 5 ~ 1,
              m18 <= 3 ~ 2,
              m18 > 5 ~ 99)) %>%
    mutate(birth_size = set_label(birth_size, label = "Birth size"))   
  BRdata[["prev_bint"]] <- ifelse(is.na(BRdata[["prev_bint"]]), 999, BRdata[["prev_bint"]])
  BRdata[["birth_size"]] <- ifelse(is.na(BRdata[["birth_size"]]), 999, BRdata[["birth_size"]])
  BRdata <- BRdata %>%
    mutate(prev_bint = factor(prev_bint, levels = c(1,2,3,4,999), labels = c("Previous birth interval <2 years", "Previous birth interval 2 years", "Previous birth interval 3 years","Previous birth interval 4+ years", "missing"))) %>%
    mutate(birth_size = factor(birth_size, levels = c(1,2,99,999), labels = c("Birth size: Small/very small","Birth size: Average or larger", "Birth size: Don't know/missing", "missing" )))
return(BRdata)
            }  
##error handling
tryCatch(
  expr = {

  },
  error = function(e){
    message('An Error occurred')
    message(w)
  },
  warning = function(w){
    message("There is a warning")
    message(w)
  },
  finally = {
    #do tis at the end before quittign the trCatch
  }  
)
#start the j loop over years
for(j in seq_along(years[[i]]){
    BRdata <- readRDS(br_file_list[[i,j]])
    BRdata <- chmortp_prepare(BRdata)
    #prep for calculations
    BRdata_CMORT <- (BRdata[, c("v001", "v021", "v022","v024", "v025", "v005", "v008","v011", 
                                "b3", "b7", "v106", "child_sex", "mo_age_at_birth", "birth_order", "prev_bint","birth_size")]) # add v001 here for cluster level and rm v190

    #get chmortp for cluster data and aggregate prob by cluster
    res_clust <- as.data.frame(chmortp(BRdata_CMORT, Class = "v001", Period = 120)) |> 
      group_by(Class) |> 
      summarise(
        prob = sum(W.DEATHS) / sum(W.EXPOSURE),
        .groups = 'drop'
      ) |> 
      mutate(Class = as.numeric(Class)) |> 
      rename(cluster = Class)


    #save results to a list
    chmort_outputs_all[[i,j]] <- res_clust

    #CHANGE
    # bind the mortality results back to the data frames
    #this part also needs to follow the logic of one frame for each year for each country
    br_data_list <- list(vector, length = 5)
    for(i in seq_along(year_list)){
      br_data_list[[i]] <- readRDS(file_list[[i]]) |> 
        as_tibble() |> 
        rename(cluster = v001) |> 
        left_join(chmortp_clust_results[[i]], by = "cluster", relationship = "many-to-one")
    }

