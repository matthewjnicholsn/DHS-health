rm(list=ls())
lapply(c("dplyr","survey","haven"),require,character.only=T)

source("get_file_function.R")
countries <- c("Nigeria", "Ethiopia", "DRC", "Ghana", "Kenya")

years <- list(c(1990,2003,2008,2013,2018),c(2000,2005,2011,2016,2019),c(2007,2013.5),
              c(1993,1998,2003,2008,2014,2022),c(2003,2008.5,2014,2022))

br_file_list <- vector('list',length(countries))

wi_file_list <- vector('list',length(countries))

for(i in seq_along(countries)){
  br_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "BR") 
  wi_file_list[[i]] <- get_file(countries = countries[[i]], years = c(years[[i]]), surveys = "WI") 
}


model_list <- vector('list',length(countries))


br_ng_90 <- readRDS(br_file_list[[1]][1])
wi_ng_90 <- readRDS(wi_file_list[[1]][1]) |> 
    rename(hhid = whhid,wealth = wlthind5,wealth_factor =wlthindf)
br_ng_90 <- br_ng_90 |> 
  zap_label() |> 
  mutate(hhid = substr(caseid,1,12)) |> 
  left_join(wi_ng_90, by = 'hhid', relationship = 'many-to-one') |>  
  select(b5,b4,bord,wealth,v106,v108,v025,v024,m18,b11,v012,v005,v021) |> 
  rename(birthint = b11,
        mat_age = v012,
        child_alive = b5) |> 
  mutate(child_male = ifelse(b4 == 1,1,0),
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
        small_at_birth = ifelse(m18 > 3, 1, 0),
        v005 = v005/1000000,
        no_ed = ifelse(v106 == 0, 1, 0),
        primary_ed = ifelse(v106 ==1,1,0),
        secondary_ed = ifelse(v106 == 2,1,0),
        higher_ed = ifelse(v106 == 3,1,0)
        )

survey_design <- svydesign(data = br_ng_90, ids = ~v021, weights = ~v005) 

ng_90_glm <- svyglm(child_alive ~ child_male + small_at_birth + first_born + sec_born + third_bord + fourth_born +
  fifth_born + later_than_fifth_born + poor + rural + south + illiterate + mat_age, design = survey_design, 
family = stats::quasibinomial, na.action = na.exclude)
summary(ng_90_glm)

ng_90_glm_2 <- svyglm(child_alive ~ child_male + small_at_birth + first_born + sec_born + third_bord + fourth_born +
  fifth_born + later_than_fifth_born + poor + rural + south + illiterate + mat_age, design = survey_design, 
family = stats::binomial, na.action = na.exclude)

library(readr)

#cluster-level model - chmort
ng_90_chmort <- read_csv('/Users/matthewnicholson/DHS/chmortp_clust_Nigeria_1990_.csv') |> select(cluster,prob)
ng_90_clust <- readRDS(br_file_list[[1]][1])|> 
  zap_label() |> 
  mutate(hhid = substr(caseid,1,12)) |> 
  left_join(wi_ng_90, by = 'hhid', relationship = 'many-to-one') |>  
  select(v001,v005,v023,v021,wealth,v024,v013,v012,v106) |> 
  rename(cluster = v001,
         weight = v005,
         region = v024,
         age_group = v013,
         age = v012,
         urban_rural = v023,
         PSU = v021
         ) |> 
  mutate(weight = weight/1000000) |> 
  group_by(cluster,region,age,urban_rural,PSU) |>
        summarize(
          weighted_wealth = sum(weight * wealth, na.rm = TRUE) / 
                            sum(weight[!is.na(weight)], na.rm = TRUE),
          unweighted_n = n(),
          total_weight = sum(weight, na.rm = TRUE),
          age = median(age),
          .groups = "drop") |> 
  left_join(ng_90_chmort,by = 'cluster') |> 
  mutate(urban = ifelse(urban_rural == 1,1,0),
         rural = ifelse(urban_rural == 2,1,0),
         southeast = ifelse(region == 1,1,0),
         southwest = ifelse(region == 2,1,0),
         northwest = ifelse(region == 3,1,0),
         northeast = ifelse(region == 4,1,0),
         wealthy = ifelse(weighted_wealth >= 4,1,0),
         poor = ifelse(weighted_wealth < 4,1,0),
        prob = na_if(prob,0))



# #test some different imputations
# ng_90_clust_imp <- tibble(
#   original = ng_90_clust$prob,
#   imputed_pmm = complete(mice(ng_90_clust, method = "pmm"))$prob,
#   imputed_cart = complete(mice(ng_90_clust, method = "cart"))$prob,
#   imputed_lasso = complete(mice(ng_90_clust, method = "lasso.norm"))$prob
# )
# hist(ng_90_clust_imp$imputed_pmm)
# hist(ng_90_clist$imputed_cart)

#pmm is closest to original distribution

ng_90_clust <- ng_90_clust |> 
  mutate(prob = complete(mice(ng_90_clust, method = "pmm"))$prob)

library(mice)
md.pattern(ng_90_clust)


chmort_clust_svy <- svydesign(data = ng_90_clust, ids = ~PSU, weights = ~total_weight)
options(survey.lonely.psu = "adjust")

chmort_glm <- svyglm(prob ~ rural + poor + northeast + northwest + southeast, design = chmort_clust_svy, family = "Gamma")
summary(chmort_glm)

plot(chmort_glm)
library(car)
car::vif(chmort_glm)
mcfadden_r <- 1 - (chmort_glm$deviance/chmort_glm$null.deviance)


