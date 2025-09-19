lib_list <- c("dplyr", "tidyverse", "conflicted","tidyr", "ggplot2", "pscl", "caret", "haven")
lapply(lib_list, require, character.only = T)
conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())
source("/Users/matthewnicholson/DHS/get_file_function.R")

ir <- readRDS(get_file("Kenya_DHS", 2003, "IR")) |> 
  rename(wealth = v190,
wealth_factor = v191,
age = v012,
water = v115,
toilet = v116,
has_elec = v119,
has_radio = v120,
has_tv = v121,
has_fridge = v122,
has_bike = v123,
has_moto = v124,
has_car = v125,
floor = v127,
roof = v129,
religion = v130,
ethnicity = v131,
education = v133,
residence_defacto = v134,
res_or_visitor = v135,
num_in_house = v136,
num_of_child = v137,
num_of_wom = v138,
region = v139,
residenc_dejure = v140,
relationship_to_head = v150,
sex_of_head = v151,
age_of_head = v152,
has_phone = v153,
literacy = v155,
partic_literacy_prog = v156,
freq_reading = v157,
freq_tv = v159,
toilet_shared = v160,
cooking_fuel = v161,
total_child_born = v201,
sons_at_home = v203,
daughters_at_home = v203,
sons_elsewhere = v204,
daughters_elseqhere = v204,
sons_dead = v206,
daughters_dead = v207,
births_last_five_yrs = v208,
births_last_yr = v209,
knowledge_of_ovul = v217,
num_of_living_child = v218,
marriage_to_birth_int = v221,
contra_knowledge = v301,
contra_use = v302,
current_contra_method = v312,
current_type_contra = v313,
yrs_since_steril = v319,
age_at_steril = v320,
rohrer = v446,
has_bug_net = v459,
smokes_cig = v463a,
smokes_pipe = v463b,
smokes_other = v463c,
num_of_cigs_today = v464,
current_marital = v501,
husband_lives_here = v504,
num_of_wives = v505,
age_at_first_marriage = v511,
age_at_first_sex = v525,
partner_occupation = v704,
resp_currently_working = v714,
resp_occupation = v716,
heard_std = v750,
heard_aids = v751,
avoid_aids = v753,
water_avail = s22a,
solar_pow = s25b,
rooms_slept_in = s25a,
state_of_repair = s27b,
dwelling_ownership = s28a,
dwelling_land_ownership = s28b,
waste_dispo = s28c,
district = sdist,
lang_of_resp = slangr) |> 
  select(wealth,wealth_factor,age,water,toilet,has_elec,has_radio,has_tv,has_fridge,has_bike,has_moto,has_car,floor,roof,religion,ethnicity,education,residence_defacto,res_or_visitor,num_in_house,num_of_child,num_of_wom,region,residenc_dejure,relationship_to_head,sex_of_head,age_of_head,has_phone,literacy,partic_literacy_prog,freq_reading,freq_tv,toilet_shared,cooking_fuel,total_child_born,daughters_at_home,daughters_elseqhere,sons_dead,daughters_dead,births_last_five_yrs,births_last_yr,knowledge_of_ovul,num_of_living_child,marriage_to_birth_int,contra_knowledge,contra_use,current_contra_method,current_type_contra,yrs_since_steril,age_at_steril,rohrer,has_bug_net,smokes_cig,smokes_pipe,smokes_other,num_of_cigs_today,current_marital,husband_lives_here,num_of_wives,age_at_first_marriage,age_at_first_sex,partner_occupation,resp_currently_working,resp_occupation,heard_std,heard_aids,avoid_aids,water_avail,solar_pow,rooms_slept_in,state_of_repair,dwelling_ownership,dwelling_land_ownership,waste_dispo,district,lang_of_resp) |> 
  mutate(across(where(~ !is.integer(.)), ~ as.integer(.))) 

#try a pca model
data <- ir
# 2. Remove columns with near zero variance
nzv <- nearZeroVar(data)
if(length(nzv) > 0) {
  data <- data[, -nzv, drop = FALSE]
}

# 3. Handle NA values:
#    - Impute columns with <10% NAs using median
#    - Drop columns with >=10% NAs

na_prop <- sapply(data, function(x) mean(is.na(x)))
cols_to_impute <- names(na_prop[na_prop > 0 & na_prop < 0.10])
cols_to_drop   <- names(na_prop[na_prop >= 0.10])

# Impute with median
for(col in cols_to_impute) {
  med <- median(data[[col]], na.rm = TRUE)
  data[[col]][is.na(data[[col]])] <- med
}

# Drop high-NA columns
if(length(cols_to_drop) > 0) {
  data <- data %>% select(-all_of(cols_to_drop))
}

# Now 'data' is ready for PCA
pca_results <- prcomp(data, scale. = TRUE)
screeplot(pca_results)

# Proportion of variance explained by each PC
explained_var <- pca_results$sdev^2 / sum(pca_results$sdev^2)
cumulative_var <- cumsum(explained_var)
print(explained_var)
plot(explained_var)
print(cumulative_var)
plot(cumulative_var)

# Loadings: contributions of each variable to each PC
loadings <- pca_results$rotation
# View loadings for PC1
print(loadings[, 1]) #This shows that highest educational attainment, 
#roof material, floor material, and birth control explained significant 
# #amounts of variance in the data

# Variables with highest (absolute) contribution to PC1
head(sort(abs(loadings[, 1]), decreasing = TRUE), 5)
 #v762bz    v762br    v762bf    v762bx    v762bo 
 #0.1344685 0.1341633 0.1341556 0.1341556 0.1341551 
 # This shows that highest contribution to PC1 are all female contraception related

# Scores: coordinates of your samples in PC space
scores <- pca_results$x
# Plot the first two PCs
plot(scores[, 1], scores[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA: Individuals in PC1 vs PC2 space")

biplot(pca_results, scale = 0)


# Number of top variables to show
top_n <- 10

# Get loadings for PC1 and PC2
loadings <- pca_results$rotation[, 1:2]
loading_magnitude <- apply(abs(loadings), 1, max) # Use max or sum(abs(...))
top_vars <- names(sort(loading_magnitude, decreasing = TRUE)[1:top_n])

# Use the 'factoextra' package for a clean biplot (install if needed)
if(!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(factoextra)

fviz_pca_biplot(
  pca_results,
  select.var = list(name = top_vars), # only top variables
  repel = TRUE, # better label spacing
  col.var = "red", 
  col.ind = "black"
)

# Plot only a random subset of 200 individuals
set.seed(42)
ind_sample <- sample(1:nrow(pca_results$x), 200)
fviz_pca_biplot(
  pca_results,
  select.ind = list(name = ind_sample),
  select.var = list(name = top_vars),
  repel = TRUE
)

#Plot only variables, no individuals
fviz_pca_var(pca_results, col.var = "red", repel = TRUE)

#problem: many age related variables are collinear. Need to control for this

plot(data$wealth_factor, data$rohrer)

data <- data |> 
  filter(rohrer < 9998)
t.test()