library(haven)
library(data.table)

##read in household 
HR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGHR21DT/NGHR21FL.DTA")
#read in wealth index
WI <-read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA")
#read in individual birth data
BR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGBR21DT/NGBR21FL.dta")
##Check variables
View(HR)
View(WI)
##change id name in WI to represent
names(WI) <- c("hhid")
##merge datasets
HRWI <- merge(HR, WI, by = "hhid", all.x = TRUE)

#fix name back so it matches BR
names(HRWI)[names(HRWI) == "hv002"] <- "v002"
#create subset with only var of interest
HRWI_sub <- HRWI[, c("v002", "NA.1")]

##convert to data tables for memory management
HRWI_subT <- setDT(HRWI_sub)
BR_T <- setDT(BR)
#This is where I  merge the HRWI subset with the BR dataset
BRmerge1 <- merge(HRWI_subT, BR_T, by =.EACHI, all.x = TRUE, allow.cartesian = TRUE)
View(BRmerge)
##This runs but returns a massive data.table

