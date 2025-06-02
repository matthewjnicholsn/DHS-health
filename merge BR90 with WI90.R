# Created houshold + cluster id from WI
BRdata90$whhid <- paste(BRdata90$v001, BRdata90$v002)
View(BRdata90$WIcaseid)
# merge the datasets with new caseid
merged_BRdata90I <- merge(BRdata90, BRData90WI, by = "whhid", all.x = TRUE)  # Use all.x = TRUE for a left join
head(BRdata90$WIcaseid)
