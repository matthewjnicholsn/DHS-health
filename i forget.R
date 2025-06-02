rm(list = ls(all = TRUE))
library(haven)
library(dplyr)
library(openxlsx)
library(naniar)#for replace_with_na fx
library(sjlabelled) #for fx set_label
library(tidyr)

#laod WI data for 1990 DHS
WI <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGWI21DT/NGWI21FL.DTA")
WI <- WI %>% 
  select(-wlthindf) %>% 
  rename(whhid, hhid = whhid) %>% 
  mutate(hhid = as.numeric(gsub(" ", "", hhid)))

BR <- read_dta("/Users/matthewnicholson/DHS/Nigeria\ DHS\'s/NG_1990_DHS_04072025_2113_219655/NGBR21DT/NGBR21FL.dta")
BR <- BR %>% 
  mutate(gsub('.{2}$', '', BR$caseid)) %>% 
  rename(`gsub(".{2}$", "", BR$caseid)`, hhid = `gsub(".{2}$", "", BR$caseid)`) %>% 
  mutate(hhid = as.numeric(gsub(" ", "", hhid))) %>% 
  left_join(WI, by = "hhid") %>% 
  rename(v190 = wlthind5) %>% 
  
BR |> left_join(WI |> distinct(hhid, wlthind5), join_by(hhid), relationship = "many-to-many")

print(BR$v190)


 mean(BR$v190
      )
mean(WI$v190) 
mean(WI$wlthind5)
weighted.mean(BR$v190, BR$v005, na.rm=T)
