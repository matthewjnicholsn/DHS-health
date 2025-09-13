infix using "C:\COUNTRIES\KE81\KEFW81FL.dct"

label variable fw101    "Fieldworker code"
label variable fw000    "Country code and phase"
label variable fw102    "Fieldworker region of residence"
label variable fw103    "Fieldworker type of place of residence"
label variable fw104    "Fieldworker age"
label variable fw105    "Fieldworker sex"
label variable fw106    "Fieldworker marital status"
label variable fw107    "Fieldworker number of living children"
label variable fw108    "Fieldworker ever had a child who died"
label variable fw109    "Highest level of school attended by fieldworker"
label variable fw110    "Highest grade/form/year at that level completed by fieldworker"
label variable fw111    "Fieldworker religion"
label variable fw113a   "Embu"
label variable fw113b   "Kalenjin"
label variable fw113c   "Kamba"
label variable fw113d   "Kikuyu"
label variable fw113e   "Kisii"
label variable fw113f   "Luhya"
label variable fw113g   "Luo"
label variable fw113h   "Maasi"
label variable fw113i   "Meru"
label variable fw113j   "Mijikenda/Swahili"
label variable fw113k   "Somali"
label variable fw113l   "Taita/Taveta"
label variable fw113x   "Other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115a   "Has fieldworker ever worked on a DHS"
label variable fw115b   "Has fieldworker ever worked on a MIS"
label variable fw115c   "Has fieldworker ever worked on any other survey"
label variable fw116    "Was fieldworker working for agency at the time employed for MIS"
label variable fw117    "Is fieldworker a permanent or temporary employee of DNMP or KNBS"

#delimit ;
label define FW102   
     1 "Coast"
     2 "North Eastern"
     3 "Eastern"
     4 "Central"
     5 "Rift Valley"
     6 "Western"
     7 "Nyanza"
     8 "Nairobi"
;
label define FW103   
     1 "CIty"
     2 "Town"
     3 "Rural"
;
label define FW105   
     1 "Male"
     2 "Female"
;
label define FW106   
     1 "Currently married"
     2 "Living with a man/woman"
     3 "Widowed"
     4 "Divorced"
     5 "Separated"
     6 "Never married or lived with a man/woman"
;
label define FW108   
     1 "Yes"
     2 "No"
;
label define FW109   
     1 "Primary"
     2 "Post-primary"
     3 "Secondary/'A' level"
     4 "College (middle level)"
     5 "University"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Roman Catholic"
     2 "Protestant/other Christian"
     3 "Muslim"
     4 "No religion"
    96 "Other"
;
label define FW114   
     1 "Embu"
     2 "Kalenjin"
     3 "Kamba"
     4 "Kikuyu"
     5 "Kisii"
     6 "Luhya"
     7 "Luo"
     8 "Maasi"
     9 "Meru"
    10 "Mijikenda/Swahili"
    11 "Somali"
    12 "Taita/Taveta"
    96 "Other"
;
label define FW115A  
     1 "Yes"
     2 "No"
;
label define FW115B  
     1 "Yes"
     2 "No"
;
label define FW115C  
     1 "Yes"
     2 "No"
;
label define FW116   
     1 "Yes, DNMP"
     2 "Yes, KNBS"
     3 "No"
;
label define FW117   
     1 "Permanent"
     2 "Temporary"
;

#delimit cr
label values fw102    FW102   
label values fw103    FW103   
label values fw105    FW105   
label values fw106    FW106   
label values fw108    FW108   
label values fw109    FW109   
label values fw110    FW110   
label values fw111    FW111   
label values fw114    FW114   
label values fw115a   FW115A  
label values fw115b   FW115B  
label values fw115c   FW115C  
label values fw116    FW116   
label values fw117    FW117   
