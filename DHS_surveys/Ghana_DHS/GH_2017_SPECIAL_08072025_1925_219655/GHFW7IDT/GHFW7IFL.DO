infix using "C:\COUNTRIES\GH7I\GHFW7IFL.dct"

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
label variable fw112    "Fieldworker ethnicity"
label variable fw113a   "Akan"
label variable fw113b   "Ga"
label variable fw113c   "Ewe"
label variable fw113d   "English"
label variable fw113e   "Guan"
label variable fw113f   "Mole-dagbani"
label variable fw113g   "Grusi"
label variable fw113h   "Gurma"
label variable fw113i   "Mande"
label variable fw113x   "Other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115    "Has fieldworker ever worked on a DHS"
label variable fw116    "Has fieldworker ever worked on any other survey"
label variable fw117    "Was fieldworker working for agency at the time employed for DHS"
label variable fw118    "Is fieldworker permanent or temporary employed of agency"

#delimit ;
label define FW102   
     1 "Western"
     2 "Central"
     3 "Greater accra"
     4 "Volta"
     5 "Eastern"
     6 "Ashanti"
     7 "Brong ahafo"
     8 "Northern"
     9 "Upper east"
    10 "Upper west"
;
label define FW103   
     1 "City"
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
     2 "Middle"
     3 "Jss/jhs"
     4 "Secondary"
     5 "Sss/shs"
     6 "Higher"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Catholic"
     2 "Anglican"
     3 "Methodist"
     4 "Presbyterian"
     5 "Pentecostal/charismatic"
     6 "Other christian"
     7 "Islam"
     8 "Traditional/spiritualist"
    95 "No religion"
    96 "Other"
;
label define FW112   
     1 "Akan"
     2 "Ga/dangme"
     3 "Ewe"
     4 "Guan"
     5 "Mole-dagbani"
     6 "Grusi"
     7 "Gurma"
     8 "Mande"
    96 "Other"
;
label define FW114   
     1 "Akan"
     2 "Ga"
     3 "Ewe"
     4 "Guan"
     5 "Mole-dagbani"
     6 "Grusi"
     7 "Gurma"
     8 "Mande"
    96 "Other"
;
label define FW115   
     1 "Yes"
     2 "No"
;
label define FW116   
     1 "Yes"
     2 "No"
;
label define FW117   
     1 "Yes, GHS"
     2 "Yes, GSS"
     3 "No"
;
label define FW118   
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
label values fw112    FW112   
label values fw114    FW114   
label values fw115    FW115   
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
