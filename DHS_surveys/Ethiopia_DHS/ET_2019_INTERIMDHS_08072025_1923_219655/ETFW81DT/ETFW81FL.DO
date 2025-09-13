infix using "C:\COUNTRIES\ET81\ETFW81FL.dct"

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
label variable fw113a   "Amarigna"
label variable fw113b   "Tigrigna"
label variable fw113c   "Oromifa"
label variable fw113d   "Afarigna"
label variable fw113e   "Somaligna"
label variable fw113f   "Walyta"
label variable fw113g   "English"
label variable fw113h   "Language H"
label variable fw113i   "Language I"
label variable fw113j   "Language J"
label variable fw113k   "Language K"
label variable fw113l   "Language L"
label variable fw113m   "Language M"
label variable fw113x   "Other"
label variable fw113y   "No other language"
label variable fw114    "Fieldworker mother's tongue/native language"
label variable fw115    "Have fieldworker ever worked on a DHS"
label variable fw116    "Have fieldworker ever worked on any other survey"
label variable fw117    "Was fieldworker working for agency at the time employed for DHS"
label variable fw118    "Is fieldworker permanent or te temporary employed by agency"

#delimit ;
label define FW102   
     1 "TIGRAY"
     2 "AFAR"
     3 "AMHARA"
     4 "OROMIA"
     5 "SOMALI"
     6 "BENISHANGUL-GUMUZ"
     7 "SNNPR"
    12 "GAMBELA"
    13 "HARARI"
    14 "ADDIS ABABA"
    15 "DIRE DAWA"
    16 "Special enumeration area"
    96 "Outside of Ethiopia"
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
     2 "Secondary"
     3 "Technical/ vocational"
     4 "Higher"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Orthodox"
     2 "Catholic"
     3 "Protestant"
     4 "Muslin"
     5 "Traditional"
    95 "No religion"
    96 "Other"
;
label define FW112   
     1 "Amhara"
     2 "Oromo"
     3 "Tigrie"
     4 "Affar"
     5 "Somalie"
     6 "Guragie"
     7 "Sidama"
     8 "Nuwer"
     9 "Welaita"
    10 "Berta"
    11 "Kefficho"
    12 "Gumuz"
    13 "Hadiya"
    14 "Gamo"
    15 "Silte"
    16 "Anyiwak"
    17 "Kembata"
    18 "Gedeo"
    19 "Agew-Awi"
    96 "OTHER"
;
label define FW114   
     1 "Amarigna"
     2 "Trigrigna"
     3 "Oromigna"
     4 "Afarigna"
     5 "Somaligna"
     6 "Walytta"
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
     1 "Yes, CSA"
     2 "Yes, EPHI"
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
