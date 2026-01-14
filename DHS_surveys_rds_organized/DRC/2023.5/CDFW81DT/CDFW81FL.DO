infile using "C:\USERS\21180\ICF\DATA ARCHIVE - DOCUMENTS\EXPORT MENU\CD81\CDFW81FL.dct"

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
label variable fw113a   "French"
label variable fw113b   "Kikongo"
label variable fw113c   "Lingala"
label variable fw113d   "Swahili"
label variable fw113e   "Tshiluba"
label variable fw113f   "Language F (not used)"
label variable fw113g   "Language G (not used)"
label variable fw113h   "Language H (not used)"
label variable fw113i   "Language I (not used)"
label variable fw113j   "Language J (not used)"
label variable fw113k   "Language K (not used)"
label variable fw113l   "Language L (not used)"
label variable fw113m   "Language M (not used)"
label variable fw113x   "Other language"
label variable fw113y   "No other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115a   "Has fieldworker ever worked on a DHS prior to this survey"
label variable fw115b   "Has fieldworker ever worked on an MICS prior to this survey"
label variable fw115c   "Has fieldworker ever worked on any other survey prior to this survey"
label variable fw116    "Was fieldworker working for implementing agency at the time employed for DHS"
label variable fw117    "Is fieldworker a permanent or temporary employee of agency"
label variable fw118    "Does fieldworker has any comments"

#delimit ;
label define FW102   
     1 "Kinshasa"
     2 "Kongo Central"
     3 "Kwango"
     4 "Kwilu"
     5 "Mai-Ndombe"
     6 "Equateur"
     7 "Nord Ubangi"
     8 "Sud Ubangi"
     9 "Mongala"
    10 "Tshuapa"
    11 "Tshopo"
    12 "Bas-Uele"
    13 "Haut-Uele"
    14 "Ituri"
    15 "Nord-Kivu"
    16 "Sud-Kivu"
    17 "Maniema"
    18 "Haut-Katanga"
    19 "Haut Lomami"
    20 "Lualaba"
    21 "Tanganyika"
    22 "Lomami"
    23 "Sankuru"
    24 "KasaÃ¯ Oriental"
    25 "KasaÃ¯"
    26 "KasaÃ¯ Central"
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
     3 "Higher"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Catholic"
     2 "Protestant"
     3 "Salvation Army"
     4 "Kimbanguiste"
     5 "Muslim"
     6 "Non denominational/Charismatic/Apostolic"
     7 "Jehovah Witness"
     8 "Animist/Traditional religion"
     9 "Branhamism"
    10 "No religion"
    96 "Other"
;
label define FW114   
     1 "French"
     2 "Kikongo"
     3 "Lingala"
     4 "Swahili"
     5 "Tshiluba"
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
     1 "Yes, implementing agency 1"
     2 "Yes, implementing agency 2"
     3 "No"
;
label define FW117   
     1 "Permanent"
     2 "Temporary"
;
label define FW118   
     1 "Yes"
     2 "No"
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
label values fw118    FW118   
