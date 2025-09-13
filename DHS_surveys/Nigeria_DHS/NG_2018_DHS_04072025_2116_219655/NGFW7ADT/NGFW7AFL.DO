infix using "C:\COUNTRIES\NG7A\NGFW7AFL.dct"

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
label variable fw113a   "English"
label variable fw113b   "Hausa"
label variable fw113c   "Yoruba"
label variable fw113d   "Igbo"
label variable fw113e   "Urhobo"
label variable fw113f   "Ibibio"
label variable fw113g   "Edo"
label variable fw113h   "Fulfulde"
label variable fw113i   "Kanuri"
label variable fw113x   "Other language"
label variable fw113y   "No other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115    "Has fieldworker ever worked on a DHS"
label variable fw116    "Has fieldworker ever worked on any other survey"
label variable fw117    "Was fieldworker working for NPC at the time employed for DHS"
label variable fw118    "Is fieldworker a permanent or temporary employee of NPC"

#delimit ;
label define FW102   
     1 "ABIA"
     2 "ADAMAWA"
     3 "AKWA IBOM"
     4 "ANAMBRA"
     5 "BAUCHI"
     6 "BAYELSA"
     7 "BENUE"
     8 "BORNO"
     9 "CROSS RIVER"
    10 "DELTA"
    11 "EBONYI"
    12 "EDO"
    13 "EKITI"
    14 "ENUGU"
    15 "FCT-ABUJA"
    16 "GOMBE"
    17 "IMO"
    18 "JIGAWA"
    19 "KADUNA"
    20 "KANO"
    21 "KATSINA"
    22 "KEBBI"
    23 "KOGI"
    24 "KWARA"
    25 "LAGOS"
    26 "NASARAWA"
    27 "NIGER"
    28 "OGUN"
    29 "ONDO"
    30 "OSUN"
    31 "OYO"
    32 "PLATEAU"
    33 "RIVERS"
    34 "SOKOTO"
    35 "TARABA"
    36 "YOBE"
    37 "ZAMFARA"
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
     2 "Secondary"
     3 "Higher"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Catholic"
     2 "Other Christian"
     3 "Islam"
     4 "Traditionalist"
     5 "No religion"
    96 "Other"
;
label define FW114   
     1 "English"
     2 "Hausa"
     3 "Yoruba"
     4 "Igbo"
     5 "Urhobo"
     6 "Ibibio"
     7 "Edo"
     8 "Fulfude"
     9 "Kanuri"
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
     1 "Yes"
     2 "No"
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
label values fw114    FW114   
label values fw115    FW115   
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
