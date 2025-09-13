infix using "C:\COUNTRIES\KE7A\KEFW7AFL.dct"

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
label variable fw113    "Fieldworker's mother's tongue/native language"
label variable fw114a   "Embu"
label variable fw114b   "English"
label variable fw114c   "Kalenjin"
label variable fw114d   "Kamba"
label variable fw114e   "Kikuyu"
label variable fw114f   "Kisii"
label variable fw114g   "Luhya"
label variable fw114h   "Luo"
label variable fw114i   "Maasai"
label variable fw114j   "Meru"
label variable fw114k   "Mijikenda"
label variable fw114l   "Somali"
label variable fw114m   "Taiti/Taveta"
label variable fw114n   "Borana"
label variable fw114o   "Maragoli"
label variable fw114p   "Pokot"
label variable fw114q   "Turkana"
label variable fw114r   "Swahili"
label variable fw114x   "Other language"
label variable fw114y   "No other language"
label variable fw115    "Has fieldworker ever worked on a DHS"
label variable fw116    "Has fieldworker ever worked on any other survey"
label variable fw117    "Was fieldworker working for agency at the time employed for DHS"
label variable fw118    "Is fieldworker permanently or temporarily employed by agency"

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
     9 "Missing"
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
     1 "Roman Catholic"
     2 "Protestant/Other Christian"
     3 "Muslim"
     4 "No religion"
     6 "Other"
;
label define FW112   
     1 "Embu"
     2 "Kalenjin"
     3 "Kamba"
     4 "Kikuyu"
     5 "Kisii"
     6 "Luhya"
     7 "Luo"
     8 "Maasai"
     9 "Meru"
    10 "Mijikenda"
    11 "Somali"
    12 "Taita/Taveta"
    13 "Borana"
    14 "Maragoli"
    15 "Pokot"
    16 "Turkana"
    17 "Swahili"
    96 "Other"
;
label define FW113   
     1 "Embu"
     2 "English"
     3 "Kalenjin"
     4 "Kamba"
     5 "Kikuyu"
     6 "Kisii"
     7 "Luhya"
     8 "Luo"
     9 "Maasai"
    10 "Meru"
    11 "Mijikenda"
    12 "Somali"
    13 "Taita/Taveta"
    14 "Borana"
    15 "Maragoli"
    16 "Pokot"
    17 "Turkana"
    18 "Swahili"
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
     1 "Yes, MCU"
     2 "Yes, KNBS"
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
label values fw113    FW113   
label values fw115    FW115   
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
