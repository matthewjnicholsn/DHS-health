infile using "C:\USERS\21429\ICF\DATA ARCHIVE - DOCUMENTS\EXPORT MENU\KE8A\KEFW8AFL.dct"

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
label variable fw113b   "Kiswahili"
label variable fw113c   "Borana"
label variable fw113d   "Embu"
label variable fw113e   "Kalenjin"
label variable fw113f   "Kamba"
label variable fw113g   "Kikuyu"
label variable fw113h   "Kisii"
label variable fw113i   "Luhya"
label variable fw113j   "Maragoli"
label variable fw113k   "Luo"
label variable fw113l   "Maasai"
label variable fw113m   "Meru"
label variable fw113x   "Other language"
label variable fw113y   "No other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115a   "Has fieldworker ever worked on a DHS prior to this survey"
label variable fw115b   "Has fieldworker ever worked on an MIS prior to this survey"
label variable fw115c   "Has fieldworker ever worked on any other survey prior to this survey"
label variable fw116    "Was fieldworker working for implementing agency at the time employed for DHS"
label variable fw117    "Is fieldworker a permanent or temporary employee of agency"
label variable fw118    "Does fieldworker has any comments"

#delimit ;
label define FW102   
     1 "Mombasa"
     2 "Kwale"
     3 "Kilifi"
     4 "Tana River"
     5 "Lamu"
     6 "Taita Taveta"
     7 "Garissa"
     8 "Wajir"
     9 "Mandera"
    10 "Marsabit"
    11 "Isiolo"
    12 "Meru"
    13 "Tharaka-Nithi"
    14 "Embu"
    15 "Kitui"
    16 "Machakos"
    17 "Makueni"
    18 "Nyandarua"
    19 "Nyeri"
    20 "Kirinyaga"
    21 "Murang'a"
    22 "Kiambu"
    23 "Turkana"
    24 "West Pokot"
    25 "Samburu"
    26 "Trans Nzoia"
    27 "Uasin Gishu"
    28 "Elgeyo-Marakwet"
    29 "Nandi"
    30 "Baringo"
    31 "Laikipia"
    32 "Nakuru"
    33 "Narok"
    34 "Kajiado"
    35 "Kericho"
    36 "Bomet"
    37 "Kakamega"
    38 "Vihiga"
    39 "Bungoma"
    40 "Busia"
    41 "Siaya"
    42 "Kisumu"
    43 "Homa Bay"
    44 "Migori"
    45 "Kisii"
    46 "Nyamira"
    47 "Nairobi"
;
label define FW103   
     1 "City (Nairobi, Kisumu, Mombasa, Nakuru) ."
     2 "Town"
     3 "Rural area"
;
label define FW104   
    99 "Missing"
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
label define FW107   
    99 "Missing"
;
label define FW108   
     1 "Yes"
     2 "No"
;
label define FW109   
     1 "Primary"
     2 "Secondary/'A' level"
     3 "Middle level college (certificate/diploma)"
     4 "University"
     5 "Vocational"
     6 "Informal education (madrasa/adult basic)"
;
label define FW110   
     0 "Less than one year"
;
label define FW111   
     1 "Catholic"
     2 "Protestant"
     3 "Evangelical churches"
     4 "African instituted churches"
     5 "Orthodox"
     6 "Other Christian"
     7 "Islam"
     8 "Hindu"
     9 "Traditionists"
    95 "No religion"
    96 "Other"
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
    10 "Mijikenda/Swahili"
    11 "Somali"
    12 "Taita/Taveta"
    96 "Other"
;
label define FW114   
     1 "English"
     2 "Kiswahili"
     3 "Borana"
     4 "Embu"
     5 "Kalenjin"
     6 "Kamba"
     7 "Kikuyu"
     8 "Kisii"
     9 "Luhya"
    10 "Maragoli"
    11 "Luo"
    12 "Maasai"
    13 "Meru"
    14 "Mijkenda"
    15 "Pokot"
    16 "Somali"
    17 "Turkana"
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
     1 "Yes, KNBS"
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
label values fw112    FW112   
label values fw114    FW114   
label values fw115a   FW115A  
label values fw115b   FW115B  
label values fw115c   FW115C  
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
