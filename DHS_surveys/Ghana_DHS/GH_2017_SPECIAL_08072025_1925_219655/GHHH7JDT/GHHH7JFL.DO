infix using "C:\COUNTRIES\GH7J\GHHH7JFL.dct"

label variable qhclust  "Cluster number"
label variable qhnumber "Household number"
label variable qhregion "Region"
label variable qhdistr  "District"
label variable qhintd   "Interview day"
label variable qhintm   "Interview month"
label variable qhinty   "Interview year"
label variable qhintnum "Interviewer number"
label variable qhresult "Result of household interview"
label variable qhvisits "Number of household visits"
label variable qhconsent "Consent agreement"
label variable qhmember "Total members in household"
label variable qhwomen  "Eligible women in household"
label variable qhresp   "Line of household respondent"
label variable qhsuperv "Supervisor number"
label variable qhlangq  "Language of questionnaire"
label variable qhlangi  "Language of interview"
label variable qhlangr  "Language of respondent"
label variable qhtrans  "Translator used"
label variable qh100h   "Hour interview begins"
label variable qh100m   "Minutes interview begins"
label variable qhbabies "Babies in the household"
label variable qhnofam  "Other non-family members in household"
label variable qhtemp   "Other temporal visitors in household"
label variable qhintc   "Date of household interview (CMC)"
label variable qhintcd  "Date of household interview Century Day Code (CDC)"
label variable qhweight "Household weight (6 decimals)"
label variable qhstrata "Sampling errors stratum"
label variable qhwlthf  "Wealth index factor (5 decimals)"
label variable qhwlthi  "Wealth index quintile"
label variable qhtype   "Type of place of residence"
label variable qhwltfur "Wealth index factor for urban/rural (5 decimals)"
label variable qhwltiur "Wealth index quintile for urban/rural"
label variable qh101    "Source of drinking water"
label variable qh102    "Source of water for cooking/handwashing"
label variable qh103    "Location of source for water"
label variable qh104    "Time to water and back (mins)"
label variable qh105    "Type of toilet facility"
label variable qh106    "Share facilities with other households"
label variable qh107    "Number of households sharing toilet"
label variable qh108    "Type of cooking fuel"
label variable qh109    "Number of rooms used for sleeping"
label variable qh110    "Livestock, herds or farm animals"
label variable qh111a   "Milk cows"
label variable qh111b   "Other cattle"
label variable qh111c   "Horses/donkeys/mules"
label variable qh111d   "Goats"
label variable qh111e   "Sheep"
label variable qh111f   "Chickens or other poultry"
label variable qh111g   "Pigs"
label variable qh111h   "Rabbits"
label variable qh111i   "Grasscutter"
label variable qh112    "Own land usable for agriculture"
label variable qh113u   "Area for agricultural land (unit)"
label variable qh113n   "Area for agricultural land (number)"
label variable qh114a   "Electricity"
label variable qh114b   "Radio"
label variable qh114c   "Television"
label variable qh114d   "Telephone (non-mobile)"
label variable qh114e   "Computer"
label variable qh114f   "Refrigerator"
label variable qh114g   "Freezer"
label variable qh114h   "Generator"
label variable qh114i   "Washing machine"
label variable qh114j   "Camera"
label variable qh114k   "Video/dvd/vcd"
label variable qh114l   "Sewing machine"
label variable qh114m   "Bed"
label variable qh114n   "Table"
label variable qh114o   "Chair"
label variable qh114p   "Cabinet"
label variable qh115a   "Wrist watch"
label variable qh115b   "Mobile telephone"
label variable qh115c   "Bicycle"
label variable qh115d   "Motorcycle or scooter"
label variable qh115e   "Animal-drawn cart"
label variable qh115f   "Car, Bus or Truck"
label variable qh115g   "Boat with a motor"
label variable qh115h   "Boat without a motor"
label variable qh116    "Bank account"
label variable qh117    "Main floor material"
label variable qh118    "Main roof material"
label variable qh119    "Main wall material"
label variable qh120h   "End of household interview (hour)"
label variable qh120m   "End of household interview (minutes)"

#delimit ;
label define QHREGION
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
label define QHDISTR 
   101 "Jomoro"
   102 "Ellembelle"
   104 "Ahanta West"
   105 "Sekondi Takoradi Metropolis"
   106 "Shama"
   107 "Wassa East"
   108 "Tarkwa Nsuaem Municipal"
   109 "Prestea/Huni Valley"
   110 "Wassa Amenfi East"
   111 "Wassa Amenfi West"
   112 "Aowin"
   113 "Sefwi Akontombra"
   114 "Sefwi Wiawso"
   115 "Sefwi Bibiani-Ahwiaso Bekwai"
   116 "Juabeso"
   117 "Bia west"
   118 "Mpohor"
   119 "Wassa Amenfi Central"
   120 "Suaman"
   121 "Bodi"
   122 "Bia East"
   201 "Komenda-Edina-Egyafo-Abirem Municipal"
   202 "Cape Coast Metropolis"
   203 "Abura-Asebu-Kwamankese"
   204 "Mfantsiman"
   205 "Ajumako-Enyan-Essiam"
   206 "Gomoa West"
   207 "Effutu Municipal"
   208 "Gomoa East"
   209 "Ewutu Senya"
   210 "Agona East"
   211 "Agona West Municipal"
   212 "Asikuma-Odoben Brakwa"
   213 "Assin South"
   214 "Assin North Municipal"
   215 "TwifoAti Morkwa"
   216 "Upper Denkyira East Municipal"
   217 "Upper Denkyira West"
   218 "Twifo Heman Lower Denkyira"
   219 "Ekumfi"
   220 "Awutu Senya East Municipal"
   301 "Ga South Municipal"
   302 "Ga West Municipal"
   303 "Ga East Municipal"
   304 "Accra Metropolis"
   305 "Adenta Municipal"
   306 "Ledzokuku/Krowor Municipal"
   307 "Ashaiman Municipal"
   308 "Tema Metropolis"
   309 "Shai Osudoku"
   310 "Ada East"
   311 "Ga Central Municipal"
   312 "La Dade Kotopon Municipal"
   313 "La Nkwantanang Madina Municipal"
   314 "Kpone Katamanso"
   315 "Ningo Prampram"
   316 "Ada West"
   401 "South Tongu"
   402 "Keta Municapal"
   403 "Ketu South"
   404 "Ketu North"
   405 "Akatsi"
   406 "Central Tongu"
   407 "Adaklu"
   408 "Ho Municipal"
   409 "South Dayi"
   410 "Kpando Municipal"
   411 "Hohoe Municipal"
   412 "Biakoye"
   413 "Jasikan"
   414 "Kadjebi"
   415 "Krachi East"
   417 "Nkwanta South"
   418 "Nkwanta North"
   419 "North Tongu"
   420 "Akatsi North"
   421 "Agotime Ziope"
   422 "Ho West"
   423 "Afadzato South"
   424 "North Dayi"
   425 "Krachi Nchumuru"
   501 "Birim South"
   502 "Birim Municipal"
   503 "West Akim Municipal"
   504 "Suhum Municipal"
   505 "Nsawam Adoagyiri Municipal"
   506 "Akwapem North"
   507 "New Juaben Municipal"
   508 "Yilo Krobo"
   509 "Lower Manya"
   510 "Asuogyaman"
   511 "Upper Manya"
   512 "Fanteakwa"
   513 "East Akim Municipal"
   514 "Kwaebibirem"
   515 "Akyem Mansa"
   516 "Birim North"
   517 "Atiwa"
   518 "Kwahu West Municipal"
   519 "Kwahu South"
   520 "Kwahu East"
   521 "Kwahu Afram Plains North"
   522 "Upper West Akyem"
   523 "Akwapim South"
   524 "Ayensuano"
   525 "Denkyembuor"
   526 "Kwahu Afram Plains South"
   601 "Atwima Mponua"
   602 "Amansie West"
   603 "Amansie Central"
   604 "Adansi South"
   605 "Obuasi Municipal"
   606 "Adansi North"
   607 "Bekwai Municipal"
   608 "Bosome Freho"
   609 "Asante Akim South"
   610 "Asante Akim Central Municipal"
   611 "Ejisu Juaben Municipal"
   612 "Bosumtwi"
   613 "Atwima Kwanwoma"
   614 "Kumasi Metropolis"
   615 "Atwima Nwabiagya"
   616 "Ahafo Ano South"
   617 "Ahafo Ano North"
   618 "Offinso Municipal"
   619 "Afigya Kwabre"
   620 "Kwabre East"
   621 "Afigya Sekyere"
   622 "Mampong Municipal"
   623 "Sekyere East"
   624 "Sekyere Afram Plains"
   625 "Sekyere Central"
   626 "Ejura Sekye Dumasi"
   627 "Offinso North"
   628 "Asokore Mampong Municipal"
   629 "Asante Akim North"
   630 "Sekyere Afram Plains North"
   701 "Asunafo South"
   702 "Asunafo North Municipal"
   703 "Asutifi North"
   704 "Dormaa Municipal"
   705 "Dormaa Central Municipal"
   706 "Tano South"
   707 "Tano North"
   708 "Sunyani Municipal"
   709 "Sunyani West"
   710 "Berekum Municipal"
   711 "Jaman South"
   712 "Jaman North"
   713 "Tain"
   714 "Wenchi Municipal"
   715 "Techiman Municipal"
   716 "Nkoranza South"
   717 "Nkoranza North"
   718 "Atebubu Amantin"
   719 "Sene West"
   720 "Pru"
   721 "Kintampo South"
   722 "Kintampo North Municipal"
   723 "Asutifi South"
   724 "Dormaa West"
   725 "Techiman North"
   727 "Sene East"
   801 "Bole"
   802 "Sawla-Tuna-Kalba"
   803 "West Gonja"
   804 "Gonja Central"
   805 "East Gonja"
   806 "Kpandai"
   807 "Nanumba South"
   808 "Nanumba North"
   809 "Zabzugu"
   810 "Yendi Municipal"
   811 "Tamale Metropolis"
   812 "Tolon"
   813 "Savelugu Nanton"
   814 "Karaga"
   815 "Gushiegu"
   816 "Saboba"
   817 "Chereponi"
   818 "Bunkpurugu Yonyo"
   819 "Mamprusi East"
   820 "Mamprusi West"
   821 "North Gonja"
   822 "Kumbumgu"
   823 "Sagnerigu Municipal"
   824 "Mion"
   825 "Tatale"
   826 "Mamprugu Moagduri"
   901 "Builsa North"
   902 "Kasena Nankana West"
   903 "Kasena Nankana East"
   904 "Bolgatanga Municipal"
   905 "Talensi"
   906 "Bongo"
   907 "Bawku West"
   908 "Garu Tempane"
   909 "Bawku Municipal"
   910 "Builsa South"
   911 "Nabdam"
   912 "Binduri"
   913 "Pusiga"
  1001 "Wa West"
  1002 "Wa Municipal"
  1003 "Wa East"
  1004 "Sissala East"
  1005 "Nadowli-Kaleo"
  1006 "Jirapa"
  1007 "Sissala West"
  1008 "Lambussie Karni"
  1009 "Lawra"
  1010 "Daffiama Bussie"
  1011 "Nandom"
;
label define QHRESULT
     1 "Completed"
     2 "No household member at home or no competent respondent at home at time of visit"
     3 "Entire household absent for extended period of time"
     4 "Postponed"
     5 "Refused"
     6 "Dwelling vacant or address not a dwelling"
     7 "Dwelling destroyed"
     8 "Dwelling not found"
     9 "Other"
;
label define QHCONSENT
     1 "Respondent agrees to be interviewed"
     2 "Respondent does not agree to be interviewed"
;
label define QHLANGQ 
     0 "English"
     1 "Akan"
     2 "Ga"
     3 "Ewe"
;
label define QHLANGI 
     0 "English"
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
label define QHLANGR 
     0 "English"
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
label define QHTRANS 
     1 "Yes"
     2 "No"
;
label define QHBABIES
     1 "Yes"
     2 "No"
;
label define QHNOFAM 
     1 "Yes"
     2 "No"
;
label define QHTEMP  
     1 "Yes"
     2 "No"
;
label define QHSTRATA
     1 "Central-Urban"
     2 "Central-Rural"
     3 "Greater Accra-Urban"
     4 "Greater Accra-Rural"
     5 "Volta-Urban"
     6 "Volta-Rural"
     7 "Western-Urban"
     8 "Western-Rural"
     9 "Ashanti-Urban"
    10 "Ashanti-Rural"
    11 "Brong Ahafo-Urban"
    12 "Brong Ahafo-Rural"
    13 "Eastern-Urban"
    14 "Eastern-Rural"
    15 "Northern-Urban"
    16 "Northern-Rural"
    17 "Upper East-Urban"
    18 "Upper East-Rural"
    19 "Upper West-Urban"
    20 "Upper West-Rural"
;
label define QHWLTHI 
     1 "Lowest"
     2 "Second"
     3 "Middle"
     4 "Fourth"
     5 "Highest"
;
label define QHTYPE  
     1 "Urban"
     2 "Rural"
;
label define QHWLTIUR
     1 "Lowest"
     2 "Second"
     3 "Middle"
     4 "Fourth"
     5 "Highest"
;
label define QH101   
    11 "Piped into dwelling"
    12 "Piped to yard/plot"
    13 "Piped to neighbor"
    14 "Public tap/standpipe"
    21 "Tube well or borehole"
    31 "Protected well"
    32 "Unprotected well"
    41 "Protected spring"
    42 "Unprotected spring"
    51 "Rainwater"
    61 "Tanker truck"
    71 "Cart with small tank"
    81 "Surface water (river/dam/lake/pond/stream/canal/irrigation channel)"
    91 "Bottled water"
    92 "Sachet water"
    96 "Other"
;
label define QH102   
    11 "Piped into dwelling"
    12 "Piped to yard/plot"
    13 "Piped to neighbor"
    14 "Public tap/standpipe"
    21 "Tube well or borehole"
    31 "Protected well"
    32 "Unprotected well"
    41 "Protected spring"
    42 "Unprotected spring"
    51 "Rainwater"
    61 "Tanker truck"
    71 "Cart with small tank"
    81 "Surface water (river/dam/lake/pond/stream/canal/irrigation channel)"
    96 "Other"
;
label define QH103   
     1 "In own dwelling"
     2 "In own yard/plot"
     3 "Elsewhere"
;
label define QH104   
   998 "Don't know"
;
label define QH105   
    11 "Flush to piped sewer system"
    12 "Flush to manhole/septic tank"
    13 "Flush to pit latrine"
    14 "Flush to somewhere else"
    15 "Flush, don't know where"
    21 "Ventilated improved pit latrine"
    22 "Pit latrine with slab"
    23 "Pit latrine without slab/open pit"
    31 "Composting toilet"
    41 "Bucket toilet"
    51 "Hanging toilet/hanging latrine"
    61 "No facility/bush/field"
    96 "Other"
;
label define QH106   
     1 "Yes"
     2 "No"
;
label define QH107   
    95 "10 or more households"
    98 "Don't know"
;
label define QH108   
     1 "Electricity"
     2 "Lpg"
     3 "Natural gas"
     4 "Biogas"
     5 "Kerosene"
     6 "Coal, lignite"
     7 "Charcoal"
     8 "Wood"
     9 "Straw/shrubs/grass"
    10 "Agricultural crop residue"
    11 "Animal dung"
    95 "No food cooked in household"
    96 "Other"
;
label define QH110   
     1 "Yes"
     2 "No"
;
label define QH111A  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111B  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111C  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111D  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111E  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111F  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111G  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111H  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH111I  
     0 "None"
    95 "95 or more"
    98 "Unknown"
;
label define QH112   
     1 "Yes"
     2 "No"
;
label define QH113U  
     1 "Hectares"
     2 "Acres"
     3 "Plots"
     4 "Poles"
     9 "Special answers"
;
label define QH113N  
  95.0 "95 or more hectares"
;
label define QH114A  
     1 "Yes"
     2 "No"
;
label define QH114B  
     1 "Yes"
     2 "No"
;
label define QH114C  
     1 "Yes"
     2 "No"
;
label define QH114D  
     1 "Yes"
     2 "No"
;
label define QH114E  
     1 "Yes"
     2 "No"
;
label define QH114F  
     1 "Yes"
     2 "No"
;
label define QH114G  
     1 "Yes"
     2 "No"
;
label define QH114H  
     1 "Yes"
     2 "No"
;
label define QH114I  
     1 "Yes"
     2 "No"
;
label define QH114J  
     1 "Yes"
     2 "No"
;
label define QH114K  
     1 "Yes"
     2 "No"
;
label define QH114L  
     1 "Yes"
     2 "No"
;
label define QH114M  
     1 "Yes"
     2 "No"
;
label define QH114N  
     1 "Yes"
     2 "No"
;
label define QH114O  
     1 "Yes"
     2 "No"
;
label define QH114P  
     1 "Yes"
     2 "No"
;
label define QH115A  
     1 "Yes"
     2 "No"
;
label define QH115B  
     1 "Yes"
     2 "No"
;
label define QH115C  
     1 "Yes"
     2 "No"
;
label define QH115D  
     1 "Yes"
     2 "No"
;
label define QH115E  
     1 "Yes"
     2 "No"
;
label define QH115F  
     1 "Yes"
     2 "No"
;
label define QH115G  
     1 "Yes"
     2 "No"
;
label define QH115H  
     1 "Yes"
     2 "No"
;
label define QH116   
     1 "Yes"
     2 "No"
;
label define QH117   
    11 "Natural: Earth/sand"
    12 "Natural: Dung"
    21 "Rudimentary: Wood planks"
    22 "Rudimentary: Palm/bamboo"
    31 "Finished: Parquet or polished wood"
    32 "Finished: Vinyl or asphalt strips"
    33 "Finished: Ceramic/marble/porcelain/tiles/terrazo"
    34 "Finished: Cement"
    35 "Finished: Woolen carpet/synthetic  carpet"
    36 "Finished: Linoleum/rubber carpet"
    96 "Other"
;
label define QH118   
    11 "Natural: Thatch/palm leaf"
    12 "Natural: Mud/sod"
    21 "Rudimentary: Rustic mat"
    22 "Rudimentary: Palm/bamboo"
    23 "Rudimentary: Wood planks"
    24 "Rudimentary: Cardboard"
    31 "Finished: Zinc/aluminium"
    32 "Finished: Wood"
    33 "Finished: Calamine/cement fiber"
    34 "Finished: Ceramic/brick tiles"
    35 "Finished: Cement"
    36 "Finished: Roofing shingles"
    37 "Finished: Asbestos/slate roofing sheets"
    96 "Other"
;
label define QH119   
    11 "Natural: Cane/palm/trunks"
    12 "Natural: Mud/landcrete"
    21 "Rudimentary: Bamboo with mud"
    22 "Rudimentary: Stone with mud"
    23 "Rudimentary: Uncovered adobe"
    24 "Rudimentary: Plywood"
    25 "Rudimentary: Cardboard"
    26 "Rudimentary: Reused wood"
    31 "Finished: Cement"
    32 "Finished: Stone with lime/cement"
    33 "Finished: Bricks"
    34 "Finished: Cement blocks"
    35 "Finished: Covered adobe"
    36 "Finished: Wood planks/shingles"
    96 "Other"
;

#delimit cr
label values qhregion QHREGION
label values qhdistr  QHDISTR 
label values qhresult QHRESULT
label values qhconsent QHCONSENT
label values qhlangq  QHLANGQ 
label values qhlangi  QHLANGI 
label values qhlangr  QHLANGR 
label values qhtrans  QHTRANS 
label values qhbabies QHBABIES
label values qhnofam  QHNOFAM 
label values qhtemp   QHTEMP  
label values qhstrata QHSTRATA
label values qhwlthi  QHWLTHI 
label values qhtype   QHTYPE  
label values qhwltiur QHWLTIUR
label values qh101    QH101   
label values qh102    QH102   
label values qh103    QH103   
label values qh104    QH104   
label values qh105    QH105   
label values qh106    QH106   
label values qh107    QH107   
label values qh108    QH108   
label values qh110    QH110   
label values qh111a   QH111A  
label values qh111b   QH111B  
label values qh111c   QH111C  
label values qh111d   QH111D  
label values qh111e   QH111E  
label values qh111f   QH111F  
label values qh111g   QH111G  
label values qh111h   QH111H  
label values qh111i   QH111I  
label values qh112    QH112   
label values qh113u   QH113U  
label values qh113n   QH113N  
label values qh114a   QH114A  
label values qh114b   QH114B  
label values qh114c   QH114C  
label values qh114d   QH114D  
label values qh114e   QH114E  
label values qh114f   QH114F  
label values qh114g   QH114G  
label values qh114h   QH114H  
label values qh114i   QH114I  
label values qh114j   QH114J  
label values qh114k   QH114K  
label values qh114l   QH114L  
label values qh114m   QH114M  
label values qh114n   QH114N  
label values qh114o   QH114O  
label values qh114p   QH114P  
label values qh115a   QH115A  
label values qh115b   QH115B  
label values qh115c   QH115C  
label values qh115d   QH115D  
label values qh115e   QH115E  
label values qh115f   QH115F  
label values qh115g   QH115G  
label values qh115h   QH115H  
label values qh116    QH116   
label values qh117    QH117   
label values qh118    QH118   
label values qh119    QH119   
