#this is part 2 of the dataset downloader scripts
#the first one throws an error at the NG mens recode,  which is why that one starts this script

file_names <- "
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGMR4ADT.zip&Tp=1&Ctry_Code=NG&surv_id=223&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGMR52DT.zip&Tp=1&Ctry_Code=NG&surv_id=302&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGMR6ADT.zip&Tp=1&Ctry_Code=NG&surv_id=438&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGMR7ADT.zip&Tp=1&Ctry_Code=NG&surv_id=528&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR21DT.zip&Tp=1&Ctry_Code=NG&surv_id=32&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR4CDT.zip&Tp=1&Ctry_Code=NG&surv_id=223&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR53DT.zip&Tp=1&Ctry_Code=NG&surv_id=302&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR61DT.zip&Tp=1&Ctry_Code=NG&surv_id=392&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR6ADT.zip&Tp=1&Ctry_Code=NG&surv_id=438&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR71DT.zip&Tp=1&Ctry_Code=NG&surv_id=474&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR7BDT.zip&Tp=1&Ctry_Code=NG&surv_id=528&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGPR81DT.zip&Tp=1&Ctry_Code=NG&surv_id=576&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGSQ23DT.zip&Tp=1&Ctry_Code=NG&surv_id=32&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=NGWI21DT.zip&Tp=1&Ctry_Code=NG&surv_id=32&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=SDBR02DT.zip&Tp=1&Ctry_Code=SD&surv_id=29&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=SDHH01DT.zip&Tp=1&Ctry_Code=SD&surv_id=29&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=SDIR02DT.zip&Tp=1&Ctry_Code=SD&surv_id=29&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=SDKR01DT.zip&Tp=1&Ctry_Code=SD&surv_id=29&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR01DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWBR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWCR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWCR41DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWCR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWCR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWCR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHH01DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHR42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHW01DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHW31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHW41DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWHW51DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR01DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWIR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR01DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWKR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWMR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWMR41DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWMR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWMR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWMR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWPR31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWPR42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWPR52DT.zip&Tp=1&Ctry_Code=ZW&surv_id=260&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWPR62DT.zip&Tp=1&Ctry_Code=ZW&surv_id=367&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWPR72DT.zip&Tp=1&Ctry_Code=ZW&surv_id=475&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWSQ02DT.zip&Tp=1&Ctry_Code=ZW&surv_id=21&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWSQ30DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWWI31DT.zip&Tp=1&Ctry_Code=ZW&surv_id=64&dm=1&dmode=nm
https://dhsprogram.com/customcf/legacy/data/download_dataset.cfm?Filename=ZWWI42DT.zip&Tp=1&Ctry_Code=ZW&surv_id=184&dm=1&dmode=nm
"

urls <- strsplit(file_names, "\n")

#name destination folder
destination_f <- "surveys"
#create destination folder 
if(!dir.exists(destination_f)) {
  dir.create(destination_f)
}
#loop through urls and download files
for (i in seq_along(urls[[1]])) {  # access the first element of the lis (index 1)
  current_url <- urls[[1]][i]  # Get the ith url
  
  file_name <- sub(".*Filename=([^&]+\\.zip).*", "\\1", current_url)  #use ith url
  destination <- file.path(destination_f, file_name)  
  if (grepl("\\.zip$", destination)) {
    download.file(current_url, destination)  #download the file
    unzip(destination, exdir = destination_f)  #unzip the file
  }
}
