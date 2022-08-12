# Set raw and intermediate directories for ookla data
DataDir<-"/home/jovyan/common_data"

SaveDir<-past0(DataDir,"/atlas_accessiblity_to_healthcare/raw")

if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

URLS<-c("https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2020_walking_only_travel_time_to_healthcare",
         "https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2020_motorized_travel_time_to_healthcare")

destfiles<-paste0(SaveDir,"/",c("2020_walking_only_travel_time_to_healthcare","2020_motorized_travel_time_to_healthcare"),".zip")
options(timeout=480)
for(i in 1:length(URLS)){
    URL<-URLS[i]
    destfile<-destfiles[i]
    # Display progress
    cat('\r                                                ')
    cat('\r',paste0("Downloading file: ",URL))
    flush.console()
    
    if(!file.exists(destfile)){
            download.file(URL, destfile)
        }  
    
}