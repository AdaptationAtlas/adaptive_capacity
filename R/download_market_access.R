# Data is available from:
#1) GEE https://explorer.earthengine.google.com/#detail/Oxford%2FMAP%2Faccessibility_to_cities_2015_v1_0
#2) Malaria atlas https://malariaatlas.org/research-project/accessibility-to-cities/

DataDir<-"/home/jovyan/common_data/"

SaveDir<-paste0(DataDir,"/atlas_market_access/raw")
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

Files<-"2015_accessibility_to_cities_v1.0.zip"

URL<-"https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2015_accessibility_to_cities_v1.0"
   
destfile<-paste0(SaveDir,"/",Files)
options(timeout=480*2)

if(!file.exists(destfile)){
        download.file(URL, destfile)
    }  

unzip(destfile, exdir="./raw")