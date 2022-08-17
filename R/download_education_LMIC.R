DataDir<-"/home/jovyan/common_data/"

SaveDir<-paste0(DataDir,"/atlas_education_LMIC/raw"
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

Files<- "Data.zip"

URL<-"https://cloud.ihme.washington.edu/index.php/download/"
   

destfile<-paste0(SaveDir,"/",Files)
options(timeout=480*2)

if(!file.exists(destfile)){
        download.file(URL, destfile)
    }  
