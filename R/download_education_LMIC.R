DataDir<-"/home/jovyan/common_data/"

SaveDir<-paste0(DataDir,"/atlas_education_LMIC/raw"
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

Files<- "Data.zip"

URLS<-"https://cloud.ihme.washington.edu/index.php/download/"
   

destfile<-paste0(SaveDir,"/",Files)
options(timeout=480*2)

# Display progress
cat('\r                                                ')
cat('\r',paste0("Downloading file: ",URL))
flush.console()

if(!file.exists(destfile)){
        download.file(URL, destfile)
    }  
