SaveDir<-"/home/jovyan/common_data/atlas/raw/3_adaptivecapacity/education_LMIC"

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
