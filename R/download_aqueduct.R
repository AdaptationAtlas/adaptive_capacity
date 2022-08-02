SaveDir<-"/home/jovyan/common_data/atlas/raw/3_adaptivecapacity/aqueduct_3"

URLS<-c("https://wri-projects.s3.amazonaws.com/Aqueduct30/finalData/Y2019M07D12_Aqueduct30_V01.zip")

destfiles<-paste0(SaveDir,"/aqueduct_3.zip")

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