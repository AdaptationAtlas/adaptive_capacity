DataDir<-"/home/jovyan/common_data/"

SaveDir<-paste0(DataDir,"/atlas_relative_wealth/raw"
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }
                
URLS<-"https://data.humdata.org/dataset/76f2a2ea-ba50-40f5-b79c-db95d668b843/resource/de2f953e-940c-43bb-b1f8-4d02d28124b5/download/relative-wealth-index-april-2021.zip"

destfiles<-paste0(SaveDir,"/relative_wealth.zip")

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