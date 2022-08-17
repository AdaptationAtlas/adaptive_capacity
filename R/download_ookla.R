# https://github.com/teamookla/ookla-open-data

DataDir<-"/home/jovyan/common_data/"

SaveDir<-paste0(DataDir,"/atlas_ookla/raw")
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

URLS<-c("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2022/quarter=2/2022-04-01_performance_mobile_tiles.zip")

destfiles<-paste0(SaveDir,"/2022-04-01_performance_mobile_tiles.zip")

options(timeout=480*2)

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