# https://github.com/teamookla/ookla-open-data

DataDir<-"/home/jovyan/common_data"

SaveDir<-paste0(DataDir,"/atlas_ookla/raw")
                
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

years<-c(2020,2021,2022)
quarters=c(1,2,3,4)

dates<-expand.grid(quarter=quarters,year=years)
dates$start.date<-paste0(dates$year,rep(c("-01-01","-04-01","-07-01","-10-01"),3))
dates<-dates[!(dates$year==2022 & dates$quarter %in% c(1,2)),]
dates$URL<-paste0("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=",dates$year,"/quarter=",dates$quarter,"/",dates$start.date,"_performance_mobile_tiles.zip")
dates$destfile<-paste0(SaveDir,"/",dates$start.date,"_performance_mobile_tiles.zip")

URLS<-dates$URL
destfiles<-dates$destfile

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