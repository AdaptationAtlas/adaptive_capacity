DataDir<-"/home/jovyan/common_data"
SaveDir<-paste0(DataDir,"/atlas_electrification/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

# https://www.nature.com/articles/s41597-019-0122-6
URLS<-"https://md-datasets-cache-zipfiles-prod.s3.eu-west-1.amazonaws.com/kn4636mtvg-6.zip"

destfiles<-paste0(SaveDir,"/electrification_v6.zip")

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

# https://doi.org/10.1016/j.oneear.2020.03.007
# https://github.com/giacfalk/inequality_electrification_SSA

types<-c("shp","qpj","dbf","shx","prj")

URLS<-paste0("https://github.com/giacfalk/inequality_electrification_SSA/blob/master/maps/growing_pop_without_access.",types)
destfiles<-paste0(SaveDir,"/growing_pop_without_access.",types)
    
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