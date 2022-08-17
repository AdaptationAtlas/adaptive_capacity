DataDir<-"/home/jovyan/common_data"
SaveDir<-paste0(DataDir,"/atlas_groundwater/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
    }

# Groundwater storage, depth to and productivity maps must be manually downloaded from https://www2.bgs.ac.uk/groundwater/international/africangroundwater/mapsDownload.html
# “Based upon mapping provided by British Geological Survey © NERC 2012. All rights reserved”.

# Groundwater Recharge
# https://www2.bgs.ac.uk/nationalgeosciencedatacentre/citedData/catalogue/45d2b71c-d413-44d4-8b4b-6190527912ff.html
# https://iopscience.iop.org/article/10.1088/1748-9326/abd661#erlabd661f4
URLS<-c("https://www2.bgs.ac.uk/downloads/start.cfm?id=2531")
        

destfiles<-paste0(SaveDir,"/Groundwater productivity.zip")

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

# Unzip
unzip(destfile, exdir=SaveDir)

R.utils::copyDirectory(paste0(SaveDir,"/139265/Africa_Recharge_Map"), SaveDir)
unlink(paste0(SaveDir,"/139265"),recursive=T)
