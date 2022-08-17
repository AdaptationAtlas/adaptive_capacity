DataDir<-"/home/jovyan/common_data"
SaveDir<-paste0(DataDir,"/atlas_irrigation/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

# https://zenodo.org/record/6886564#.YvteZXZBxD9
# https://doi.org/10.31223/X5C932

URLS<-c("https://zenodo.org/record/6886564/files/G_AEI_2015.asc?download=1")
        
destfiles<-paste0(SaveDir,c("/G_AEI_2015.asc"))

options(timeout=480*2)

for(i in 1:length(URLS)){
    URL<-URLS[i]
    destfile<-destfiles[i]
    # Display progress
    cat('\r                                                                                                                             ')
    cat('\r',paste0("Downloading file: ",URL))
    flush.console()
    
    if(!file.exists(destfile)){
            download.file(URL, destfile)
        }      
}