DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
SaveDir<-paste0(DataDir,"/atlas_aqueduct_3/raw")

if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

# https://www.wri.org/applications/aqueduct/water-risk-atlas
# https://www.wri.org/research/aqueduct-30-updated-decision-relevant-global-water-risk-indicators

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