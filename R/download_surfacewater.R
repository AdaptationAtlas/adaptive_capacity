DataDir<-"/home/jovyan/common_data"
SaveDir<-paste0(DataDir,"/atlas_surfacewater/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }

# Rivers
# https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/b891ca64-4cd4-4efd-a7ca-b386e98d52e8

# Waterbodies
# https://data.apps.fao.org/catalog/dataset/bd8def30-88fd-11da-a88f-000d939bc5d8

URLS<-c("https://storage.googleapis.com/fao-maps-catalog-data/geonetwork/aquamaps/rivers_africa_37333.zip","https://storage.googleapis.com/fao-maps-catalog-data/geonetwork/aquamaps/Aquamaps-River_Data_description.pdf","https://storage.googleapis.com/fao-maps-catalog-data/geonetwork/aquamaps/waterbodies_africa.zip")
        
destfiles<-paste0(SaveDir,c("/rivers_africa_37333.zip","/Aquamaps-River_Data_description.pdf","/waterbodies_africa.zip"))

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

# Unzip
unzip(destfiles[1], exdir=SaveDir)
unzip(destfiles[3], exdir=SaveDir)
