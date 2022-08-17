install.packages("dataverse")
require(dataverse)

DataDir<-"/home/jovyan/common_data"
SaveDir<-paste0(DataDir,"/atlas_poverty_IWI/raw")
if(!dir.exists(SaveDir)){
    dir.create(SaveDir,recursive=T)
    }


# https://www.sciencedirect.com/science/article/abs/pii/S0305750X22002182?via%3Dihub=&s=09
# https://doi.org/10.7910/DVN/5OGWYM

X<-get_dataset("10.7910/DVN/5OGWYM",server="dataverse.harvard.edu")

# X$files returns an error so files "estimated_wealth_index.csv.zip" downloaded manually from website 

# Unzip and tidy up files
Files<-list.files(SaveDir,".zip",full.names=T)

for(FILE in Files){
    unzip(FILE,exdir=SaveDir)
}

unlink(Files)
