require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_irrigation/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }
    
# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in irrigation data
data<-terra::rast(paste0(DataDir,c("/atlas_irrigation/raw/G_AEI_2015.asc")))
# Crop and mask
data<-terra::mask(terra::crop(data,adm1_africa),adm1_africa)
# Resample
data<-terra::resample(data,base_raster,method="near")

names(data)<-"irrigated_area"

# Save data
terra::writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

hist(data[data>0][])

data2<-data
data2[data2>0]<-1
plot(data2)
