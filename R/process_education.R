require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_education/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Load datasets
data<-terra::rast(paste0(DataDir,"/atlas_education/raw/shdi-2018-education.tif"))

# 0 is an NA value is this dataset
data[data==0]<-NA
names(data)<-"Education"

# Resample data
data<-terra::resample(data,base_raster,method="average")

# Mask data
data<-terra::mask(data,adm1_africa)
data<-data*water_mask

lapply(names(data),FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    suppressWarnings(terra::writeRaster(data[[FIELD]],file=File,overwrite=T))
    })

Overwrite=T
# Adaptive capacity is classified thus: high = good, low = bad
# High education is better, lower tercile = bad, upper tercile = good 
Invert=F

devtools::source_url("https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/functions.R?raw=TRUE")

data_terciles<-quantile_split(data,
                              Labels=c("low","medium","high"),
                              Invert=Invert,
                              Overwrite=T,
                              do_binary_splits=T,
                              savedir=DataDirInt)

terra::plot(c(data,data_terciles))

Files<-list.files(DataDirInt,".tif",full.names=T)
Files<-Files[!grepl("aux",Files)]
terra::plot(terra::rast(Files))