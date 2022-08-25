require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_market_access/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt,recursive=T)
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

# Read in market access data
market_access<-terra::rast(paste0(DataDir,"/atlas_market_access/raw/2015_accessibility_to_cities_v1.0.tif"))

# Resample
market_access<-terra::resample(market_access,base_raster,method="average")

# Crop and mask
data<-terra::mask(terra::crop(market_access,adm1_africa),adm1_africa)*water_mask
names(data)<-"market_access"

# Save data
terra::writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

# Generate terciles
# Adaptive capacity is classified thus: high = good, low = bad
# Low distance to market is better, lower tercile = good, upper tercile = bad 
Invert<-T

devtools::source_url("https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/functions.R?raw=TRUE")

data_terciles<-quantile_split(data,
                              Labels=c("low","medium","high"),
                              Invert=Invert,
                              Overwrite=T,
                              do_binary_splits=T,
                              savedir=DataDirInt)

Files<-list.files(DataDirInt,".tif",full.names=T)
Files<-Files[!grepl("aux",Files)]
terra::plot(terra::rast(Files))
