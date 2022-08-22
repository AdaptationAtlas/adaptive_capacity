require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_pop/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt,recursive=T)
    }
    
# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))

# Read in boundary of subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in population data
pop<-terra::rast(list.files(paste0(DataDir,"/atlas_pop/raw"),".tif",full.names=T))
cellsize_ha<-terra::cellSize(pop[[1]],unit="ha")

# Create a higher resolution raster to work out area of waterbody per pixel
cellsize_da<-terra::disagg(cellsize_ha,fact=10)
water_rast<-terra::rasterize(waterbodies,cellsize_da)

# Work out proportion of cell that is not water and multiply cellsize_ha by this value
water_rast<-(100-terra::aggregate(water_rast,fact=10,fun=sum,na.rm=T))/100
water_rast[is.na(water_rast)]<-1
cellsize_ha<-cellsize_ha*water_rast

# Population per ha (density)
pop_ha<-pop/cellsize 

pop_ha<-terra::mask(terra::crop(pop_ha,sh_africa),sh_africa)
pop_ha<-terra::resample(pop_ha,base_raster,method="near")
 
terra::plot(pop_ha)

# Save processed files
list.files(paste0(DataDir,"/atlas_pop/raw"),".tif")
Files<-c("rural_pop_density","total_pop_density","urban_pop_density")
for(i in 1:terra::nlyr(pop_ha)){
    terra::writeRaster(pop_ha[[i]],paste0(DataDirInt,"/",Files[i],".tif"))
    }