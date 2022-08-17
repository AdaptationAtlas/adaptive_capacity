require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_surfacewater/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }
    
# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in rivers
rivers<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/rivers_africa_37333.shp"))

# Subset to perennial
rivers_p<-rivers[rivers$Regime=="P" & rivers$Strahler>=2]
rivers_p<-terra::mask(rivers_p,adm1_africa)

# Reproject to Chamberlin Trimetric projection
rivers_p_pr<-project(rivers_p,"+proj=chamb +lat_1=22 +lon_1=0 +lat_2=22 +lon_2=45 +lat_3=-22 +lon_3=22.5 +datum=WGS84")
# Create raster for distance calculations
riv_dist<-terra::rast(rivers_p_pr,resolution=c(5000,5000))

riv_dist<-terra::distance(riv_dist,rivers_p_pr)
riv_dist<-terra::mask(riv_dist,adm1_africa)
terra::writeRaster(riv_dist,paste0(DataDirInt,"/river_dist.tif"))

# Find distances
riv_dist_rs<-terra::resample(riv_dist,base_raster)

# Read in waterbodies
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
