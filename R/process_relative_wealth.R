require(terra)
require(sf)
require(raster)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Source functions
devtools::source_url("https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/functions_relative_wealth.R?raw=TRUE")

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))
adm1_iso3<-unique(adm1_africa$GID_0)

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# input
datadir <- paste0(DataDir,"/atlas_relative_wealth/raw")

ff <- list.files(datadir, full.names = TRUE)

# list of countries
isol <- gsub("_relative_wealth_index.csv", "", basename(ff))
# need to do this because IND and PAK are merged
isol <- unlist(strsplit(isol, "_"))

isol<-isol[isol %in% adm1_iso3]

lapply(isol, convertPoints, ff, datadir, overwrite=TRUE)
