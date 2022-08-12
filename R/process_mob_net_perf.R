install.packages("slippymath")
require(slippymath)
require(terra)

# Set raw and intermediate directories for ookla data
DataDir<-"/home/jovyan/common_data"

# Read in boundary of sub-saharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))
sh_africa<-terra::simplifyGeom(sh_africa, tolerance=0.1)

# Read in ookla data
OoklaDir<-paste0(DataDir,"/mob_net_perf_ookla/raw")
OoklaDirInt<-paste0(DataDir,"/mob_net_perf_ookla/intermediate")
if(!dir.exists(OoklaDirInt)){
    dir.create(OoklaDirInt)
    }


# Ani's code
mnp <- terra::vect(paste0(OoklaDir,"/gps_mobile_tiles.shp"))



# Ookla approach
# https://github.com/teamookla/ookla-open-data/blob/master/tutorials/aggregate_by_county.md
require(sf)
require(tidyverse)

tiles <- sf::read_sf(paste0(OoklaDir,"/gps_mobile_tiles.shp")) %>%
  mutate(avg_d_kbps = as.numeric(avg_d_kbps),
         avg_u_kbps = as.numeric(avg_u_kbps),
         avg_lat_ms = as.numeric(avg_lat_ms))

sh_africa<-sf::st_as_sf(sh_africa)

tiles_in_af <- sf::st_join(sh_africa, tiles, left = FALSE)

