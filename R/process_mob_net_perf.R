install.packages("slippymath")
require(slippymath)
require(terra)
options(scipen=999)

# Set raw and intermediate directories for ookla data
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_ookla/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in ookla data
OoklaDir<-paste0(DataDir,"/atlas_ookla/raw")

mnp <- terra::vect(paste0(OoklaDir,"/gps_mobile_tiles.shp"))

# Convert tiles to points
mnp_centroids<-terra::centroids(mnp)

# Mask to sub-Saharan Africa
mnp_centroids<-terra::mask(terra::crop(mnp_centroids,adm1_africa),adm1_africa)

terra::plot(mnp_centroids)
hist(mnp_centroids$avg_d_kbps)

# Rasterize points within 5m pixels, averaging the download speed (a more sophisticated approach would be to take a weighted mean based on number of tests per point)
mnp_rast<-terra::rasterize(mnp_centroids,base_raster,field="avg_d_kbps",fun=mean)
names(mnp_rast)<-"avg_d_kbps"

# Change to projected CRS
mnp_rast_pr<-terra::project(mnp_rast,"+proj=merc +datum=WGS84 +units=km")

# Convert rasterized data back to points
mnp_points<-as.points(mnp_rast_pr, values=TRUE, na.rm=TRUE)

# Inverse distance weighted interpolation
dat2 <- data.frame(geom(mnp_points)[, c("x", "y")], as.data.frame(mnp_points))

gs <- gstat::gstat(formula=avg_d_kbps~1, locations=~x+y, data=dat2)
mnp <- terra::interpolate(mnp_rast_pr, gs, debug.level=0)
mnp<-mnp[[1]]

# Convert back to lat/lon
mnp_rp<-terra::project(mnp,crs(mnp_rast))
# Crop and mask to SSA
mnp_rp<-terra::mask(terra::crop(mnp_rp,adm1_africa),adm1_africa)
names(mnp_rp)<-"dl_speed"
terra::plot(mnp_rp)

# Save data
terra::writeRaster(mnp_rp,paste0(DataDirInt,"/",names(mnp_rp),".tif"),overwrite=T)
data<-mnp_rp

# Generate terciles
Overwrite<-T
# Adaptive capacity is classified thus: high = good, low = bad
# High download speed is better, lower tercile = bad, upper tercile = good 
Invert<-F

Labels<-c("low","medium","high")
Values<-c(0,1,2)

data_terciles<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
    File<-paste0(DataDirInt,"/",FIELD,"_terciles.tif")
    
    if((!file.exists(File))|Overwrite){
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:3/3),na.rm=T)

       Levels<-if(Invert){Labels[length(Labels):1]}else{Labels}
                
        Intervals<-data.frame(Intervals=apply(cbind(Levels,round(vTert[1:3],3),round(vTert[2:4],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(DataDirInt,"/",FIELD,"_terciles.csv"),row.names=F)
        
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = Values)
        
        X[]<-as.numeric(Terciles)-1
        levels(X)<-  Levels  
        suppressWarnings(terra::writeRaster(X,file=File,overwrite=T))
        X
        }else{
            terra::rast(File)
    }
}))

terra::plot(c(data,data_terciles))


