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
# convert kbps to mbps
mnp_centroids$avg_d_mbps<-mnp_centroids$avg_d_kbps/1000

# explore dl speed data
avg_d_mbps<-mnp_centroids$avg_d_mbps
round(
    c(
        perc_0.1=100*(length(avg_d_mbps[avg_d_mbps<0.1])/length(avg_d_mbps)),
        perc_0.1_10=100*(length(avg_d_mbps[avg_d_mbps>=0.1 & avg_d_mbps<10])/length(avg_d_mbps)),
        perc_10_100=100*(length(avg_d_mbps[avg_d_mbps>=10 & avg_d_mbps<100])/length(avg_d_mbps)),
        perc_100=100*(length(avg_d_mbps[avg_d_mbps>=100])/length(avg_d_mbps))
    )
    ,2)

terra::plot(mnp_centroids)
hist(mnp_centroids$avg_d_mbps,breaks=100)
hist(mnp_centroids$avg_d_mbps[mnp_centroids$avg_d_mbps<100],breaks=100)

# Rasterize points within 5m pixels, averaging the download speed (a more sophisticated approach would be to take a weighted mean based on number of tests per point)
mnp_rast<-terra::rasterize(mnp_centroids,base_raster,field="avg_d_mbps",fun=median)
names(mnp_rast)<-"med_d_mbps"
terra::plot(mnp_rast)
hist(mnp_rast[],breaks=100)
hist(mnp_rast[mnp_rast<100][],breaks=100)

# re-explore dl speed data
avg_d_mbps<-mnp_rast[!is.na(mnp_rast)][]
round(
    c(
        perc_lt0.1=100*(length(avg_d_mbps[avg_d_mbps<0.1])/length(avg_d_mbps)),
        perc_0.1_1=100*(length(avg_d_mbps[avg_d_mbps>=0.1 & avg_d_mbps<1])/length(avg_d_mbps)),
        perc_1_10=100*(length(avg_d_mbps[avg_d_mbps>=1 & avg_d_mbps<10])/length(avg_d_mbps)),
        perc_10_100=100*(length(avg_d_mbps[avg_d_mbps>=10 & avg_d_mbps<100])/length(avg_d_mbps)),
        perc_gt100=100*(length(avg_d_mbps[avg_d_mbps>=100])/length(avg_d_mbps))
    )
    ,2)


# Change to projected CRS
mnp_rast_pr<-terra::project(mnp_rast,"+proj=merc +datum=WGS84 +units=km")

# Convert rasterized data back to points
mnp_points<-as.points(mnp_rast_pr, values=TRUE, na.rm=TRUE)
terra::plot(mnp_points,"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps<0.1],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps<1],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=1 & mnp_points$med_d_mbps<10],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=10 & mnp_points$med_d_mbps<100],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=100],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)

# Inverse distance weighted interpolation
dat2 <- data.frame(geom(mnp_points)[, c("x", "y")], as.data.frame(mnp_points))

gs <- gstat::gstat(formula=med_d_mbps~1, locations=~x+y, data=dat2)
mnp <- terra::interpolate(mnp_rast_pr, gs, debug.level=0)
mnp<-mnp[[1]]
# Convert back to lat/lon
mnp_rp<-terra::project(mnp,crs(mnp_rast))
# Crop and mask to SSA
mnp_rp<-terra::mask(terra::crop(mnp_rp,adm1_africa),adm1_africa)
names(mnp_rp)<-"dl_speed"
terra::plot(mnp_rp)
terra::plot(log10(mnp_rp))

if(F){
# Thin plate spline
library(fields)
m <- fields::Tps(dat2[,c("x", "y")], dat2$med_d_mbps)
tps <- terra::interpolate(mnp_rast_pr, m)
tps<-tps[[1]]
# Convert back to lat/lon
tps_rp<-terra::project(tps,crs(mnp_rast))
# Crop and mask to SSA
tps_rp<-terra::mask(terra::crop(tps_rp,adm1_africa),adm1_africa)
names(tps_rp)<-"dl_speed"
terra::plot(tps_rp)
terra::plot(log10(tps_rp))
}    

# Save data
terra::writeRaster(mnp_rp,paste0(DataDirInt,"/",names(mnp_rp),".tif"),overwrite=T)
data<-mnp_rp

# Generate manual breakpoints
m_reclass<-cbind(c(0,100,100000),c(100,100000,99999999),c(0,1,2))
data_reclass<-terra::classify(data,m_reclass)
levels(data_reclass)<-c("low","medium","high")
terra::plot(data_reclass)
suppressWarnings(terra::writeRaster(data_reclass,file=paste0(DataDirInt,"/",names(mnp_rp),"_manclass.tif"),overwrite=T))
                 
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


