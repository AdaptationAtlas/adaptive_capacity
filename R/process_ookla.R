require(gstat)
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
rm(mnp)
gc()

# Mask to sub-Saharan Africa
mnp_centroids<-terra::mask(terra::crop(mnp_centroids,adm1_africa),adm1_africa)
# convert kbps to mbps
mnp_centroids$avg_d_mbps<-mnp_centroids$avg_d_kbps/1000

# explore dl speed data
if(F){
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
}

# Rasterize points within 5m pixels, averaging the download speed (a more sophisticated approach would be to take a weighted mean based on number of tests per point)
mnp_rast<-terra::rasterize(mnp_centroids,base_raster,field="avg_d_mbps",fun=median)
names(mnp_rast)<-"med_d_mbps"

if(F){
# re-explore dl speed data
terra::plot(mnp_rast)
hist(mnp_rast[],breaks=100)
hist(mnp_rast[mnp_rast<100][],breaks=100)
    
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
}

# Change to projected CRS
mnp_rast_pr<-terra::project(mnp_rast,"+proj=merc +datum=WGS84 +units=km",method="near")

# Convert rasterized data back to points
mnp_points<-as.points(mnp_rast_pr, values=TRUE, na.rm=TRUE)

if(F){
terra::plot(mnp_points,"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps<0.1],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps<1],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=1 & mnp_points$med_d_mbps<10],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=10 & mnp_points$med_d_mbps<100],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
terra::plot(mnp_points[mnp_points$med_d_mbps>=100],"med_d_mbps",breaks=c(0,.1,1,10,100,999),cex=0.75)
}

dat2 <- data.frame(geom(mnp_points)[, c("x", "y")], as.data.frame(mnp_points))

# Create null model
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(mnp_points$med_d_mbps), mnp_points$med_d_mbps)
null

# Nearest Neighbour
gs <- gstat(formula=med_d_mbps~1, locations=~x+y, data=dat2, nmax=5, set=list(idp = 0))
nn <- interpolate(mnp_rast_pr, gs, debug.level=0)

nn<-nn[[1]]
# Convert back to lat/lon
nn<-terra::project(nn,crs(mnp_rast))
# Crop and mask to SSA
nn<-terra::mask(terra::crop(nn,adm1_africa),adm1_africa)

terra::plot(nn)
terra::plot(log10(nn))
hist(nn[nn<100],breaks=100)

# Cross-validate optimized NN model
rmse <- rep(NA, 5)

kf <- sample(1:5, nrow(dat2), replace=TRUE)
for (k in 1:5) {
  test <- dat2[kf == k, ]
  train <- dat2[kf != k, ]
  gs <- gstat(formula=med_d_mbps~1, locations=~x+y, data=train, nmax=5, set=list(idp = 0))
  p <- predict(gs, test, debug.level=0)
  rmse[k] <- RMSE(test$med_d_mbps, p$var1.pred)
}

mean(rmse)
1 - (mean(rmse) / null)


# Inverse distance weighted interpolation

# Optimize IDW parameters - https://rspatial.org/terra/analysis/4-interpolation.html

f1 <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat::gstat(formula=med_d_mbps~1, locations=~x+y, data=train, nmax=nmx, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$med_d_mbps, p)
}

kf <- sample(1:5, nrow(dat2), replace=TRUE)

opt<-lapply(1:5,FUN=function(k){
    test <- dat2[kf == k, ]
    train <- dat2[kf != k, ]
    opt <- optim(c(8, .5), f1, test=test, train=train)
    opt
})

opt_par<-do.call("rbind",lapply(opt,"[[","par"))
opt_par<-apply(opt_par,2,mean)

m <- gstat(formula=med_d_mbps~1, locations=~x+y, data=dat2, nmax=opt_par[1], set=list(idp=opt_par[2]))
idw_opt <- interpolate(mnp_rast_pr, m, debug.level=0)
idw_opt<-idw_opt[[1]]
# Convert back to lat/lon
idw_opt<-terra::project(idw_opt,crs(mnp_rast))
# Crop and mask to SSA
idw_opt<-terra::mask(terra::crop(idw_opt,adm1_africa),adm1_africa)

terra::plot(idw_opt)
terra::plot(log10(idw_opt))
hist(idw_opt[idw_opt<100],breaks=100)

# Cross-validate optimized IDW model
rmse <- rep(NA, 5)

kf <- sample(1:5, nrow(dat2), replace=TRUE)
for (k in 1:5) {
  test <- dat2[kf == k, ]
  train <- dat2[kf != k, ]
  gs <- gstat(formula=med_d_mbps~1, locations=~x+y, data=train, nmax=opt_par[1], set=list(idp=opt_par[2]))
  p <- predict(gs, test, debug.level=0)
  rmse[k] <- RMSE(test$med_d_mbps, p$var1.pred)
}

mean(rmse)
1 - (mean(rmse) / null)

# Standard IDW model without interpolation
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

# Cross-validate Standard IDW model
rmse <- rep(NA, 5)

kf <- sample(1:5, nrow(dat2), replace=TRUE)
for (k in 1:5) {
  test <- dat2[kf == k, ]
  train <- dat2[kf != k, ]
  gs <- gstat(formula=med_d_mbps~1, locations=~x+y, data=train)
  p <- predict(gs, test, debug.level=0)
  rmse[k] <- RMSE(test$med_d_mbps, p$var1.pred)
}

mean(rmse)
1 - (mean(rmse) / null)


# Save data
data<-idw_opt
names(data)<-"dl_speed"
terra::writeRaster(data,paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

if(F){
# Thin plate spline
library(fields)
m <- fields::Tps(dat2[,c("x", "y")], dat2$med_d_mbps)
tps <- terra::interpolate(mnp_rast_pr, m, debug.level=0)
tps<-tps[[1]]
# Convert back to lat/lon
tps_rp<-terra::project(tps,crs(mnp_rast))
# Crop and mask to SSA
tps_rp<-terra::mask(terra::crop(tps_rp,adm1_africa),adm1_africa)
names(tps_rp)<-"dl_speed"
terra::plot(tps_rp)
terra::plot(log10(tps_rp))
}    

# Generate manual breakpoints
m_reclass<-cbind(c(0,10,50),c(10,50,99999999),c(0,1,2))
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

terra::plot(c(log10(data),data_reclass,data_terciles))


