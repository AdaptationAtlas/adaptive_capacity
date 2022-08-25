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

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Read in ookla data
OoklaDirs<-list.dirs(paste0(DataDir,"/atlas_ookla/raw"))
OoklaDirs<-grep("20",OoklaDirs,value=T)
Dates<-unlist(data.table::tstrsplit(OoklaDirs,"/",keep=7))

Overwrite<-F
mnp_stack<-terra::rast(lapply(1:length(OoklaDirs),FUN=function(i){
    File<-paste0(DataDirInt,"/",Dates[i],".tif")
    if(!file.exists(File)|Overwrite==T){
        OoklaDir<-OoklaDirs[i]
        mnp <- terra::vect(paste0(OoklaDir,"/gps_mobile_tiles.shp"))

        # Convert tiles to points
        mnp_centroids<-terra::centroids(mnp)
        rm(mnp)
        gc()

        # Mask to sub-Saharan Africa
        mnp_centroids<-terra::mask(terra::crop(mnp_centroids,adm1_africa),adm1_africa)
        # convert kbps to mbps
        mnp_centroids$avg_d_mbps<-mnp_centroids$avg_d_kbps/1000

         # Rasterize points within 5m pixels, averaging the download speed (a more sophisticated approach would be to take a weighted mean based on number of tests per point)
        mnp_rast<-terra::rasterize(mnp_centroids,base_raster,field="avg_d_mbps",fun=median)
        names(mnp_rast)<-"med_d_mbps"

        suppressWarnings(terra::writeRaster(mnp_rast,file=File,overwrite=T))

        mnp_rast
    }else{
        terra::rast(File)
    }
}))

mnp_rast<-app(mnp_stack,mean,na.rm=T)
names(mnp_rast)<-"med_d_mbps"

# Aggregate to 10km cells, create binary mask of ookla data vs no data
mask<-mnp_rast
mask<-terra::aggregate(mask,factor=2,fun=max,na.rm=T)
mask_da<-terra::disagg(mask,fact=2)
mask_da[mask_da==0]<-NA
mask_da[mask_da>0]<-1
mask_da[is.na(mask_da)]<-0
mask_da<-terra::mask(terra::crop(mask_da,adm1_africa),adm1_africa)*water_mask
terra::plot(mask_da)

# Change to projected CRS
mnp_rast_pr<-terra::project(mnp_rast,"+proj=merc +datum=WGS84 +units=km",method="near")

# Convert rasterized data back to points
mnp_points<-as.points(mnp_rast_pr, values=TRUE, na.rm=TRUE)

dat2 <- data.frame(geom(mnp_points)[, c("x", "y")], as.data.frame(mnp_points))

# Create null model
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(mnp_points$med_d_mbps), mnp_points$med_d_mbps)
null

if(F){
# Nearest Neighbour
gs <- gstat(formula=med_d_mbps~1, locations=~x+y, data=dat2, nmax=5, set=list(idp = 0))
nn <- interpolate(mnp_rast_pr, gs, debug.level=0)

nn<-nn[[1]]
# Convert back to lat/lon
nn<-terra::project(nn,crs(mnp_rast))
# Crop and mask to SSA
nn<-terra::mask(terra::crop(nn,adm1_africa),adm1_africa)*water_mask

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
}
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
idw_opt<-terra::mask(terra::crop(idw_opt,adm1_africa),adm1_africa)*water_mask

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
if(F){
gs <- gstat::gstat(formula=med_d_mbps~1, locations=~x+y, data=dat2)
mnp <- terra::interpolate(mnp_rast_pr, gs, debug.level=0)
mnp<-mnp[[1]]
# Convert back to lat/lon
mnp_rp<-terra::project(mnp,crs(mnp_rast))
# Crop and mask to SSA
mnp_rp<-terra::mask(terra::crop(mnp_rp,adm1_africa),adm1_africa)*water_mask
names(mnp_rp)<-"Mobile_Internet_Access"
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
}

# Save data
data<-idw_opt
names(data)<-"Mobile_Internet_Access"
terra::writeRaster(data,paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

# Reclass areas without ookla data
data_m<-data*mask_da
names(data_m)<-"Mobile_Internet_Access_masked"
terra::writeRaster(data_m,paste0(DataDirInt,"/",names(data_m),".tif"),overwrite=T)
# Set 0 values to NA
data_m_na<-data_m
data_m_na[data_m_na==0]<-NA
names(data_m_na)<-"Mobile_Internet_Access_coverage_only"
terra::writeRaster(data_m,paste0(DataDirInt,"/",names(data_m_na),".tif"),overwrite=T)

# Generate manual breakpoints
m_reclass<-cbind(c(0,0.000000000001,10.0000001),
                 c(0,10,99999999),
                 c(0,1,2))
data_m_reclass<-terra::classify(data_m,m_reclass)
levels(data_m_reclass)<-c("low","medium","high")
names(data_m_reclass)<-paste(names(data_m_reclass),"_manclass")

terra::plot(c(data,data_m,data_m_reclass))
suppressWarnings(terra::writeRaster(data_m_reclass,file=paste0(DataDirInt,"/",names(data_m_reclass),".tif"),overwrite=T))
                
# Generate terciles for non-masked data
# Adaptive capacity is classified thus: high = good, low = bad
# High download speed is better, lower tercile = bad, upper tercile = good 
Invert<-F

devtools::source_url("https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/functions.R?raw=TRUE")

data_terciles<-quantile_split(data,
                              Labels=c("low","medium","high"),
                              Invert=Invert,
                              Overwrite=T,
                              do_binary_splits=F,
                              savedir=DataDirInt)

Files<-list.files(DataDirInt,".tif",full.names=T)
Files<-Files[!grepl("aux",Files)]
terra::plot(terra::rast(Files))

# Generate 2 classes for masked data
Labels<-c("medium","high")
Values<-c(1,2)

X<-data_m
N<-which(data_m[]<0.1)
X[X<0.1]<-NA
vTert = quantile(X[], c(0:2/2),na.rm=T)
Intervals<-data.frame(Intervals=apply(cbind(c("low",Labels),c(0,round(vTert[1:2],3)),c(0.1,round(vTert[2:3],3))),1,paste,collapse="-"))
write.csv(Intervals,file=paste0(DataDirInt,"/",names(X),".csv"),row.names=F)
        
Terciles<-cut(X[],
          vTert, 
          include.lowest = T, 
          labels = Values)
 
X[]<-as.numeric(Terciles)
X[N]<-0
levels(X)<-c("low","medium","high")
names(X)<-paste0(names(X),"_duociles")
plot(X)

suppressWarnings(terra::writeRaster(X,file=paste0(DataDirInt,"/",names(X),".tif"),overwrite=T))

terra::plot(c(data,data_terciles,data_m_reclass,X))

