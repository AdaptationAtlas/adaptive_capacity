require(terra)
require(gstat)
require(fields) 

options(scipen = 999)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_poverty_IWI/intermediate")
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

# Load data
Files<-list.files(paste0(DataDir,"/atlas_poverty_IWI/raw"),".csv",full.names=T)

dat<-do.call("rbind",lapply(1:length(Files),FUN=function(i){
    read.csv(Files[i])
    }))

# Create a vector of the points
dat_points<-terra::vect(dat[,c("lat","lon","estimated_IWI")],crs='+proj=longlat +datum=WGS84 +no_defs')

dat_points_pr<-terra::project(dat_points,"+proj=merc +datum=WGS84 +units=km")

x_cols<-ceiling((ext(dat_points_pr)$xmax-ext(dat_points_pr)$xmin)/(1.85*5))
y_rows<-ceiling((ext(dat_points_pr)$ymax-ext(dat_points_pr)$ymin)/(1.85*5))

rast_blank<-terra::rast(dat_points_pr,ncol=x_cols,nrow=y_rows)

# Rasterize points to resolution of base_raster, average values across points in the same cell
dat_rast<-terra::rasterize(dat_points_pr,rast_blank,field="estimated_IWI",method="average")
names(dat_rast)<-"IWI"

# Convert rasterized data back to points
dat_points2<-as.points(dat_rast, values=TRUE, na.rm=TRUE)

# Create a data.frame for use with kriging model
dat2 <- data.frame(geom(dat_points2)[, c("x", "y")], as.data.frame(dat_points2))

# Optimize IDW parameters - https://rspatial.org/terra/analysis/4-interpolation.html
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

f1 <- function(x, test, train) {
  nmx <- x[1]
  idp <- x[2]
  if (nmx < 1) return(Inf)
  if (idp < .001) return(Inf)
  m <- gstat::gstat(formula=IWI~1, locations=~x+y, data=train, nmax=nmx, set=list(idp=idp))
  p <- predict(m, newdata=test, debug.level=0)$var1.pred
  RMSE(test$IWI, p)
}

kf <- sample(1:5, nrow(dat2), replace=TRUE)

opt<-lapply(1:5,FUN=function(k){
    test <- dat2[kf == k, ]
    train <- dat2[kf != k, ]
    opt <- optim(c(8, .5), f1, test=test, train=train)
    opt
})

opt_par<-do.call("rbind",lapply(opt,"[[","par"))
opt_par<-apply(opt_par,2,median)

m <- gstat(formula=IWI~1, locations=~x+y, data=dat2, nmax=opt_par[1], set=list(idp=opt_par[2]))
idw_opt <- terra::interpolate(dat_rast, m, debug.level=0)
idw_opt<-terra::resample(idw_opt,base_raster,method="average")
idw_opt<-terra::mask(terra::crop(idw_opt,adm1_africa),adm1_africa)*water_mask

# Standard IDW model without interpolation
gs <- gstat::gstat(formula=IWI~1, locations=~x+y, data=dat2)
idw <- terra::interpolate(dat_rast, gs, debug.level=0)
idw<-idw[[1]]

idw<-terra::resample(idw,base_raster,method="average")
idw<-terra::mask(terra::crop(idw,adm1_africa),adm1_africa)*water_mask

terra::plot(c(idw,idw_opt))

# Cross-validate optimized IDW model

# Create null model
null <- RMSE(mean(dat_points2$IWI), dat_points2$IWI)
null

rmse <- list(rep(NA, 5),rep(NA, 5))

kf <- sample(1:5, nrow(dat2), replace=TRUE)
for (k in 1:5) {
  test <- dat2[kf == k, ]
  train <- dat2[kf != k, ]
  gs <- gstat(formula=IWI~1, locations=~x+y, data=train, nmax=opt_par[1], set=list(idp=opt_par[2]))
  p <- predict(gs, test, debug.level=0)
  rmse[[1]][k] <- RMSE(test$IWI, p$var1.pred)

  gs <- gstat(formula=IWI~1, locations=~x+y, data=train)
  p <- predict(gs, test, debug.level=0)
  rmse[[2]][k] <- RMSE(test$IWI, p$var1.pred)
  
}

# Optimized IDW
mean(rmse[[1]])
1 - (mean(rmse[[1]]) / null)

# IDW
mean(rmse[[2]])
1 - (mean(rmse[[2]]) / null)


data<-idw_opt[[1]]
names(data)<-"poverty_IWI"
terra::writeRaster(data,paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

# Generate terciles
Overwrite<-T
# Adaptive capacity is classified thus: high = good, low = bad
# High wealth is better, lower tercile = bad, upper tercile = good 
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

hist(data[])


# *********the below kriging crashes the kernel**********
# Fit a variogram: Use gstat to create an emperical variogram ‘v’
gs<- gstat::gstat(formula=IWI~1, locations=~x+y, data=dat2)
v <- gstat::variogram(gs,width=100)
plot(v)

fve <- gstat::fit.variogram(v, gstat::vgm(psill=145, "Sph", range=2000,nugget=75))

plot(gstat::variogramLine(fve, 3000), type='l', ylim=c(0,200) ,col='blue', lwd=2)
points(v[,2:3], pch=20, col='red')

# Ordinary kriging: Use variogram fve in a kriging interpolation
k <- gstat::gstat(formula=IWI~1, locations=~x+y, data=dat2, model=fve)

# predicted values
kp <- interpolate(dat_rast, k, debug.level=0)

