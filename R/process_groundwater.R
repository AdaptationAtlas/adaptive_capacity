require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_groundwater/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }
    
# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in recharge data
recharge<-terra::rast(paste0(DataDir,"/atlas_groundwater/raw/Africa_recharge.tif"))
recharge<-terra::resample(recharge,base_raster)
recharge<-terra::mask(terra::crop(recharge,adm1_africa),adm1_africa)

# Generate sextiles

Levels<-c("very low","low","medium low","medium high","high","very high")
Values<-c(0,1,2,3,4,5)

vTert = quantile(recharge[], c(0:6/6),na.rm=T)
Intervals<-data.frame(Intervals=apply(cbind(Levels,round(vTert[1:6],3),round(vTert[2:7],3)),1,paste,collapse="-"))

Sextiles<-cut(recharge[],
              vTert, 
              include.lowest = T, 
              labels = Values)

recharge_class<-recharge
recharge_class[]<-as.numeric(Sextiles)-1
levels(recharge_class)<-  Levels  
terra::plot(c(recharge,recharge_class))

# Read in depth to groundwater data
depth_gw<-read.delim(paste0(DataDir,"/atlas_groundwater/raw/xyzASCII_dtwmap_v1.txt"),header = TRUE, sep = "\t", dec = ".")

depth_gw$values<-factor(depth_gw$DTWAFRICA_,levels=rev(c("VS","S","SM","M","D","VD")))
depth_gw$values<-as.numeric(depth_gw$values)-1
depth_gw_vals<-unique(depth_gw[,3:4])
depth_gw_rast<-terra::rast(depth_gw[,c(1,2,4)])
names(depth_gw_rast)<-"depth_to_groundwater"
terra::plot(depth_gw_rast)

terra::crs(depth_gw_rast)<-terra::crs(base_raster)
depth_gw_rast<-terra::resample(depth_gw_rast,base_raster)
depth_gw_rast<-terra::mask(terra::crop(depth_gw_rast,adm1_africa),adm1_africa)

# Read in groundwater storage data
gw_st<-read.delim(paste0(DataDir,"/atlas_groundwater/raw/xyzASCII_gwstor_v1.txt"),header = TRUE, sep = "\t", dec = ".")
gw_st$values<-factor(gw_st$GWSTOR_V2,levels=c("0","L","LM","M","H","VH"))
gw_st$values<-as.numeric(gw_st$values)-1
gw_st_vals<-unique(gw_st[,3:4])
gw_st_rast<-terra::rast(gw_st[,c(1,2,4)])
names(gw_st_rast)<-"groundwater_storage"

terra::crs(gw_st_rast)<-terra::crs(base_raster)
gw_st_rast<-terra::resample(gw_st_rast,base_raster)
gw_st_rast<-terra::mask(terra::crop(gw_st_rast,adm1_africa),adm1_africa)

# Read in groundwater productivity data
gw_prod<-read.delim(paste0(DataDir,"/atlas_groundwater/raw/xyzASCII_gwprod_v1.txt"),header = TRUE, sep = "\t", dec = ".")
gw_prod$values<-factor(gw_prod$GWPROD_V2,levels=c("VL","L","LM","M","H","VH"))
gw_prod$values<-as.numeric(gw_prod$values)-1
gw_prod_vals<-unique(gw_prod[,3:4])
gw_prod_rast<-terra::rast(gw_prod[,c(1,2,4)])
names(gw_prod_rast)<-"groundwater_productivity"
terra::plot(gw_prod_rast)

terra::crs(gw_prod_rast)<-terra::crs(base_raster)
gw_prod_rast<-terra::resample(gw_prod_rast,base_raster)
gw_prod_rast<-terra::mask(terra::crop(gw_prod_rast,adm1_africa),adm1_africa)

# Stack rasters - productivity is derived from depth and storage, so it does not make sense to included all these rasters
gw_stk<-c(gw_prod_rast,recharge_class)

# Sum values
data<-sum(gw_stk)
names(data)<-"irrigation_suitability"
terra::plot(data)

suppressWarnings(terra::writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"),overwrite=T))

# Generate terciles
Overwrite=T
# Adaptive capacity is classified thus: high = good, low = bad
# High suitability is better, lower tercile = bad, upper tercile = good 
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

terra::plot(data_terciles)
terra::plot(c(data,data_terciles))


