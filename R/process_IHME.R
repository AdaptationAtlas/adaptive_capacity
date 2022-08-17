require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_IHME/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Load datasets
stunting<-terra::rast(paste0(DataDir,"/atlas_IHME/raw/IHME_AFRICA_CGF2000_2015_STUNTING_MEAN_2015_PREV.tiff"))
wasting<-terra::rast(paste0(DataDir,"/atlas_IHME/raw/IHME_AFRICA_CGF_2000_2015_WASTING_MEAN_2015_PREV.tiff"))

data<-c(stunting,wasting)
names(data)<-c("child_stunting","child_wasting")

# Resample data
data<-terra::resample(data,base_raster,method="average")

lapply(names(data),FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    suppressWarnings(terra::writeRaster(data[[FIELD]],file=File,overwrite=T))
    })

Overwrite<-T

# Adaptive capacity is classified thus: high = good, low = bad
# Low stunting is better, lower tercile = good, upper tercile = bad 
Levels<-c("high","medium","low")

data_terciles<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
    File<-paste0(DataDirInt,"/",FIELD,"_terciles.tif")
    
    if((!file.exists(File))|Overwrite){
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:3/3),na.rm=T)
        Intervals<-data.frame(Intervals=apply(cbind(Levels,round(vTert[1:3],3),round(vTert[2:4],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(DataDirInt,"/",FIELD,"_terciles.csv"),row.names=F)
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = c(0,1,2))
        X[]<-as.numeric(Terciles)-1
        levels(X)<-Levels 
        suppressWarnings(terra::writeRaster(X,file=File,overwrite=T))
        X
        }else{
            terra::rast(File)
    }
}))

terra::plot(data_terciles)
