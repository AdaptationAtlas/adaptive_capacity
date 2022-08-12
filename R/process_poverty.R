require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_poverty/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Load datasets
data<-terra::vect(paste0(DataDir,"/atlas_poverty/raw/extreme-poverty.geojson"))

# Rasterize data
data<-terra::rasterize(data,base_raster,field="poverty_mean")

names(data)<-"poverty"

lapply(names(data),FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    suppressWarnings(terra::writeRaster(data[[FIELD]],file=File,overwrite=T))
    })

data_terciles<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
    File<-paste0(DataDirInt,"/",FIELD,"_terciles.tif")
    
    if(!file.exists(File)){
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:3/3),na.rm=T)
        Intervals<-data.frame(Intervals=apply(cbind(c("low","medium","high"),round(vTert[1:3],3),round(vTert[2:4],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(DataDirInt,"/",FIELD,"_terciles.csv"),row.names=F)
        
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = c(0,1,2))
        X[]<-as.numeric(Terciles)-1
        levels(X)<-c("low","medium","high")    
        suppressWarnings(terra::writeRaster(X,file=File,overwrite=T))
        X
        }else{
            terra::rast(File)
    }
}))