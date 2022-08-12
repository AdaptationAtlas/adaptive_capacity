require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_education/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Load datasets
data<-terra::rast(paste0(DataDir,"/atlas_education/raw/shdi-2018-education.tif"))

# It seem 0 is an NA value is this dataset
data[data==0]<-NA

names(data)<-"education"

# Resample data
data<-terra::resample(data,base_raster,method="average")

lapply(names(data),FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    suppressWarnings(terra::writeRaster(data[[FIELD]],file=File,overwrite=T))
    })

data_terciles<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
    File<-paste0(DataDirInt,"/",FIELD,"_terciles.tif")
    
    if(!file.exists(File)){
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:3/3),na.rm=T)
        Intervals<-data.frame(Intervals=apply(cbind(c("high","medium","low"),round(vTert[1:3],3),round(vTert[2:4],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(DataDirInt,"/",FIELD,"_terciles.csv"),row.names=F)
        
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = c(0,1,2))
        X[]<-as.numeric(Terciles)-1
        levels(X)<-c("high","medium","low")    
        suppressWarnings(terra::writeRaster(X,file=File,overwrite=T))
        X
        }else{
            terra::rast(File)
    }
}))