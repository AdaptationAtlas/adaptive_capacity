require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_healthcare_access/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }
    
# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in market access data
hc_access_walking<-terra::rast(paste0(DataDir,"/atlas_healthcare_access/raw/2020_walking_only_travel_time_to_healthcare.geotiff"))
hc_access_motor<-terra::rast(paste0(DataDir,"/atlas_healthcare_access/raw/2020_motorized_travel_time_to_healthcare.geotiff"))

data<-c(hc_access_walking,hc_access_motor)
names(data)<-c("healthcare_access_walking","healthcare_access_motorized")

# Resample
data<-terra::resample(data,base_raster,method="average")

# Crop and mask
data<-terra::mask(terra::crop(data,adm1_africa),adm1_africa)

# Save data
terra::writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

# Generate terciles
Overwrite=T
# Adaptive capacity is classified thus: high = good, low = bad
# Low distance to healthcare is better, lower tercile = good, upper tercile = bad 
Invert<-T

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