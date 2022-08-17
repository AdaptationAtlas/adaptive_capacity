require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# dhs data location
DataDirInt<-paste0(DataDir,"/atlas_dhs/intermediate")

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in DHS data
dhs_data<- read.csv(paste0(DataDirInt,"/data.csv"))

dhs_polys<-merge(adm1_africa,dhs_data,all.x=TRUE, by.x='HASC_1', by.y='HASC_1')

fields<-colnames(dhs_data)
fields<-fields[!grepl("HASC_1|ISO3",fields)]

data<-terra::rast(lapply(fields,FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    if(!file.exists(File)){
        X<-terra::rasterize(dhs_polys,base_raster,field=FIELD)
        names(X)<-FIELD
        suppressWarnings(terra::writeRaster(X,file=File))
        X
    }else{
    terra::rast(File)
    }
}))

# Generate terciles
Overwrite<-T

# Adaptive capacity is classified thus: high = good, low = bad
# High empowerment is better, lower tercile = bad, upper tercile = good 
Levels<-c("low","medium","high")
    
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

# create subdirectory for atlas specific files
DHSDir2<-paste0(DataDir,"/atlas_dhs/intermediate/atlas_subset")
if(!dir.exists(DHSDir2)){
    dir.create(DHSDir2)
}

Files<-list.files(DataDirInt,full.names=T,recursive=F)
Files<-grep("decisions_2015|index_2015",Files,value=T)


file.copy(Files, DHSDir2,overwrite=T)

Files<-list.files(DHSDir2,".tif",full.names=T)
Files<-Files[!grepl("aux",Files)]
terra::plot(terra::rast(Files))
