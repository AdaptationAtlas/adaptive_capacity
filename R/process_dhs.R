require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# dhs data location
DHSDir<-paste0(DataDir,"/atlas_dhs/intermediate")

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# Read in DHS data
dhs_data<- read.csv(paste0(DHSDir,"/data.csv"))

dhs_polys<-merge(adm1_africa,dhs_data,all.x=TRUE, by.x='HASC_1', by.y='HASC_1')

fields<-colnames(dhs_data)
fields<-fields[!grepl("HASC_1|ISO3",fields)]

dhs_data<-terra::rast(lapply(fields,FUN=function(FIELD){
    File<-paste0(DHSDir,"/",FIELD,".tif")
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

dhs_data_terciles<-terra::rast(lapply(names(dhs_data),FUN=function(FIELD){
    
    File<-paste0(DHSDir,"/",FIELD,"_terciles.tif")
    
    if(!file.exists(File)){
        X<-dhs_data[[FIELD]]
        vTert = quantile(X[], c(0:3/3),na.rm=T)
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = c(0,1,2))
        X[]<-as.numeric(Terciles)-1
        levels(X)<-c("low","medium","high")    
        suppressWarnings(terra::writeRaster(X,file=File))
        X
        }else{
            terra::rast(File)
    }
}))

