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

# Read in waterbodies to create mask
waterbodies<-terra::vect(paste0(DataDir,"/atlas_surfacewater/raw/waterbodies_africa.shp"))
water_mask<-terra::rasterize(waterbodies,base_raster)
water_mask[!is.na(water_mask)]<-0
water_mask[is.na(water_mask)]<-1
water_mask[water_mask==0]<-NA
water_mask<-terra::mask(terra::crop(water_mask,adm1_africa),adm1_africa)

# Read in DHS data
dhs_data<- read.csv(paste0(DataDirInt,"/data.csv"))
dhs_polys<-merge(adm1_africa,dhs_data,all.x=TRUE, by.x='HASC_1', by.y='HASC_1')

# Check for missing values
if(F){
Dat<-data.frame(dhs_polys)
Dat[is.na(Dat$reproHealth_2015),c("GID_0","NAME_1","HASC_1")]

# dhs data has no value for DJ.AR
dhs_data[dhs_data$ISO3=="DJI",]
Dat[Dat$GID_0=="DJI" & !is.na(Dat$GID_0) & is.na(Dat$reproHealth_2015),]

# Ghana missing data are confirmed NAs in dhs data 
dhs_data[dhs_data$ISO3=="GHA",]
Dat[Dat$GID_0=="GHA" & !is.na(Dat$GID_0) & is.na(Dat$reproHealth_2015),]

# dhs data has no value for TZ.SO
dhs_data[dhs_data$ISO3=="TZA",]
Dat[Dat$GID_0=="TZA" & !is.na(Dat$GID_0) & is.na(Dat$reproHealth_2015),]

# Missing uganda values appear to be lakes
dhs_data[dhs_data$ISO3=="UGA",]
Dat[Dat$GID_0=="UGA" & !is.na(Dat$GID_0) & is.na(Dat$reproHealth_2015),]
}

# Create vector of dhs variable names
fields<-colnames(dhs_data)
fields<-fields[!grepl("HASC_1|ISO3",fields)]
Overwrite<-T

data<-terra::rast(lapply(fields,FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    if(!(file.exists(File))|Overwrite==T){
        X<-terra::rasterize(dhs_polys,base_raster,field=FIELD)
        names(X)<-FIELD
        # Mask out water bodies
        X<-X*water_mask
        suppressWarnings(terra::writeRaster(X,file=File,overwrite=T))
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
