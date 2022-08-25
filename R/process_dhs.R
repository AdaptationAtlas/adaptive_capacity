require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# dhs data location
DataDirInt<-paste0(DataDir,"/atlas_dhs/intermediate")

# Read in admin1 for subsaharan africa
adm1_africa36<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm36_ssa_1.shp"))

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
dhs_polys<-terra::merge(adm1_africa36,dhs_data,all.x=TRUE, by.x='HASC_1', by.y='HASC_1')

# Check for missing values
Dat<-data.frame(dhs_polys)
Dat[is.na(Dat$reproHealth_2015),c("HASC_1","GID_0","NAME_0","NAME_1")]

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


# Adaptive capacity is classified thus: high = good, low = bad
# A high proportion of people with bank accounts is better, lower tercile = bad, upper tercile = good 
Invert=F

devtools::source_url("https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/functions.R?raw=TRUE")

data_terciles<-quantile_split(data,
                              Labels=c("low","medium","high"),
                              Invert=Invert,
                              Overwrite=T,
                              do_binary_splits=T,
                              savedir=DataDirInt)

terra::plot(c(data[[1]],data_terciles[[1]]))

# create subdirectory for atlas specific files
DHSDir2<-paste0(DataDir,"/atlas_dhs/intermediate/atlas_subset")
if(!dir.exists(DHSDir2)){
    dir.create(DHSDir2)
}

Files<-list.files(DataDirInt,full.names=T,recursive=F)
Files<-grep("decisions_2015",Files,value=T)


file.copy(Files, DHSDir2,overwrite=T)

Files<-list.files(DHSDir2,full.names=T)
file.rename(Files, gsub("decisions_2015_","Gender_Equity-",Files))
Files<-list.files(DHSDir2,full.names=T)
file.rename(Files, gsub("decisions_2015","Gender_Equity",Files))

Files<-list.files(DHSDir2,".tif",full.names=T)
Files<-Files[!grepl("aux",Files)]
terra::plot(terra::rast(Files))
