require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_financial_services/intermediate")
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

# Load datasets
data_vect<-terra::vect(paste0(DataDir,"/atlas_financial_services/raw/DHS_bankaccount_SSA_v1.shp"))
# HV247_p = the ratio of DHS households in the administrative unit that have a bank account)
terra::plot(data_vect,"HV247_p")

# Set south africa to NA
data_vect$HV247_p[data_vect$CNTRYNAMEE=="South Africa"]<-NA

# Rasterize
data<-terra::rasterize(data_vect,base_raster,field="HV247_p")
terra::plot(data)
names(data)<-"bank_account_pr"

# Mask data
data<-terra::mask(data,adm1_africa)
data<-data*water_mask

lapply(names(data),FUN=function(FIELD){
    File<-paste0(DataDirInt,"/",FIELD,".tif")
    suppressWarnings(terra::writeRaster(data[[FIELD]],file=File,overwrite=T))
    })


Overwrite=T
# Adaptive capacity is classified thus: high = good, low = bad
# A high proportion of people with bank accounts is better, lower tercile = bad, upper tercile = good 
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
