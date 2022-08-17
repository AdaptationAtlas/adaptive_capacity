# Intersect crop VoP with smallholder size classes
require(data.table)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/mapspam_2017/intermediate/atlas_irrigation")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt,recursive=T)
    }
    

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Set directory for crops data
CropDir<-paste0(DataDir,"/mapspam_2017/raw")
# Set save directory for livestock x smallholder data
CropDirInt<-paste0(DataDir,"/mapspam_2017/intermediate/atlas_smallholders")
if(!dir.exists(CropDirInt)){
    dir.create(CropDirInt,recursive=T)
    }

# Load crop meta-data
MS_metadata<-data.table::fread(paste0(CropDir,"/mapspam_meta.csv"))

# Convert Crop VoP per pixel to per ha of harvested area ####

# Create a list of crop groupings
total<-MS_metadata[,unique(Commodity)]

# Harvested area files

# Irrigated
files_irr<-MS_metadata[Variable=="physical area" & Subvarible == "irrigated",File]
# All technologies
files_all<-MS_metadata[Variable=="physical area" & Subvarible == "all technologies",File]

# Irrigated: Stack, crop, mask and sum
irr_area<-terra::rast(paste0(CropDir,"/",files_irr))
irr_area<-terra::mask(terra::crop(irr_area,sh_africa),sh_africa)
irr_area<-sum(irr_area)

# All: Stack, crop, mask and sum
tot_area<-terra::rast(paste0(CropDir,"/",files_all))
tot_area<-terra::mask(terra::crop(tot_area,sh_africa),sh_africa)
tot_area<-sum(tot_area)

# Calculate % of total area that is irrigated
data<-100*irr_area/tot_area
names(data)<-"irrigated_area"
terra::writeRaster(data,paste0(DataDirInt,"/irrigation.tif"),overwrite=T)

data[data==0]<-NA

# Read in water stress



# Generate two classes
Overwrite<-T
# Adaptive capacity is classified thus: high = good, low = bad
Invert<-F

Labels<-c("medium","high")
Values<-c(0,1)

data_terciles<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:2/2),na.rm=T)

       Levels<-if(Invert){Labels[length(Labels):1]}else{Labels}
                
        Intervals<-data.frame(Intervals=apply(cbind(Levels,round(vTert[1:2],2),round(vTert[2:3],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(DataDirInt,"/",FIELD,"_terciles.csv"),row.names=F)
        
        Terciles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = Values)
        
        X[]<-as.numeric(Terciles)-1
        levels(X)<-  Levels  
        X
}))

terra::plot(data_terciles)

data_terciles[data_terciles==1]<-2
data_terciles[data_terciles==0]<-1

N<-!is.na(tot_area[]) & is.na(data_terciles[])
data_terciles[][N]<-0
levels(data_terciles)<-c("low","medium","high")
terra::plot(data_terciles)
suppressWarnings(terra::writeRaster(data_terciles,file=paste0(DataDirInt,"/",names(data_terciles),"_terciles.tif"),overwrite=T))
