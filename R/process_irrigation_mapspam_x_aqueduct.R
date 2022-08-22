# Intersect crop VoP with smallholder size classes
require(data.table)
require(terra)

# Set data directory
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_mapspam/intermediate/irrigation")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt,recursive=T)
    }
    

# Read in boundary of subsaharan africa
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
irr_area<-terra::mask(terra::crop(irr_area,adm1_africa),adm1_africa)
irr_area<-sum(irr_area)

# All: Stack, crop, mask and sum
tot_area<-terra::rast(paste0(CropDir,"/",files_all))
tot_area<-terra::mask(terra::crop(tot_area,adm1_africa),adm1_africa)
tot_area<-tot_area * water_mask
tot_area<-sum(tot_area)

# Calculate % of total area that is irrigated
irrig_pr<-100*irr_area/tot_area
names(irrig_pr)<-"irrig_prop"
# terra::writeRaster(irrig_pr,paste0(DataDirInt,"/irrig_prop.tif"),overwrite=T)

irrig_pr[irrig_pr==0]<-NA

# Classify irrigated area into terciles

Values<-c(low=1,medium=2,high=3)

irrig_pr_cl<-irrig_pr
vTert = quantile(irrig_pr_cl[], c(0:3/3),na.rm=T)
Intervals<-data.frame(Intervals=apply(cbind(names(Values),round(vTert[1:3],3),round(vTert[2:4],3)),1,paste,collapse="-"))
Terciles<-cut(irrig_pr_cl[],
              vTert, 
              include.lowest = T, 
              labels = Values)
irrig_pr_cl[]<-as.numeric(Terciles)

N<-!is.na(tot_area[]) & is.na(irrig_pr_cl[])
irrig_pr_cl[][N]<-0
levels(irrig_pr_cl)<-c("0 none","1 low","2 med","3 high")

terra::plot(irrig_pr_cl)


# Read in water stress
# see https://github.com/AdaptationAtlas/adaptive_capacity/blob/main/R/process_aqueduct.R for how to generate the layer below
wstress<-terra::rast(paste0(DataDir,"/atlas_aqueduct_3/intermediate/bws_cat.tif"))
terra::plot(wstress)

# Reclassify into three classes + zero
# -1 Arid and Low Water Use  -> 0 (not applicable)
# 0 Low (<10%) = 10 (low capacity) -> 30 (high capacity)    
# 1 Low - Medium (10-20%) -> 30 (high capacity)
# 2 Medium - High (20-40%) -> 20 (medium capacity)
# 3 High (40-80%) -> 10 (low capacity)
# 4 Extremely High (>80%) -> 10 (low capacity)

wstress_cl<-terra::classify(wstress,cbind(-1:4,c(0,30,30,20,10,10)))
terra::plot(wstress_cl)

# Combine two classifications
irrig_wstress<-irrig_pr_cl+wstress_cl
names(irrig_wstress)<-"irrig_comb"
terra::plot(irrig_wstress)

# No irrigation = 0 (low)
# Arid = NA
# Irrigation>0 then classification depends on water stress:
    # 10 = 0 (high stress, low capacity)
    # 20 = 1(moderate stress, med capacity)
    # 30 = 2 (low stress high capacity)
irrig_wstress_cl<-terra:::classify(irrig_wstress,
                                   cbind(c(0,10,20,30,1,11,21,31,2,12,22,32,3,13,23,33),
                                         c(NA,0,0,0,NA,0,1,2,NA,0,1,2,NA,0,1,2)))


names(irrig_wstress_cl)<-"irrigation_suitability"
levels(irrig_wstress_cl)<-c("low","medium","high")
terra::writeRaster(irrig_wstress_cl,paste0(DataDirInt,"/irrigation.tif"),overwrite=T)



terra::plot(c(irrig_pr,wstress))
terra::plot(c(irrig_pr_cl,wstress_cl))
terra::plot(irrig_wstress_cl)

# Not run
if(F){
# Generate two classes from irrigated area data
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
}