require(terra)

# This openlab tutorial is highly relevant https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube/

DataDir<-"/home/jovyan/common_data"
ISDADir<-paste0(DataDir,"/isda/raw")

ISDADirInt<-paste0(DataDir,"/isda/intermediate/atlas")
if(!dir.exists(ISDADirInt)){
    dir.create(ISDADirInt,recursive=T)
    }

# Read in boundary of sub-saharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))
sh_africa<-terra::simplifyGeom(sh_africa, tolerance=0.1)

# Atlas base raster to resample to
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_V_ACOF_A.tif"))
base_raster<-terra::crop(terra::mask(base_raster,sh_africa),sh_africa)

# Layers to resample
ISDA_vars<-c("p","k","n","fe","zn","mg","ecec","oc")

# List paths for ISDA files
Files<-list.files(ISDADir,paste0("log.",ISDA_vars,collapse="|"),full.names=T)

# Aggregate and resample the 30m rasters to the base_raster resolution
for(File in Files){
    print(File)
    SaveFile<-gsub(".tif","_resamp.tif",gsub(ISDADir,ISDADirInt,File))
    if(!file.exists(SaveFile)){
        soildata<-terra::rast(File)
        soildata<-terra::aggregate(soildata,fact=50,fun="mean")
        terra::resample(soildata,base_raster,method="average",file=SaveFile)
     }
    }

# Stack resampled files, mask, crop and rename them 
Files<-gsub(".tif","_resamp.tif",gsub(ISDADir,ISDADirInt,Files))

sn_stack<-terra::rast(Files)
sn_stack<-terra::mask(terra::crop(sn_stack,sh_africa),sh_africa)

names(sn_stack)<-gsub("_mehlich3","",gsub("_2001..2017_v0.13_wgs84","",gsub("_m_30m","",gsub("sol_","",names(sn_stack)))))

# Back-transform values from log-scale to ppms (exclude nitrogen)
sn_stack<-terra::rast(lapply(names(sn_stack),FUN=function(LAYER){
    if(!grepl("log.n",LAYER)){
        X<-sn_stack[LAYER]
        X<-expm1(X/10) # computes exp(x) - 1 accurately also for |x| << 1.
                
        names(X)<-if(grepl("log.oc",LAYER)){gsub("log","perc",LAYER)}else{gsub("log","ppm",LAYER)}
        X
    }else{
        sn_stack[LAYER]
    }
       }))

# Take weighted mean of the two depths
layers<-unique(stringi::stri_replace_all_regex(names(sn_stack),c("_0..20cm","_20..50cm"),""))

sn_stack_wm<-terra::rast(lapply(layers,FUN=function(LAYER){
    layers<-grep(LAYER,names(sn_stack))
    X<-terra::weighted.mean(sn_stack[[layers]],w=c(0.4,0.6))
    names(X)<-LAYER
    X
    }))

# Classify soil nutrients

# https://link.springer.com/article/10.1007/s10705-017-9870-
# https://www.fao.org/3/a0443e/a0443e.pdf
# See table 11 page59 
# To convert cmol/kg to ppm see https://www.growyoursoil.org/Base-Saturation.pdf

thresholds<-list(avail_P=c(very_low=5,low=9,medium=17,high=25,very_high=9999), # Available P ppm - Table 13 https://www.fao.org/3/a0443e/a0443e.pdf p78    
                 avail_K=c(very_low=50,low=100,medium=175,high=300,very_high=9999), # Available K ppm - Table 13 https://www.fao.org/3/a0443e/a0443e.pdf p78
                 avail_Mg=c(very_low=20,low=40,medium=80,high=180,very_high=9999), # Available Mg ppm - Table 13 https://www.fao.org/3/a0443e/a0443e.pdf p78
                 exch_Mg=c(very_low=0.3,low=1,medium=3,high=8,very_high=9999),  # Exchangeable Mg cmol/kg - Table 11 https://www.fao.org/3/a0443e/a0443e.pdf p59
                 exch_Ca=c(very_low=2,low=5,medium=10,high=20,very_high=9999), # Exchangeable Ca cmol/kg - Table 11 https://www.fao.org/3/a0443e/a0443e.pdf p59
                 exch_K=c(very_low=0.2,low=0.3,medium=0.6,high=1.2,very_high=9999), # Exchangeable K cmol/kg - Table 11 https://www.fao.org/3/a0443e/a0443e.pdf p59
                 exch_Na=c(very_low=0.1,low=0.3,medium=0.7,high=2,very_high=9999), # Exchangeable Na cmol/kg - Table 11 https://www.fao.org/3/a0443e/a0443e.pdf p59
                 CEC=c(very_low=6,low=12,medium=25,high=40,very_high=9999), # Exchangeable CEC cmol/kg - Table 11 https://www.fao.org/3/a0443e/a0443e.pdf p59
                 oc=c(low=10,medium=20,high=9999) # Organic carbon g/kg - Pers comms Todd Rosenstock
                ) 

# *************************************************
# Classify organic carbon
# Fixed breaks
oc_class<-terra::classify(sn_stack_wm$perc.oc,
                         rcl=as.matrix(data.frame(from=c(0,thresholds$oc[1:2]),
                                                  to=thresholds$oc,
                                                  becomes=0:2)))
levels(oc_class)<-names(thresholds$oc)
terra::plot(oc_class)

# Terciles
data<-sn_stack_wm$perc.oc
names(data)<-gsub("[.]","_",names(data))

DataDirInt<-paste0(ISDADirInt,"/atlas_subset")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

terra::writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"))

Overwrite=T
# Adaptive capacity is classified thus: high = good, low = bad
# High soil carbon is better, lower tercile = bad, upper tercile = good 
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

terra::plot(data_terciles)


# *************************************************
# Classify phosphorus
P_class<-terra::classify(sn_stack_wm$ppm.p,
                         rcl=as.matrix(data.frame(from=c(0,thresholds$avail_P[1:4]),
                                                  to=thresholds$avail_P,
                                                  becomes=0:4)))
levels(P_class)<-names(thresholds$avail_P)

# Classify potassium
K_class<-terra::classify(sn_stack_wm$ppm.k,
                         rcl=as.matrix(data.frame(from=c(0,thresholds$avail_K[1:4]),
                                                  to=thresholds$avail_K,
                                                  becomes=0:4)))
levels(K_class)<-names(thresholds$avail_K)

# Classify magnesium
Mg_class<-terra::classify(sn_stack_wm$ppm.mg,
                         rcl=as.matrix(data.frame(from=c(0,thresholds$avail_Mg[1:4]),
                                                  to=thresholds$avail_Mg,
                                                  becomes=0:4)))
levels(Mg_class)<-names(thresholds$avail_Mg)
