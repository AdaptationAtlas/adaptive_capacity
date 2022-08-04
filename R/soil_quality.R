DataDir<-"/home/jovyan/common_data"
ISDADir<-paste0(DataDir,"/isda/raw")

ISDADirInt<-paste0(DataDir,"/isda/intermediate/atlas")
if(!dir.exists(ISDADirInt)){
    dir.create(ISDADirInt,recursive=T)
    }

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# Atlas base raster to resample to
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_V_ACOF_A.tif"))

# https://link.springer.com/article/10.1007/s10705-017-9870-x
# Macronutrients
MacroN<-c("p","k","n")

#https://www.fao.org/3/a0443e/a0443e.pdf
Thresholds<-list(c(5,9,17,25,9999),list(50,100,175,300,9999))

# See table 11 page59                 
MicroN<-c("fe","zn")

Files<-list.files(ISDADir,paste0("log.",MacroN,collapse="|"),full.names=T)

for(File in Files){
    soildata<-terra::rast(File)
    soildata<-terra::crop(soildata,sh_africa,file=paste0(ISDADirInt,"/temp1.tif"),overwrite=TRUE)
    soildata<-terra::mask(soildata,sh_africa,file=paste0(ISDADirInt,"/temp2.tif"),overwrite=TRUE)
    soildata<-terra::resample(soildata,base_raster,fun="mean",file=gsub(ISDADir,ISDADirInt,File))
    unlink(paste0(ISDADirInt,"/temp1.tif"))
    unlink(paste0(ISDADirInt,"/temp2.tif"))
    }

# Units should be ppm
N<-terra::rast(Files[grep("log.n",Files)])
N<-terra::mask(terra::crop(N,sh_africa),sh_africa)

N<-terra::weighted.mean(N,w=c(2,3),na.rm=T)
P<-terra::rast(Files[grep("log.p",Files)])
K<-terra::rast(Files[grep("log.k",Files)])

