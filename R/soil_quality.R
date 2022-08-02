DataDir<-"/home/jovyan/common_data"
ISDADir<-paste0(DataDir,"/isda/raw")

# Read in boundary of subsaharan africa
sh_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadml0_4326_agg.shp"))

# https://link.springer.com/article/10.1007/s10705-017-9870-x

# Macronutrients
MacroN<-c("p","k","n")

#https://www.fao.org/3/a0443e/a0443e.pdf
Thresholds<-list(c(5,9,17,25,9999),list(50,100,175,300,9999))

# See table 11 page59                 
MicroN<-c("fe","zn")

Files<-list.files(ISDADir,full.names=T)

# Units should be ppm
N<-terra::rast(Files[grep("log.n",Files)])
N<-terra::mask(terra::crop(N,sh_africa),sh_africa)

N<-terra::weighted.mean(N,w=c(2,3),na.rm=T)
P<-terra::rast(Files[grep("log.p",Files)])
K<-terra::rast(Files[grep("log.k",Files)])

