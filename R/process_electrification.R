require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Create intermediate directory
DataDirInt<-paste0(DataDir,"/atlas_electrification/intermediate")
if(!dir.exists(DataDirInt)){
    dir.create(DataDirInt)
    }

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# **********************************************
# Population without access to electricity #####
# **********************************************

# Dividing population without access to electricity / estimated population per grid cell results in discrepancies in the final output (many values >1 or infinite) 
# We decided to aggregate data (access and population) to overcome this issue

# Load electrification data (pop without access)
# See https://www.nature.com/articles/s41597-019-0122-6 for methods
elec_na<-terra::rast(paste0(DataDir,"/atlas_electrification/raw/noaccess_SSA_2014_2020.nc"))

# Subset to most recent time point (2020)
# Unit is number of people without access to electricity
elec_na_20<-elec_na[[7]]

# Extract totals by admin1 area
elec_na_20_adm1<-terra::extract(elec_na_20,adm1_africa,fun=sum,na.rm=T)

# Load landscan or worldpop population data, note that you will need to manually download this from https://landscan.ornl.gov/ if it is not already available
# According to the methods the elec_na layer uses landscan data
pop20_ls<-terra::rast(paste0(DataDir,"/landscan_pop/raw/landscan-global-2020.tif"))
pop20_wp<-terra::rast(paste0(DataDir,"/worldpop/raw/ppp_2020_1km_Aggregated.tif"))

pop20_ls_adm1<-terra::extract(pop20_ls,adm1_africa,fun=sum,na.rm=T)
pop20_wp_adm1<-terra::extract(pop20_wp,adm1_africa,fun=sum,na.rm=T)

# Add extracted data to admin1 polygons
adm1_africa$elec20_na<-elec_na_20_adm1[,2]
adm1_africa$pop20_ls<-pop20_ls_adm1[,2]
adm1_africa$pop20_wp<-pop20_wp_adm1[,2]

# Calculate percentage without electricty
adm1_africa$elec_na_perc_ls<-adm1_africa$elec20_na/adm1_africa$pop20_ls
adm1_africa$elec_na_perc_wp<-adm1_africa$elec20_na/adm1_africa$pop20_wp

# Set infinite values to NA, this should be where population is nil
adm1_africa$elec_na_perc_ls[is.infinite(adm1_africa$elec_na_perc_ls)]<-NA
adm1_africa$elec_na_perc_wp[is.infinite(adm1_africa$elec_na_perc_wp)]<-NA

# Explore results
hist(adm1_africa$elec_na_perc_ls)
sum(adm1_africa$elec_na_perc_ls>1)

hist(adm1_africa$elec_na_perc_wp)
sum(adm1_africa$elec_na_perc_wp>1)

# Set values >1 to 1
adm1_africa$elec_na_perc_ls[adm1_africa$elec_na_perc_ls>1]<-1
adm1_africa$elec_na_perc_wp[adm1_africa$elec_na_perc_wp>1]<-1

hist(adm1_africa$elec_na_perc_ls)
hist(adm1_africa$elec_na_perc_wp)

# Invert to make more intuitive
adm1_africa$elec_na_perc_ls<-1-adm1_africa$elec_na_perc_ls
adm1_africa$elec_na_perc_wp<-1-adm1_africa$elec_na_perc_wp


# Convert to raster
elec_na_perc_ls<-terra::rasterize(adm1_africa,base_raster,field="elec_na_perc_ls")
elec_na_perc_wp<-terra::rasterize(adm1_africa,base_raster,field="elec_na_perc_wp")

terra::plot(c(elec_na_perc_ls,elec_na_perc_wp))

data<-elec_na_perc_ls
names(data)<-"elec_access"
writeRaster(data,file=paste0(DataDirInt,"/",names(data),".tif"),overwrite=T)

# Generate terciles
Overwrite<-T
# Adaptive capacity is classified thus: high = good, low = bad
# A high proportion of population without access to electricity is market is worse, lower tercile = good, upper tercile = bad 
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

# Not run
if(F){
# Raster level calculations - note issue with alignment to population datasets #####
# Convert to raster
population20_ls<-terra::crop(population20_ls,elec_na)
population20_wp<-terra::crop(population20_wp,elec_na)

# Resample to base_raster summining population
population20_ls<-resample(population20_ls,base_raster,method="sum")
population20_wp<-resample(population20_wp,base_raster,method="sum")

# Calculate proportion of pop without access to electricity
elec_na_pr_ls<-elec_na/population20_ls
elec_na_pr_wp<-elec_na/population20_wp

# Seems to be a mismatch in the layers where elec_na has people with no access to power but the population layers indicate far less human population
terra::plot(elec_na_pr_ls)
hist(elec_na_pr_ls[!elec_na_pr_ls>=1])
hist(elec_na_pr_ls[elec_na_pr_ls>1])
X<-elec_na_pr_ls[]
length(X[X>1 & !is.na(X) & !is.infinite(X)])/length(X[!is.na(X) & !is.infinite(X)])
length(X[is.infinite(X)])/length(X[!is.na(X)])

terra::plot(elec_na_pr_wp)
hist(elec_na_pr_wp[!elec_na_pr_wp>=1])
hist(elec_na_pr_wp[elec_na_pr_wp>1])
X<-elec_na_pr_wp[]

length(X[X>1 & !is.na(X) & !is.infinite(X)])/length(X[!is.na(X) & !is.infinite(X)])
length(X[is.infinite(X)])/length(X[!is.na(X)])

# We can try setting values >1 to 1, but what is the validity of this approach?
elec_na_pr_ls[elec_na_pr_ls>1]<-1
elec_na_pr_wp[elec_na_pr_wp>1]<-1

terra::plot(elec_na_pr_ls)
hist(elec_na_pr_ls[])

terra::plot(elec_na_pr_wp)
hist(elec_na_pr_wp[])
}
            
#Not Run
if(F){
# **********************************************
# Electricity consumption tiers #####
# **********************************************

# Load electrification data
# See https://www.nature.com/articles/s41597-019-0122-6 for methods
elec_tier<-terra::rast(paste0(DataDir,"/atlas_electrification/raw/tiersofaccess_SSA_2018.nc"))
elec_tier_rs<-terra::resample(elec_tier,base_raster,method="mode")
elec_tier_rs<-terra::mask(terra::crop(elec_tier_rs,adm1_africa),adm1_africa)

terra::writeRaster(elec_tier_rs,file=paste0(DataDirInt,"/elec_consumption_tiers.tif"),overwrite=T)
terra::plot(elec_tier_rs)

elec_tier_rs[elec_tier_rs==0]<-NA

# The four tiers (1-4) are validated against survey data consistent with the World Bank Multi-Tier framework
#Consumption tier
#<0.2 KWh/hh/day
#<1 KWh/hh/day
#<3.4 KWh/hh/day
#>3.4 KWh/hh/day
    }