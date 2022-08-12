require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"

# Read in admin1 for subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read in a base raster
base_raster<-terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif"))
base_raster<-terra::crop(base_raster,adm1_africa)

# **********************************************
# Electricity consumption tiers #####
# **********************************************

# Load electrification data
# See https://www.nature.com/articles/s41597-019-0122-6 for methods
elec_na<-terra::rast(paste0(DataDir,"/atlas_electrification/raw/noaccess_SSA_2014_2020.nc"))

elec_tier<-terra::rast(paste0(DataDir,"/atlas_electrification/raw/tiersofaccess_SSA_2018.nc"))
elec_tier<-terra::resample(elec_tier,base_raster,method="mode")
elec_tier<-terra::mask(terra::crop(elec_tier,base_raster),base_raster)

# The four tiers (1-4) are validated against survey data consistent with the World Bank Multi-Tier framework

# **********************************************
# Population without access to electricity #####
# **********************************************

# The below can be used to explore population without access to electricity / estimated population per grid cell
# However there are discrepancies in the final output (many values >1) which may relate to different treatments of urban vs rural populations in the methods of Falchetta et al.

# Load electrification data (pop without access)
# See https://www.nature.com/articles/s41597-019-0122-6 for methods
elec_na<-terra::rast(paste0(DataDir,"/atlas_electrification/raw/noaccess_SSA_2014_2020.nc"))

# Subset to most recent time point (2020)
# Unit is number of people without access to electricity
elec_na<-elec_na[[7]]

# Load landscan or worldpop population data, note that you will need to manually download this from https://landscan.ornl.gov/ if it is not already available
# According to the methods the elec_na layer uses landscan data
population20_ls<-terra::rast(paste0(DataDir,"/landscan_pop/raw/landscan-global-2020.tif"))
population20_wp<-terra::rast(paste0(DataDir,"/worldpop/raw/ppp_2020_1km_Aggregated.tif"))

# Crop to elec_na layer
population20_ls<-terra::crop(population20_ls,elec_na)
population20_wp<-terra::crop(population20_wp,elec_na)

# Resample to base_raster summining population
elec_na<-resample(elec_na,base_raster,method="sum")
population20_ls<-resample(population20_ls,base_raster,method="sum")
population20_wp<-resample(population20_wp,base_raster,method="sum")

# Calculate proportion of pop without access to electricity
elec_na_pr_ls<-elec_na/population20_ls
elec_na_pr_wp<-elec_na/population20_wp

# Seems to be a mismatch in the layers where elec_na has people with no access to power but the population layers indicate far less human population
terra::plot(elec_na_pr_ls)
hist(elec_na_pr_ls[])

terra::plot(elec_na_pr_wp)
hist(elec_na_pr_wp[])

# We can try setting values >1 to 1, but what is the validity of this approach?
elec_na_pr_ls[elec_na_pr_ls>1]<-1
elec_na_pr_wp[elec_na_pr_wp>1]<-1

terra::plot(elec_na_pr_ls)
hist(elec_na_pr_ls[])

terra::plot(elec_na_pr_wp)
hist(elec_na_pr_wp[])
