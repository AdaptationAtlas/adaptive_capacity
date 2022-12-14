require(terra)

# Root of file structure
DataDir<-"/home/jovyan/common_data"
AqueDir<-paste0(DataDir,"/atlas_aqueduct_3/raw/Y2019M07D12_Aqueduct30_V01")

# Read in boundary of subsaharan africa
adm1_africa<-terra::vect(paste0(DataDir,"/atlas_boundaries/intermediate/gadm41_ssa_1.shp"))

# Read base raster
base_rast<-terra::crop(terra::rast(paste0(DataDir,"/mapspam_2017/raw/spam2017V2r1_SSA_H_YAMS_S.tif")),adm1_africa)

AqueDirInt<-paste0(DataDir,"/atlas_aqueduct_3/intermediate")
if(!dir.exists(AqueDirInt)){
    dir.create(AqueDirInt,recursive=T)
    }

gpkg_path<-grep("annual",list.files(AqueDir,".gpkg",recursive=T,full.names=T),value=T)

VectFile<-paste0(AqueDirInt,"/Y2019M07D12_Aqueduct30_V01_cr.shp")
if(!file.exists(VectFile)){
    aqueduct <- terra::vect(gpkg_path)
    aqueduct_cr<-terra::crop(aqueduct_cr,sh_africa)
    terra::writeVector(aqueduct_cr,file=VectFile)
}else{
    aqueduct_cr<-terra::vect(VectFile)
}


# https://github.com/wri/aqueduct30_data_download/blob/master/metadata.md
# Physical risk quantity:
phys_risk<-data.frame(Short=c("bws","bwd","iav","sev","gtd","rfr","cfr","drr"),
                      Full=c("Baseline water stress","Baseline water depletion","Interannual variability","Seasonal variability","Groundwater table decline","Riverine flood risk","Coastal flood risk","Drought risk"))

# Types:
types<-data.frame(Type=c("_raw","_score","_label","_cat"),
                  Data_Type=c("double","double","string","integer"),
                  Description=c("raw value. Units depend on the indicator. See the technical note.","each indicator is mapped to a [0-5] scale.","A label explaining the category of the indicator including threshold. e.g. Extremely High (more than 1 in 100).","integer for each category [-1,4], can be used for visuals."))

# Convert bws to raster
unique(data.frame(aqueduct_cr)[,c("bws_cat","bws_label")])

bws_cat<-terra::rasterize(aqueduct_cr,base_rast,field="bws_cat")
terra::writeRaster(bws_cat,file=paste0(AqueDirInt,"/bws_cat.tif"),overwrite=T)

bws_score<-terra::rasterize(aqueduct_cr,base_rast,field="bws_score")
terra::writeRaster(bws_score,file=paste0(AqueDirInt,"/bws_score.tif"),overwrite=T)

bws_raw<-terra::rasterize(aqueduct_cr,base_rast,field="bws_raw")
terra::writeRaster(bws_raw,file=paste0(AqueDirInt,"/bws_raw.tif"),overwrite=T)

terra::plot(c(bws_raw,bws_cat,bws_score))
