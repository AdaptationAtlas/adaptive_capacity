require(terra)
DataDir<-"/home/jovyan/common_data"

PlotRast<-function(Dir){
  Files<-list.files(Dir,".tif",full.names=T)
  terra::rast(Files[!grepl("aux.xml",Files)])
    }

# Education
EduDir<-paste0(DataDir,"/atlas_education/intermediate")
EduPlot<-PlotRast(EduDir)
terra::plot(EduPlot)
read.csv(list.files(EduDir,".csv",full.names=T))

# HealthCare
HealthDir<-paste0(DataDir,"/atlas_healthcare_access/intermediate")
HealthPlot<-PlotRast(HealthDir)
terra::plot(HealthPlot)
read.csv(list.files(HealthDir,".csv",full.names=T)[1]) # Motorized
read.csv(list.files(HealthDir,".csv",full.names=T)[2]) # Walking

# Gender equity
GEqDir<-paste0(DataDir,"/atlas_dhs/intermediate/atlas_subset")
GEqPlot<-PlotRast(GEqDir)
terra::plot(GEqPlot)
read.csv(list.files(GEqDir,".csv",full.names=T)[1]) # Decisions
read.csv(list.files(GEqDir,".csv",full.names=T)[2]) # Index

# Soil carbon
SoilDir<-paste0(DataDir,"/isda/intermediate/atlas/atlas_subset")
SoilPlot<-PlotRast(SoilDir)
terra::plot(SoilPlot)
read.csv(list.files(SoilDir,".csv",full.names=T)[1])

# Irrigation
IrriDir<-paste0(DataDir,"/atlas_mapspam/intermediate/irrigation")
IrriPlot<-PlotRast(IrriDir)
terra::plot(IrriPlot)

# Wealth
# https://globaldatalab.org/iwi/
IWIDir<-paste0(DataDir,"/atlas_poverty_IWI/intermediate")
IWIPlot<-PlotRast(IWIDir)
terra::plot(IWIPlot)
read.csv(list.files(IWIDir,".csv",full.names=T)[1])

# Mobile connectivity - ookla
ooklaDir<-paste0(DataDir,"/atlas_ookla/intermediate")
Files<-list.files(ooklaDir,".tif",full.names=T)
Files<-grep("dl_speed",Files,value=T)
Files<-Files[!grepl("aux.xml",Files)]

# masked - areas without ookla coverage set to 0
ooklaPlot_masked<- terra::rast(grep("masked",Files,value=T))
terra::plot(ooklaPlot_masked)
read.csv(list.files(ooklaDir,".csv",full.names=T)[1])

# not masked 
ooklaPlot<- terra::rast(Files[!grepl("masked",Files)])
terra::plot(ooklaPlot)
read.csv(list.files(ooklaDir,".csv",full.names=T)[2])

# Market access
MADir<-paste0(DataDir,"/atlas_market_access/intermediate")
MAPlot<-PlotRast(MADir)
terra::plot(MAPlot)
read.csv(list.files(MADir,".csv",full.names=T)[1])

# Electricity access
ElecDir<-paste0(DataDir,"/atlas_electrification/intermediate")
ElecPlot<-PlotRast(ElecDir)
terra::plot(ElecPlot)
read.csv(list.files(ElecDir,".csv",full.names=T)[1])
