#' Spilt continuous raster in to quantiles
#' @export
quantile_split<-function(data,Labels,Invert,do_binary_splits,savedir,Overwrite){
    Quants<-terra::rast(lapply(names(data),FUN=function(FIELD){
    
    File<-paste0(DataDirInt,"/",FIELD,"_",length(Labels),"quantiles.tif")
    Length<-length(Labels)
    Values<-(1:length(Labels))-1
    
    if((!file.exists(File))|Overwrite){
        X<-data[[FIELD]]
        vTert = quantile(X[], c(0:Length/Length),na.rm=T)

       Levels<-if(Invert){rev(Labels)}else{Labels}
                
        Intervals<-data.frame(Intervals=apply(cbind(Levels,round(vTert[1:Length],3),round(vTert[2:(Length+1)],3)),1,paste,collapse="-"))
        write.csv(Intervals,file=paste0(savedir,"/",FIELD,"_quantiles.csv"),row.names=F)
        
        Quantiles<-cut(X[],
                      vTert, 
                      include.lowest = T, 
                      labels = Values)
        X[]<-as.numeric(Quantiles)-1
        
      
        if(!Invert){
            Z<-X
            levels(Z)<-Labels 
            Vtab<-data.frame(Label=Labels,Value=Values)
        }else{
            X<-terra::classify(X,cbind(Values,rev(Values)))
            Z<-X
            levels(Z)<-Labels
            Vtab<-data.frame(Label=Labels,Value=rev(Values))
            }
                
        suppressWarnings(terra::writeRaster(Z,file=File,overwrite=T))
        
        if(do_binary_splits){
        Seq<-(1:length(Labels))-1
    
        Y<-terra::rast(lapply(Seq,FUN=function(i){
            Lab<-Labels[i+1]
            Vals<-i:max(Seq)
            Y<-X
            Y[!Y %in% Vals]<-NA
            Y[!is.na(Y)]<-1
        
            names(Y)<-paste(names(Y),"_",Lab)
            
           suppressWarnings(terra::writeRaster(Y,file=paste0(savedir,"/",FIELD,"_",Lab,".tif"),overwrite=T))
            Y
           }))
            
            }
        
        X

        }else{
            terra::rast(File)
    }
}))
    
    return(Quants)
    
}