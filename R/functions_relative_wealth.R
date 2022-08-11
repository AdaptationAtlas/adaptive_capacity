#' Quadkey to tile
#' https://gis.stackexchange.com/questions/359507/how-to-convert-quadkey-into-tiles-coordinates
#' @export
quadkey_to_tile = function(qk){
 
  if(nchar(qk)==0){
    return(list(x=0,y=0,zoom=0))
  }
 
  digits <- rev(strsplit(qk,"")[[1]])
 
  i <- length(digits)
 
  masks <- 2**(0:(i-1))
  xt <- sum(masks[digits=="1" | digits=="3"])
  yt <- sum(masks[digits=="2" | digits=="3"])
 
  return(list(x=xt, y=yt, zoom=i))
}
#' Quadkeys to tile boundary
#' @export
qk_to_sf <- function(i,v){
  vi <- v[i,]
  qt <- quadkey_to_tile(as.character(vi$quadkey))
  # slippymath::tilenum_to_lonlat(qt$x, qt$y, qt$z)
  bb <- slippymath::tile_bbox(qt$x, qt$y, qt$z)
  bbs <- st_as_sfc(bb, crs = "EPSG:3857")
  # a <- st_area(bbs)
  bbs <- st_transform(bbs, "EPSG:4326")
  ddsf <- st_sf(bbs, vi)
  return(ddsf)
}
#' convertPoints
#' @export
convertPoints <- function(iso, ff, datadir, overwrite){
  cat("Processing", iso, "\n")
  f <- grep(iso, ff, value = T)
 
  # setup output filename
  ofile <- file.path(datadir, "rwi_spatial", paste0(iso, "_relative_wealth_index_2_4_km.tif"))
  dir.create(dirname(ofile), FALSE, TRUE)
 
  # delete files first for overwrite
  if(overwrite){unlink(ofile)}
 
  # now the processing
  if(!file.exists(ofile)){
    v <- read.csv(f, stringsAsFactors = FALSE)
   
    ss <- lapply(1:nrow(v), qk_to_sf, v)
    ssg <- sf::st_as_sf(data.table::rbindlist(ss))
   
    # save output file
    jfile <- gsub(".tif", ".geojson", ofile)
    st_write(ssg, jfile)
   
    # convert to
    sv <- vect(ssg)
    g <- vect(raster::getData("GADM", country = iso, level = 0,
                              path = file.path(datadir, "input/boundary/country")))
   
    ref <- rast(crs = "epsg:4326", extent = ext(g), resolution = 0.008333)
    # resolution = 0.02197) # equivalent to 2440 meter
   
    rwi <- rasterize(sv, ref, "rwi", fun = mean)
    error <- rasterize(sv, ref, "error", fun = mean)
    re <- c(rwi, error)
   
    re <- crop(re, g)
    mask(re,g,filename = ofile, wopt= list(gdal=c("COMPRESS=LZW")))
  }
  return(NULL)
}
