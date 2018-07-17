library(raster)

clip_as_extent <- function(pnts, buffer.d, nqsegs=180, target.shp){
  buffer <- pnts %>% 
            st_buffer(., buffer.d, nqsegs)
  cae <- buffer %>% 
    as(., 'Spatial') %>% 
    raster::extent(.) %>% 
    as(., 'SpatialPolygons') %>% 
    st_as_sf %>% 
    st_set_crs(., st_crs(pnts)) %>% 
    st_intersection(., target.shp)
  
  return(cae)
}

clip_as_extent.raster <- function(pnts, buffer.d, nqsegs=180, ras){
  buffer <- pnts %>% 
            st_buffer(., buffer.d, nqsegs)
  cae <- buffer %>% 
          as(., 'Spatial') %>% 
          raster::extent(.) %>% 
          as(., 'SpatialPolygons')
  cae <- raster::crop(ras, cae, snap = 'out')
  return(cae)
}
