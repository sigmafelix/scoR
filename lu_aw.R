##--- lu_aw: proportion of land uses for the buffer polygon
## st_buf: buffer polygon
## st_poly: landuse polygon

lu_aw <- function(st_buf, st_poly){
  Sys.time()
  buf.ta <- st_buf %>% 
            mutate(TArea=st_area(geometry))
  pnts.aw <- st_intersection(st_buf, st_poly)
  pnca <- pnts.aw %>% 
          mutate(area=st_area(geometry))
  pnca <- st_set_geometry(pnca, NULL)
  pnca <- pnca %>% 
          group_by(ID) %>% 
          summarize(area=sum(area)) %>% 
          ungroup()
  buf.ta <- buf.ta %>% 
            left_join(pnca) %>% 
            mutate(LSp=area/TArea)
  buf.ta <- st_set_geometry(buf.ta, NULL)
  buf.ta <- buf.ta %>% 
            dplyr::select(ID, LSp)
  Sys.time()
  return(buf.ta)
}
