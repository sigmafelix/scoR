##--- lu_aw: proportion of land uses for the buffer polygon
## st_buf: buffer polygon
## st_poly: landuse polygon
lu_aw <- function(st_buf, st_poly){
  library(tidyverse)
  library(sf)
  buf.ta <- st_buf %>% 
            mutate(TArea = st_area(geometry)) %>% 
            as.tibble
  pnts.aw <- st_intersection(st_buf, st_poly)
  pnca <- pnts.aw %>% 
          mutate(area = st_area(geometry)) %>% 
          as.tibble %>% 
          group_by(ID) %>% 
          summarize(area = sum(area, na.rm=T)) %>% 
          ungroup()
  buf.ta <- buf.ta %>% 
            left_join(pnca) %>% 
            mutate(LSp=area/TArea)
  return(buf.ta)
}

