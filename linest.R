## linest_aw: weighted road variable extraction
## st_buf: sf object; buffer polygon
## st_line: sf object; road network line
### --- st_line should have Width and LANES fields
### TODO: generalize field names and 

linest_aw <- function(st_buf, st_line){
  pnts.road <- st_intersection(st_buf, st_line)
  pnca <- pnts.road %>% 
          mutate(leng=st_length(geometry))
  pnca <- st_set_geometry(pnca, NULL)
  pnca <- pnca %>% 
          group_by(ID) %>% 
          summarize(L = sum(leng),
                    LW = sum(leng * Width),
                    LLW = sum(leng * LANES * Width)) %>% 
          ungroup()
  return(pnca)
}


## linest_dist: find the shortest distance to the road features
## st_pnts: sf object; points
## st_line: sf object; road networks
### TODO: find more optimized way of finding the minimum distance without generic apply function
linest_dist <- function(st_pnts, st_line){
  ld <- st_distance(st_pnts, st_line) %>% 
        apply(., 1, min) 
  return(ld)
}
