## phb_aw: calculate sub-area-weighted features using buffer polygons and small-area polygons
## st_buf: sf object. buffer
## st_phb: sf object. typically, it should be census tract or equivalent small area
## ran: target fields to be calculated with area weights
phb_aw <- function(st_buf, st_phb, ran=3:44){
  pnts.phb <- st_phb %>% 
              mutate(TArea=st_area(geometry)) %>% 
              st_intersection(st_buf, .) %>% 
              mutate(area=as.numeric(st_area(geometry)))
              
  #print(colnames(pnts.phb))
  pnts.phb <- st_set_geometry(pnts.phb, NULL)
  pnts.phb %>% 
    mutate(w_area = area/TArea) %>% 
    group_by(ID) %>% 
    dplyr::summarize_at(.vars = vars(ran), 
                        .funs = funs(sum(. * w_area, na.rm=TRUE))) %>% 
    ungroup -> pnts.phb.s
  
  return(pnts.phb.s)
}
