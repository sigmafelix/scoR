C_poly_proportion_buffer = function(points, target_sp, width=500, id_field=NULL){
  # You should project your spatial dataset before execution.
  library(rgeos)
  library(sp)
  library(rgdal)
  library(dplyr)
  
  # Buffer generation
  buffer = gBuffer(points, 
                   byid = TRUE, 
                   width = width, 
                   quadsegs = 90)
  sp = gIntersection(buffer, 
                     target_sp, 
                     byid = TRUE)
  # Initialize ID field
  sp.id = c()
  buf.area = buffer@polygons[[1]]@area
  
  for (i in 1:length(sp)){sp.id[i] = sp@polygons[[i]]@ID}
  sp.id = sapply(strsplit(sp.id, split = ' '),  
                 function(x) x[1])
  sp.area = sapply(sp@polygons, 
                   function(x) x@area)
                     
  poly.df = data.frame(ID = sp.id, Shp_area = sp.area) %>% 
    group_by(ID) %>% 
    summarize(sp.area = sum(Shp_area)) %>% 
    ungroup() %>% 
    mutate(ID = as.integer(as.character(ID)) + 1,
           sp.area = sp.area / buf.area) %>% 
    arrange(ID) %>% 
    mutate(amsid = points@data[ID, id_field]) %>% 
    dplyr::select(3,2)

  return(poly.df)
}
