C_line_Wlength_buffer = function(points, target_sp, width=500, id_field=NULL, attr_field=NULL){
  # You should project your spatial dataset before execution.
  library(plyr); library(dplyr); library(sp); library(rgeos); library(rgdal)
  # Buffer generation
  buffer = gBuffer(points, byid = TRUE, 
                   width = width, quadsegs=90)
  target_sp.buf = target_sp[buffer, ]
  sp = gIntersection(buffer, target_sp.buf, byid = TRUE)
  sp.length = gLength(sp, byid=TRUE)
  #buf.length = buffer@polygons[[1]]@area
  sp.id = data.frame(ID_b = sapply(sp@lines, function(x) x@ID),
                     Length = sp.length,
                     ID = as.integer(sapply(strsplit(sapply(sp@lines, function(x) x@ID), split=' '),
                                 function(x) x[1])) + 1,
                     ID_f = as.integer(sapply(strsplit(sapply(sp@lines, function(x) x@ID), split=' '),
                                 function(x) x[2])) + 1)
                                   
  # TODO: generalize retrieval process of the name of indicator field  
  # HOWTO: evaluation of a character as used in *plyr-related packages
  target_sp_attr = target_sp@data %>% 
    filter(OBJECTID %in% sp.id$ID_f) %>% 
    dplyr::select(OBJECTID, TWID)
    
  sp.id = left_join(sp.id, target_sp_attr, c('ID'='OBJECTID')) %>% 
    mutate(W_len=TWID * Length) %>% 
    dplyr::select(ID, W_len)

  return(sp.id)
  }
