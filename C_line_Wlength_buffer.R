C_line_Wlength_buffer = function(points, target_sp, width=500, id_field=NULL, attr_field=NULL){
  # You should project your spatial dataset before execution.

  # Buffer generation
  buffer = gBuffer(points, byid = TRUE, 
                   width = width, quadsegs=90)
  target_sp.buf = target_sp[buffer, ]
  sp = gIntersection(buffer, target_sp.buf, byid = TRUE)
  sp.length = gLength(sp, byid=TRUE)
  #buf.length = buffer@polygons[[1]]@area
  ID_b = sapply(sp@lines, function(x) x@ID)
  sp.id = data.frame(ID = as.integer(sapply(strsplit(ID_b, split=' '),
                                 function(x) x[1])),
                     ID_f = as.integer(sapply(strsplit(ID_b, split=' '),
                                 function(x) x[2])),
                     Length = sp.length)
  
  target_sp_attr = target_sp@data %>% 
    filter(OBJECTID %in% sp.id$ID_f) %>% 
    dplyr::select(OBJECTID, TWID)

  ## TODO: generalize names of indicator fields                               
  sp.id = left_join(sp.id, target_sp_attr, c('ID_f'='OBJECTID')) %>% 
      mutate(W_len=TWID * Length) %>% 
      dplyr::select(ID, W_len) %>% 
      group_by(ID) %>% 
      summarize(W_len = sum(W_len)) %>% 
      ungroup() %>% data.frame
  return(sp.id)
}
