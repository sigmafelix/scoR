sp_index_grid <- function(pnts, ncutsx, ncutsy){
  grid1 <- st_make_grid(pnts, n = c(ncutsx, ncutsy)) %>% as.tibble %>% st_as_sf()
  grid1 <- grid1[pnts, ] %>% mutate(ID = 1:nrow(.)) 
  return(grid1)
}

#pts.indexed <- sp_index_grid(pnt, 60, 50)


