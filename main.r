main_proc <- function(pnts, nx, ny, ncore1 = 4, ncore2 = 6, ncore3 = 6, geom_field = 'geom', calran = 8:49){
  # pnts: points (sf object)
  # nx: the number of splits at x-axis
  # ny: the number of splits at y-axis
  # ncore 1, 2, 3: the number of cores used at extracting shapes, computing LS-related variables, and computing relalt variables, respectively.
  # You should aware of your machine's specification to accomodate the demand of jobs.
  linest_dist.mult <- function(shp, lst){
    s <- lapply(lst, function(x) linest_dist(st_pnts = shp, st_line = x))
    s1 <- do.call(cbind, s)
    s1 <- data.frame(s1)
    colnames(s1) <- paste(rep('D_', 8),
                          c('Airport', 'River', 'Rail', 'Sub', 'Bus', 'Coast', 'North', 'Port'), 
                          sep = '')
    return(s1)
  }
  
  library(sf)
  library(doParallel)
  library(foreach)
  library(tictoc)
  
  proc <- list()
  
  idf <- 'ID'
  # make grids
  dgrid <- sp_index_grid(pnts, nx, ny)
  
  # loop through grids
  for (i in 1:nrow(dgrid)) {
    sts <- pnts[dgrid[i,],]  
    cat(paste('This is iteration ', i, '/', nrow(dgrid), '(', nrow(sts), ' features)\n', sep = ''))
    
    # catch the error
    #tryCatch({
  # Compute vectors
    sites.list <- 1:nrow(sts) %>% split(.,.)
    lsflag.list <- 1:12 %>% split(.,.)
    
    if (st_bbox(sts)[3] - st_bbox(sts)[1] < 100000){
      cl <- makeCluster(ncore1, type="SOCK")
      registerDoSNOW(cl)
      ls.list.s <- foreach(x = 1:12 %>% split(.,.), 
                           .inorder = TRUE, 
                           .packages = c('sf', 'lwgeom', 'tidyverse'), 
                           .export = c('sts', 'clip_as_extent', 'ls.list')) %dopar% 
                           {clip_as_extent(sts, 
                                           5200, 
                                           target.shp = ls.list[[x]] %>% st_cast(., 'POLYGON'))}
      stopCluster(cl)
    }
    else {
      ls.list.s <- ls.list
    }
    cat(paste('Compute the shortest distance to 8 datasets...\n'))
    cl <- makeCluster(ncore2, type="SOCK")
    registerDoSNOW(cl)
    system.time(dist.df <- 
                  foreach(.export = c('sts', 'linest_dist', 'linest_dist.mult', 'sites.list', 'rdxs'),
                          .combine = rbind, 
                          .inorder = TRUE, 
                          .packages = c('sf', 'foreach', 'sp', 'lwgeom'),
                          x = sites.list) %dopar%
                          {linest_dist.mult(sts[x,], rdxs)})
    dist.df <- bind_cols(ID = sts$ID, dist.df)
    
    buffers <- c(25, 50, 100, 300, 500, 1000, 5000) %>% split(.,.)
    buffers.list <- buffers %>% lapply(function(x) st_buffer(sts, x, nQuadSegs = 180))
    buffers.list.s <- buffers.list
    buffers.list.s[[1]] <- NULL
    buffers.list.s[[1]] <- NULL

    # phb #25:66
    cat(paste('Compute PHB variables... \n'))
    system.time(phb.all <- lapply(buffers.list.s, function(x) phb_aw(x, st_phb = sgis.list[[1]], ran = calran, geom_field = geom_field))) #4:45
    # line l, llw, lw
    cat(paste('Compute areally weighted road features... \n'))
    system.time(lineaw <- lapply(buffers.list, function(x) lapply(road.list, function(y) linest_aw(x, y, geom_field = geom_field))))
    # line d
    #system.time(linedist <- lapply(dist.list, function(x) linest_dist(monitor.sp2, x)))
    
    # Fieldname definition from python code
    Fnames <- c("H_gb_1", "H_gb_2","H_gb_3","H_gb_4", "H_gb_5", "H_gb_6",
                "H_yr_1", "H_yr_2", "H_yr_3", "H_yr_4", "H_yr_5", "H_yr_6",
                "B_bnu_1","B_bnu_2", "B_bnu_3", "B_bnu_4", "B_bnu_5", "B_bnu_6", "B_bnu_7",
                "B_bem_1", "B_bem_2", "B_bem_3", "B_bem_4", "B_bem_5","B_bem_6", "B_bem_7")
    Gnames <- c("ho_gb_1_10", "ho_gb_2_10", "ho_gb_3_10", "ho_gb_4_10", "ho_gb_5_10", "ho_gb_6_10",
                "ho_yr_1_10", "ho_yr_2_10", "ho_yr_3_10", "ho_yr_4_10", "ho_yr_5_10", "ho_yr_6_10",
                "cp_bnu_1_10", "cp_bnu_2_10", "cp_bnu_3_10", "cp_bnu_4_10", "cp_bnu_5_10", "cp_bnu_6_10", "cp_bnu_7_10",
                "cp_bem_1_10", "cp_bem_2_10", "cp_bem_3_10", "cp_bem_4_10", "cp_bem_5_10", "cp_bem_6_10", "cp_bem_7_10")
    ftnames <- c('POP', 'GA')
    gtnames <- c('in_tot', 'ga_tot')
    bufsizes <- c(25, 50, 100, 300, 500, 1000, 5000)
    
    ggnames <- c(gtnames, Gnames)
    ffnames <- c(ftnames, Fnames)
    #ffnames <- paste(ffnames, sprintf('%04d', bufsizes[3]), sep = '_')
    
    buflists <- bufsizes[-1:-2] %>% split(., .) %>% 
      lapply(function(x) paste(ffnames, sprintf('%04d', x), sep = '_')) %>% 
      lapply(function(x) c('ID',x))
    
    phb.sub <- phb.all %>% 
      lapply(function(x) x %>% dplyr::select(ID, starts_with('in_tot'), starts_with('ga_tot'),
                                             starts_with('ho_gb'), starts_with('ho_yr'),
                                             starts_with('cp_bnu'), starts_with('cp_bem')))
    #phb.sub[[1]] <- NULL
    #phb.sub[[1]] <- NULL
    phb.ren <- mapply(function(x, y) {colnames(x) <- y
    return(x)}, phb.sub, buflists, SIMPLIFY = FALSE)
    phb.rdf <- join_all(phb.ren, by = 'ID', type = 'full')
    phb.rdf <- sts %>% st_set_geometry(NULL) %>% dplyr::select(ID) %>% left_join(phb.rdf, by = 'ID')
    
    lineaw.df <- lineaw %>% lapply(., function(x) join_full(x, by = 'ID')) %>% 
      join_full(by = 'ID')
    colnames(lineaw.df)[-1] <- paste(rep(rep(c('MR1', 'MR2', 'Road'), rep(3, 3)), 7),
                                     rep(c('L', 'LW', 'LLW'), 21),
                                     rep(sprintf('%04d', bufsizes), rep(9, 7)), sep = '_')
    
    #car.regist <- sites %>% st_join(carreg)
    #car.regist <- st_set_geometry(car.regist, NULL) %>% 
    #  dplyr::select(ID, SIGUNGU_CD, Mean) %>% 
    #  rename(Car_Mean = Mean)
    cat(paste('Compute landuse variables... it could take a while. \n'))
    system.time(ls0100 <- foreach(x = lsflag.list, 
                                  .combine = c, 
                                  .export = c('ls.list.s', 'sts', 'lu_aw'), 
                                  .packages = c('sf','tidyverse', 'foreach', 'doSNOW', 'lwgeom'), 
                                  .inorder = TRUE) %dopar% {sts %>% 
                                      st_buffer(., 100, 180) %>% lu_aw(., ls.list.s[[x]], geom_field = geom_field)})
    
    system.time(ls0300 <- foreach(x = lsflag.list, 
                                  .combine = c, 
                                  .export = c('ls.list.s', 'sts', 'lu_aw'), 
                                  .packages = c('sf','tidyverse', 'foreach', 'doSNOW', 'lwgeom'), 
                                  .inorder = TRUE) %dopar% {sts %>% 
                                      st_buffer(., 300, 180) %>%  lu_aw(., ls.list.s[[x]], geom_field = geom_field)})
                
    system.time(ls0500 <- foreach(x = lsflag.list, 
                                  .combine = c, 
                                  .export = c('ls.list.s', 'sts', 'lu_aw'), 
                                  .packages = c('sf','tidyverse', 'foreach', 'doSNOW', 'lwgeom'), 
                                  .inorder = TRUE) %dopar% {sts %>%  
                                      st_buffer(., 500, 180) %>% lu_aw(., ls.list.s[[x]], geom_field = geom_field)})
                
    system.time(ls1000 <- foreach(x = lsflag.list, 
                                  .combine = c, 
                                  .export = c('ls.list.s', 'sts', 'lu_aw'), 
                                  .packages = c('sf','tidyverse', 'foreach', 'doSNOW', 'lwgeom'), 
                                  .inorder = TRUE) %dopar% {sts %>%  
                                      st_buffer(., 1000, 180) %>% lu_aw(., ls.list.s[[x]], geom_field = geom_field)})
                
    system.time(ls5000 <- foreach(x = lsflag.list, 
                                  .combine = c, 
                                  .export = c('ls.list.s', 'sts', 'lu_aw'), 
                                  .packages = c('sf','tidyverse', 'foreach', 'doSNOW', 'lwgeom'), 
                                  .inorder = TRUE) %dopar% {sts %>%  
                                      st_buffer(., 5000, 180) %>% lu_aw(., ls.list.s[[x]], geom_field = geom_field)})
    
    stopCluster(cl)
    
    ##### landuse-related variables
    ls0100.l <- data.frame(ls0100[[2]], ls0100[[4]], ls0100[[6]], ls0100[[8]], ls0100[[10]],
                           ls0100[[12]], ls0100[[14]], ls0100[[16]], ls0100[[18]], 
                           ls0100[[20]], ls0100[[22]], ls0100[[24]])
    ls0300.l <- data.frame(ls0300[[2]], ls0300[[4]], ls0300[[6]], ls0300[[8]], ls0300[[10]],
                           ls0300[[12]], ls0300[[14]], ls0300[[16]], ls0300[[18]], 
                           ls0300[[20]], ls0300[[22]], ls0300[[24]])
    ls0500.l <- data.frame(ls0500[[2]], ls0500[[4]], ls0500[[6]], ls0500[[8]], ls0500[[10]],
                           ls0500[[12]], ls0500[[14]], ls0500[[16]], ls0500[[18]], 
                           ls0500[[20]], ls0500[[22]], ls0500[[24]])
    ls1000.l <- data.frame(ls1000[[2]], ls1000[[4]], ls1000[[6]], ls1000[[8]], ls1000[[10]],
                           ls1000[[12]], ls1000[[14]], ls1000[[16]], ls1000[[18]], 
                           ls1000[[20]], ls1000[[22]], ls1000[[24]])
    ls5000.l <- data.frame(ls5000[[2]], ls5000[[4]], ls5000[[6]], ls5000[[8]], ls5000[[10]],
                           ls5000[[12]], ls5000[[14]], ls5000[[16]], ls5000[[18]], 
                           ls5000[[20]], ls5000[[22]], ls5000[[24]])
    
    ls.cn <- paste('LS', c(seq(110, 160, 10), seq(200, 700, 100)), sep = '')
    #ls0100.l <- ls0100 %>% lapply(function(x) x %>% dplyr::select(LSp)) %>% do.call(bind_cols, .)
    #ls0300.l <- ls0300 %>% lapply(function(x) x %>% dplyr::select(LSp)) %>% do.call(bind_cols, .)
    #ls0500.l <- ls0500 %>% lapply(function(x) x %>% dplyr::select(LSp)) %>% do.call(bind_cols, .)
    #ls1000.l <- ls1000 %>% lapply(function(x) x %>% dplyr::select(LSp)) %>% do.call(bind_cols, .)
    #ls5000.l <- ls5000 %>% lapply(function(x) x %>% dplyr::select(LSp)) %>% do.call(bind_cols, .)
    
    ls.ll <- list(ls0100.l, ls0300.l, ls0500.l, ls1000.l, ls5000.l)
    ls.cnl <- list(ls.cn, ls.cn, ls.cn, ls.cn, ls.cn)
    ls.cnlist <- bufsizes[-1:-2] %>% split(.,.) %>% 
      lapply(function(x) paste(ls.cn, sprintf('%04d', x), sep = '_'))
    ls.ll <- mapply(function(x, y) {colnames(x) <- y
    return(x)}, ls.ll, ls.cnlist, SIMPLIFY = FALSE)
    ls.df <- do.call(bind_cols, ls.ll)
    ls.df <- cbind(ID = sts$ID, ls.df)

    # Compute rasters
    sts.5174 <- sts %>% st_transform(proj4string(aster)) %>% as('Spatial')
    
    cat(paste('Raster processing begins. \n'))
    cat(paste('Compute emissions...\n'))
    system.time(all.extract.pd <- compute_em(input = sts.5174, 
                                             seed.raster.a.2010, seed.raster.p.2010, seed.raster.l.2010))
    
    cat(paste('Compute relative altitudes... it could take a while. \n'))
    cl <- makeCluster(ncore3, type="SOCK")
    registerDoSNOW(cl)
    
    sites.extr.a <- raster::extract(aster, sts.5174)
    sites.extr.k <- raster::extract(kngii, sts.5174)
    extr.ak <- data.frame(ID = sts.5174@data[,idf],
                          Altitude_a = sites.extr.a,
                          Altitude_k = sites.extr.k)
    
    1:nrow(sts) %>% split(.,.) -> sites.list
    # TODO: parallelize relative altitude computation
    
    aster.cl <- clip_as_extent_ras2(sts.5174 %>% st_as_sf, 5500, ras = aster)
    kngii.cl <- clip_as_extent_ras2(sts.5174 %>% st_as_sf, 5500, ras = kngii)
    
    tic('ASTER-5000m buffer:')
    system.time(ra.a5k <- foreach(x = sites.list,
                                  .combine = rbind,
                                  .export = c('sts.5174', 'aster.cl', 'sites.extr.a', 'extract_rel', 'extract_rasterize'),
                                  .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                                  {extract_rel(rasterlayer = aster.cl, pointlist = sts.5174, 
                                               bufferlist = c(5030,5000), rn = 'a', 
                                               extr = sites.extr.a, index = x)})
    toc()
    tic('ASTER-1000m buffer:')
    system.time(ra.a1k <- foreach(x = sites.list,
                                  .combine = rbind,
                                  .export = c('sts.5174', 'aster.cl', 'sites.extr.a', 'extract_rel', 'extract_rasterize'),
                                  .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                                  {extract_rel(rasterlayer = aster.cl, pointlist = sts.5174, 
                                               bufferlist = c(1030,1000), rn = 'a', 
                                               extr = sites.extr.a, index = x)})
    toc()
    tic('KNGII-5000m buffer:')
    system.time(ra.k5k <- foreach(x = sites.list,
                                  .combine = rbind,
                                  .export = c('sts.5174', 'kngii.cl', 'sites.extr.k', 'extract_rel', 'extract_rasterize'),
                                  .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                                  {extract_rel(rasterlayer = kngii.cl, pointlist = sts.5174, 
                                               bufferlist = c(5090,5000), rn = 'k', 
                                               extr = sites.extr.k, index = x)})
    toc()
    tic('KNGII-1000m buffer:')
    system.time(ra.k1k <- foreach(x = sites.list,
                                  .combine = rbind,
                                  .export = c('sts.5174', 'kngii.cl', 'sites.extr.k', 'extract_rel', 'extract_rasterize'),
                                  .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                                  {extract_rel(rasterlayer = kngii.cl, pointlist = sts.5174, 
                                               bufferlist = c(1090,1000), rn = 'k', 
                                               extr = sites.extr.k, index = x)})
    toc()
    
    #system.time(sites.ra.a1k <- lapply(sites.list, 
    #                                   function(x) extract_rel(rasterlayer = aster, pointlist = sts.5174, bufferlist = c(1030,1000),rn = 'a', extr = sites.extr.a, index = x)))
    #system.time(sites.ra.a5k <- lapply(sites.list, 
    #                                   function(x) extract_rel(rasterlayer = aster, pointlist = sts.5174, bufferlist = c(5030,5000),rn = 'a', extr = sites.extr.a, index = x)))
    #system.time(sites.ra.k1k <- lapply(sites.list, 
    #                                   function(x) extract_rel(rasterlayer = kngii, pointlist = sts.5174, bufferlist = c(1090,1000),rn = 'k', extr = sites.extr.k, index = x)))
    #system.time(sites.ra.k5k <- lapply(sites.list, 
    #                                   function(x) extract_rel(rasterlayer = kngii, pointlist = sts.5174, bufferlist = c(5090,5000),rn = 'k', extr = sites.extr.k, index = x)))
    
    #ra.a1k <- sites.ra.a1k %>% do.call(rbind, .)
    #ra.a5k <- sites.ra.a5k %>% do.call(rbind, .)
    #ra.k1k <- sites.ra.k1k %>% do.call(rbind, .)
    #ra.k5k <- sites.ra.k5k %>% do.call(rbind, .)
    
    ra.all <- ra.a1k %>% 
      left_join(ra.a5k) %>% left_join(ra.k1k) %>% left_join(ra.k5k)
    
    
    cat(paste('Compute NDVI variables...\n'))
    ndvi.cl <- clip_as_extent_ras2(sts.5174 %>% st_as_sf, 6000, ras = ndvi)
    tic('NDVI (buffer: 1000m)')
    system.time(ndvi.r1 <- foreach(x = sites.list,
                                  .combine = rbind,
                                  .export = c('sts.5174', 'ndvi.cl', 'sites.extr.k', 'extract_ndvi', 'extract_rasterize'),
                                  .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                  {extract_ndvi(rasterlayer = ndvi.cl, pointlist = sts.5174, 
                               bufferdist = 1000, index = x)})
    toc()
    tic('NDVI (buffer: 5000m)')
    system.time(ndvi.r5 <- foreach(x = sites.list,
                                   .combine = rbind,
                                   .export = c('sts.5174', 'ndvi.cl', 'sites.extr.k', 'extract_ndvi', 'extract_rasterize'),
                                   .packages = c('sf', 'raster', 'foreach', 'tidyverse', 'sp')) %dopar%
                  {extract_ndvi(rasterlayer = ndvi.cl, pointlist = sts.5174, 
                                bufferdist = 5000, index = x)})
    toc()
    #system.time(ndvi.r1 <- lapply(sites.list, 
    #                              function(x) extract_ndvi(rasterlayer = ndvi, pointlist = sts.5174[x,], bufferdist = 1000)))
    #system.time(ndvi.r5 <- lapply(sites.list, 
    #                              function(x) extract_ndvi(rasterlayer = ndvi, pointlist = sts.5174[x,], bufferdist = 5000)))
    stopCluster(cl)
    #nr1 <- ndvi.r1 %>% do.call(rbind, .) %>% as.data.frame
    #nr5 <- ndvi.r5 %>% do.call(rbind, .) %>% as.data.frame
    ndvi.r <- ndvi.r1 %>% left_join(ndvi.r5)
    
  
  # join all
  fintab <- sts %>% full_join(ra.all) %>%# full_join(car.regist) %>% 
    full_join(dist.df) %>% 
    full_join(all.extract.pd) %>% 
    full_join(lineaw.df) %>% 
    full_join(ls.df) %>% 
    full_join(ndvi.r) %>% 
    full_join(phb.rdf) %>% 
    full_join(extr.ak) %>% 
    #bind_cols(Altitude_a = sites.extr.a) %>% 
    #bind_cols(Altitude_k = sites.extr.k) %>% 
    bind_cols(data.frame(st_coordinates(.))) %>% 
    dplyr::rename(TM_X = X, TM_Y = Y)
  fintab <- fintab %>% st_join(carreg %>% dplyr::select(Mean)) %>% rename(Car_Mean = Mean)
  fintab <- st_set_geometry(fintab, NULL)
  proc[[i]] <- fintab
    #}, error = function(e) {
    #  proc[[i]] <- i
    #  print(e)
    #})
  cat(paste('Iteration ', i, '/', nrow(dgrid), ' finished.\n', sep = ''))
  }
  return(proc)  
}
