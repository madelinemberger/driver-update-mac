## Creating rasters of areas above certain threshold
## Two functions that can be used to subset rasters for only pixels that are above or below a certain threshold. Also have a version for if you wanted to do it by island


# simple version

extremes = function(r,q){
  qua = quantile(r[],q,na.rm=TRUE) # get the value that demarcates the top theshold
  Q = r > qua # filter your raster for only values above that 
  Q[Q==0]=NA # set all other values to NA
  r*Q # multiply original raster by Q to zero out any values you dont want
}


# with island calc - so this will give you back the top X pixels just for one island

extremes_by_island = function(r,q,island){
  
  # testing
  
  island = "Kauai"
  r = all_rec_fish
  q = .90
  
  # check extents (not working)
  all.equal(ext(r), ext(island_rast))
  
  
  # read in the island raster
  island_rast <- rast(file.path(paste0(data_dir,"/islands/",island,".tif")))
  # project r to island rast
  r_nad83 <- project(r,crs(island_rast))
  
  # calculate quantile
  r_island <- r_nad83 * island_rast # zero out everything else other than values for your island
  qua = quantile(r_island[],q,na.rm=TRUE) # get the value that demarcates the top theshold
  Q = r_island > qua # filter your raster for only values above that 
  Q[Q==0]=NA # set all other values to NA
  r_island*Q # multiply original raster by Q to zero out any values you dont want
}