## Offshore decay function
# Maddie Berger, May 2023

# This function applies the offshore decay equation to model lessening impact from land-based runoff as distance from shore increases


offshore_decay <- function(impact_rast,dist_rast,year){
  
  nearshore_impact <- impact_rast
  dist_from_shore <- dist_rast
  
  ## Make sure rasters align and stack them
  
  impact_ext <- terra::extend(nearshore_impact,dist_from_shore)
  
  dist_ext <- terra::extend(dist_from_shore, impact_ext)
  
  rstack <- c(impact_ext,dist_ext)
  
  ## Apply the decay function 
  
  nearshore_decay <- terra::lapp(
    rstack,
    fun = function(x,y){
      return(x * exp(-1 *(y^2.5)/10^7.5))
    }
  )
  
  
  ### Find the max value and divide the raster by that to normalize it
  ### Update July 2024 - I commented this out because we are going to want to normalize by the highest value for any of the rasters we make
  
  #nearshore_decay_01 <- nearshore_decay / minmax(nearshore_decay)[2]
  
  # Save raster to output folder - comment out based on if you are doing ag or golf
  #writeRaster(nearshore_decay, filename = paste0(ag_dir,"/output_data/final_Golf/V1/GolfRunoff_",year,"_decay_qaqc.tif"), overwrite = TRUE)
  #writeRaster(nearshore_decay, filename = paste0(ag_dir,"/output_data/final_AgRunoff/V1/AgRunoff_USGS_",year,"_decay_qaqc.tif"), overwrite = TRUE)
  
  #writeRaster(nearshore_decay_01, filename = paste0(dropbox_dir,"/Drivers/1_AgGolf_Runoff/R/R_int/GolfRunoff_",year,"_01.tif"), overwrite = TRUE)
  
  return(nearshore_decay)
  
  
}
