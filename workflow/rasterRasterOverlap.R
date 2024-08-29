# Raster to Raster overlap comparison

# Code for executing a raster to raster overlap analysis
# Maddie Berger
# August 2024 - DRAFT



function(numer_rast,denom_rast){
  
  ################ Step 1: Create Quantiles #####################################################
  
  # One of the input rasters is reclassified to 1 for all areas above the percentile of interest (ie anything above 90%). Usually makes sense for this one to be the one that is not the denominator
  
  
  # reclassify numerator layers to NA and 1
  
  numer_rast_01 <- ifel(!is.na(numer_rast),1,NA)
  numer2_rast_01 <- ifel(!is.na(numer_rast_2),1,NA)
  
  # osds_runoff_top10_01 <- ifel(!is.na(osds_top_10),1,NA)
  
  # quick gut check: are the number of pixels in these the same as if we divded the total pixels by 10? 
  
  #total_numer_pixels <- global(summed_runoff, fun = "notNA")
  #total_denom_10 <- global(summed_runoff_top_10, fun = "notNA")
  
  #total_denom_10 / total_numer_pixels # this should be 10 %
  
  # Find total area of other raster to use as the demoninator for the "percent" calculation below
  
  denom_pixels <- global(denom_rast, fun = "notNA")
  
  ################### Step 2: Find intersection of highest quantile ###### ##################################################
  
  
  # multiply the two rasters together - this will "zero out" any areas that are not top 10 for both and leave you with a raster just for where the most instense areas, as defined by your threshold overlap
  
  overlap_rast <- numer_rast_01 * denom_rast
  overlap_pix <- global(overlap_rast, fun = "notNA")
  
  
  #overlap_rast2 <- number2_rast_01 * denom_rast
  #overlap_pix2 <- global(overlap_rast2, fun = "notNA")
  
  ############# Step 3:  calculate area of overlap and compare to area of all top 10 fishing
  
  percent_overlap_1 = overlap_pix[1,1] / denom_pixels[1,1] 
  #percent_overlap_2 = overlap_pix2[1,1] / denom_pixels[1,1]
  
  # put together in df
  
  results <- data.frame(
    `Total Area (pixels)` = c(denom_pixels),
    `Percent Overlap` = c(percent_overlap_1)
  )
  
  
  
  return(results)
  
  
}