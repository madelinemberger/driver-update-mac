# Set up packages and directors 

library(here)
library(tidyverse)
library(terra)
library(sf)
library(mapview)

# input / output directories

data_dir <- here("data")
out_dir <- here("outputs")


dropbox_dir <- c(
  'Windows' = paste0('C:/Users/', Sys.getenv("USERNAME"), "/Donovan Lab Dropbox/Donovan Lab Team Folder/Donovan_Lab_GIS"),
  'Linux' = "~/Donovan Lab Dropbox/Donovan Lab Team Folder/Donovan_Lab_GIS/",
  'Darwin' = "~/Donovan Lab Dropbox/Donovan Lab Team Folder/Donovan_Lab_GIS/"
)[[Sys.info()[['sysname']]]]


driver_dir <- file.path(dropbox_dir,"Drivers")
