### 3D Greyscale Basemaps
# Madeline Berger
# May 2025


# This R script creates some simple basemaps that can be used to plot or create map figures of data in this project for internal 
# and external sharing. These basemaps mirror what Joey Lecky did in the past. New versions we come up with and added here as needed

# To use, read in this script to a figure script and the basemaps will appear in your environment. You can then use ggplot to build on top.


# read in DEM coastlines for each island

hi_dem <- rast(file.path(data_dir,"dem_hi_smaller.tif"))
oahu_dem <- rast(file.path(data_dir,"dem_oahu_smaller.tif"))
mauinui_dem <- rast(file.path(data_dir,"dem_mauinui_smaller.tif"))
kauai_dem <- rast(file.path(data_dir,"dem_kauai_smaller.tif"))


coastlines <- read_sf(here("data/Coastlines_Watersheds/shoreline_CorrectedInlets_2022-23.shp")) %>% 
  st_transform(., crs = crs(hi_dem))

hi_coast <- filter(coastlines, Island == "Hawaii")
oahu_coast <- filter(coastlines, Island == "Oahu")
mauinui_coast <- filter(coastlines, Island %in% c("Maui","Molokai","Lanai","Kahoolawe"))
kauai_coast <- filter(coastlines, Island == "Kauai")

# convert dems to data frame for ggplot and add name to values column for each reference 

hi_dem_df <- terra::as.data.frame(hi_dem, xy = TRUE,na.rm = TRUE)
colnames(hi_dem_df)[3] <- "elevation"

oahu_dem_df <- terra::as.data.frame(oahu_dem, xy = TRUE,na.rm = TRUE)
colnames(oahu_dem_df)[3] <- "elevation"

mauinui_dem_df <- terra::as.data.frame(mauinui_dem, xy = TRUE,na.rm = TRUE)
colnames(mauinui_dem_df)[3] <- "elevation"

kauai_dem_df <- terra::as.data.frame(kauai_dem, xy = TRUE,na.rm = TRUE)
colnames(kauai_dem_df)[3] <- "elevation"

# build ggplots 
hi_with_dem <- ggplot()+
  geom_sf(data = hi_coast, fill = "white", color = "#696969", size = 0.3)+
  geom_raster(data = hi_dem_df, aes(x = x, y = y, fill = elevation), alpha = 0.7)+
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  coord_sf()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())

oahu_with_dem <- ggplot()+
  geom_sf(data = oahu_coast, fill = "white", color = "#696969", size = 0.3)+
  geom_raster(data = oahu_dem_df, aes(x = x, y = y, fill = elevation), alpha = 0.7)+
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  coord_sf()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())

oahu_with_dem

mauinui_with_dem <- ggplot()+
  geom_sf(data = mauinui_coast, fill = "white", color = "#696969", size = 0.3)+
  geom_raster(data = mauinui_dem_df, aes(x = x, y = y, fill = elevation), alpha = 0.7)+
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  coord_sf()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())

mauinui_with_dem


kauai_with_dem <- ggplot()+
  geom_sf(data = kauai_coast, fill = "white", color = "#696969", size = 0.3)+
  geom_raster(data = kauai_dem_df, aes(x = x, y = y, fill = elevation), alpha = 0.7)+
  scale_fill_gradientn(colours=c("#454545", "#e9e9e9"))+
  guides(fill = "none")+
  coord_sf()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(axis.ticks = element_blank())

kauai_with_dem
