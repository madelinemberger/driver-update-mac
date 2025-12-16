## Examine Intermediate Products


### Hawaii


# hawaii - only one with just one file
int_gdb_path <- here("data/osds/shared-data/Intermediate_Products.gdb")

int_layers <- st_layers(int_gdb_path)

hi_int <- st_read(file.path(int_gdb_path), layer= "HAW_2023_update_backup_06_06_24")

# what are the three datasets? 

hi_pre2010 <- hi_int %>% filter(DATA_SET == "2010&Prior") #53530
hi_post2010 <- hi_int %>% filter(DATA_SET == "Post 2010") #6093
hi_na <- hi_int %>% filter(is.na(DATA_SET)) #4069 


# how many duplicate TMKS? do all of these have different dates?

hi_dup_tmks <- hi_int %>% group_by(TMK) %>% 
  filter(n()>1) 

# remove all information related to OSDS except date, bed and bath and reshape
# clean up dates so we just have a year column for each one

hi_dup_tmks_cl <- hi_dup_tmks %>% 
  select(TMK,OSDS_TYPE,OSDS_QTY,COMMERCIAL,B_ROOMS,BATHS,EFF_DATE,DATE_BASIS,DATA_SET) %>% 
  mutate(date_clean = as_date(EFF_DATE)) %>% 
  mutate(year = year(date_clean)) %>% 
  st_drop_geometry()


# osds type by year

hi_dup_tmks_osds_wd <- hi_dup_tmks_cl %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(TMK,COMMERCIAL), names_from = year, values_from = OSDS_TYPE)


# osds qty by year

hi_dup_tmks_qty_wd <- hi_dup_tmks_cl %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(TMK,COMMERCIAL), names_from = year, values_from = OSDS_QTY)


# bedrooms by year

hi_dup_tmks_bedrooms_wd <- hi_dup_tmks_cl %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(TMK,COMMERCIAL), names_from = year, values_from = B_ROOMS)

# bathrooms by year

hi_dup_tmks_bathrooms_wd <- hi_dup_tmks_cl %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(TMK,COMMERCIAL), names_from = year, values_from = BATHS)


```

Overall Hawaii seems pretty "clean" I guess. My main questions are:
  - Why would a TMK have -999 for bedrooms and an actual number for baths? I think this might be from a weird join prior. But might be good to check if that's coming from the dwell data


### Maui 
```{r}

st_layers(int_gdb_path)

# these look exactly the same so not sure, maybe one just has Julia's update columns and the other does not?
  maui_iws1 <- st_read(file.path(int_gdb_path), layer= "MauiNui_2023_IWS_update") #2024

maui_iws2 <- st_read(file.path(int_gdb_path), layer= "Mau_IWS_31_01_2010_to_03_15_2023") #2388

# do these have the same data? what are the extra 364?

maui_in1_butnot2 <- setdiff(maui_iws1$tmk, maui_iws2$tmk) 




```

### Kauai

```{r}

kauai_iws1 <- st_read(file.path(int_gdb_path), layer= "KAU_IWS_2023_update")

kauai_iws2 <- st_read(file.path(int_gdb_path), layer= "On_site_Sewage_Disposal_Systems_Kauai_nutrient_update") #



```


### Oahu

```{r}


oahu_points_with_dwell <- st_read(file.path(int_gdb_path), layer= "Oahu_address_Points_Including_Dwellings_IWS") #11

oahu_iws2 <- st_read(file.path(int_gdb_path), layer= "OAH_OSDS_2019_V2_merge_missing_points_10_01_24")



```

So these are confusing to me. 




Old code

```{r}
gdb_path <- here("data/osds/FINAL_OSDS_Products.gdb")

layers <- st_layers(gdb_path)

oahu_osds_23 <- st_read(file.path(gdb_path), layer = "Oahu_OSDS_2023") %>% 
  clean_names() %>% 
  mutate(class_desc = case_when(
    osds_class == 1 ~ "1: Septic - Tank, Bed, or Trench",
    osds_class == 2 ~ "2: Septic - Seepage Pit (or unknown)",
    osds_class == 3 ~ "3: Compost Toilet, Grey Water System, or ATU Pit",
    osds_class == 4 ~ "4: Cesspool",
    osds_class == 5 ~ "5: Multiple types",
    TRUE ~ "unknown"
  ))


total_pre2008 <- sum(oahu_osds_23$osds_qty[oahu_osds_23$year == "pre 2009"], na.rm = TRUE) #13175


oahu_osds_2009_2023 <- oahu_osds_23 %>% 
  #clean_names() %>% 
  filter(year != "pre 2009") %>% 
  mutate(osds_class = as.integer(osds_class)) %>% 
  #mutate(year = ifelse(year == "pre 2009",2008,year)) %>% 
  mutate(year = as.Date(year, format = "%Y")) %>% 
  mutate(year = year(year)) %>% 
  rowid_to_column()

total <- sum(oahu_osds_2009_2023$osds_qty)

oahu_osds_agg <- oahu_osds_2009_2023 %>% 
  as.data.frame() %>% 
  select(-starts_with("point")) %>% 
  group_by(year, class_desc) %>% 
  summarize(
    total = sum(osds_qty)
  )

oahu_osds_table <- oahu_osds_agg %>%
  pivot_wider(id_cols = "class_desc", names_from = "year", values_from = "total") %>% 
  adorn_totals(where = c("row","col"))


table_gt <- gt(oahu_osds_table)
gtsave(table_gt, "table_output.png")

# number by year, oahu
year_bar <- ggplot(oahu_osds_agg, aes(x = year, y = total, group = class_desc))+
  geom_bar(stat = "identity", position = "stack",aes(fill = class_desc))+
  theme_bw()

year_bar



# older 

oahu_osds_pre2009 <- oahu_osds_23 %>% 
  #clean_names() %>% 
  filter(year == "pre 2009") %>% 
  mutate(osds_class = as.integer(osds_class)) %>% 
  #mutate(year = ifelse(year == "pre 2009",2008,year)) %>% 
  mutate(year = as.Date(year, format = "%Y")) %>% 
  mutate(year = year(year)) %>% 
  rowid_to_column()


oahu_osds_old_agg <- oahu_osds_pre2009 %>% 
  as.data.frame() %>% 
  select(-starts_with("point")) %>% 
  group_by(class_desc) %>% 
  summarize(
    total = sum(osds_qty)
  ) %>% 
  adorn_totals(where = "row")

old_table_gt <- gt(oahu_osds_old_agg)

gtsave(old_table_gt, "old_table_output.png")
```
