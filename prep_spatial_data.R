library(tidyverse)
library(raster) 
library(sf)
library(abind) # combine arrays
# Statistical mode
library(DescTools) 

library(rdwd)
library(pbapply)
library(parallel)
library(terra)
library(stars)


## Load libraries ----
library(tidyverse)
library(support.BWS)
library(mlogit)
library(gmnl)
library(units)
library(stargazer)


data <- readRDS("data700.rds")
#### Prepare data for analysis ####

### Create suitable data frame
### Load BIBD
bibd <- readRDS("data/bibd_field_phase.rds")
stargazer(bibd, type="text", out="bibd.htm")

### function to transform data format
formatDF <- function(subDF, numQes=1){
  df <-data.frame(b=rep(NA, nrow(data)))
  
  best <- sapply(1:nrow(df), function(i) 
    colnames(data[subDF])[which(data[i,subDF]==1)])
  
  worst <- sapply(1:nrow(df), function(i) 
    colnames(data[subDF])[which(data[i,subDF]==2)])
  
  df <- df %>% 
    dplyr::mutate(b =  dplyr::case_when( 
      startsWith(best, "prec_farming") ~ 1, 
      startsWith(best, "manag_rhythm") ~ 2,
      startsWith(best, "plant_protect") ~ 3,
      startsWith(best, "rotation") ~ 4,
      startsWith(best, "con_tillage") ~ 5, 
      startsWith(best, "fertilizer") ~ 6,
      startsWith(best, "res_crops") ~ 7,
      startsWith(best, "res_varieties") ~ 8,
      startsWith(best, "catch_crops") ~ 9, 
      startsWith(best, "mixed_crops") ~ 10,
      startsWith(best, "irrigation") ~ 11,
      startsWith(best, "insurance") ~ 12,
      startsWith(best, "business_div") ~ 13),
      
      w = dplyr::case_when( 
        startsWith(worst, "prec_farming") ~ 1, 
        startsWith(worst, "manag_rhythm") ~ 2,
        startsWith(worst, "plant_protect") ~ 3,
        startsWith(worst, "rotation") ~ 4,
        startsWith(worst, "con_tillage") ~ 5, 
        startsWith(worst, "fertilizer") ~ 6,
        startsWith(worst, "res_crops") ~ 7,
        startsWith(worst, "res_varieties") ~ 8,
        startsWith(worst, "catch_crops") ~ 9, 
        startsWith(worst, "mixed_crops") ~ 10,
        startsWith(worst, "irrigation") ~ 11,
        startsWith(worst, "insurance") ~ 12,
        startsWith(worst, "business_div") ~ 13))
  
  colnames(df) <- c(paste0("b", numQes), paste0("w", numQes))
  return(df)
}

bw1 <- formatDF(subDF=2:5,numQes = 1)
bw2 <- formatDF(subDF=6:9,numQes = 2)
bw3 <- formatDF(subDF=10:13,numQes = 3)
bw4 <- formatDF(subDF=14:17,numQes = 4)
bw5 <- formatDF(subDF=18:21,numQes = 5)
bw6 <- formatDF(subDF=22:25,numQes = 6)
bw7 <- formatDF(subDF=26:29,numQes = 7)
bw8 <- formatDF(subDF=30:33,numQes = 8)
bw9 <- formatDF(subDF=34:37,numQes = 9)
bw10 <- formatDF(subDF=38:41,numQes = 10)
bw11 <- formatDF(subDF=42:45,numQes = 11)
bw12 <- formatDF(subDF=46:49,numQes = 12)
bw13 <- formatDF(subDF=50:53,numQes = 13)

df_bw<-cbind(data$ID, bw1, bw2, bw3, bw4, bw5, bw6, 
             bw7, bw8, bw9, bw10, bw11, bw12, bw13)

# df_bw<-rename(df_bw,c("data$ID"="ID"))
colnames(df_bw)[1] <- "ID"
#deleting bws from data frame, so that I can replace it with bw.
data_no_bws <- data[,-(2:53)] 

# join data
df_final<-left_join(df_bw, data_no_bws, by = "ID")


east <-readRDS("data/plz_east_attribution.rds")
df_final$location <- parse_number(df_final$location) %>% as.character()
df_final$adapt_friends_d <- ifelse(df_final$adapt_friends==3, 1, -1)
df_final$climate_impact_d <- ifelse(df_final$climate_impact==2, 1, -1)
df_final$high_risk_d <- ifelse(df_final$risk_willingness>3, 1, -1)
df_final$perception_others_imp <- ifelse(df_final$perception_others<3, 1, -1)
df_final$organic <- ifelse(df_final$management_type==2, 1, -1)
df_final$loc_east <- ifelse(
  startsWith(df_final$location, "0") |
    startsWith(df_final$location, "1") |
    startsWith(df_final$location, "99") |
    startsWith(df_final$location, "98") |
    startsWith(df_final$location, "39") |
    df_final$location %in% east, 1, -1)
df_final$arable_d <- with(df_final, ifelse(business_type<3|business_type==7, 1,-1))

saveRDS(df_final, "df_final.rds")




## Load data monthly gridded data from DWD database

# Load string vector with all vailable directories (CDC)
data("gridIndex")

# Filter growing season subset, only directories with monthly gridded data from March till 
# September for the 10 years prior to 2020 are relevant (see Stetter,Sauer (2022))
years=as.character(1991:2020)

base_dwd <- as.data.frame(gridIndex) %>%
  mutate("month"=as.numeric(str_sub(gridIndex,-09,-8)),
         "year"=as.numeric(str_sub(gridIndex,-13,-10))) %>% 
  filter(year %in% years, 
         month %in% 3:10)


# growing season precipitation string index
precip_index <- base_dwd  %>% 
  filter(str_detect(gridIndex, "monthly_precipitation"))

# growing season mean temperature string index
temp_index <- base_dwd  %>% 
  filter(str_detect(gridIndex, "monthly_air_temp_mean"))

# Download monthly data
raster_rain <- stack(dataDWD(precip_index[[1]], base=gridbase, joinbf=TRUE, dir=locdir()))
raster_temp <- stack(dataDWD(temp_index[[1]], base=gridbase, joinbf=TRUE, dir=locdir()))

# Assign prjection 
# (https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_mean/DESCRIPTION_gridsgermany_monthly_air_temperature_mean_en.pdf)
crs(raster_rain) <- crs(raster_temp) <- "EPSG:31467"

## Delete unnecessary files to save memory
rm(gridIndex, precip_index, temp_index)


## 2.1 Yearly average temperature growing season
### Annual GS data at raster level
list_temp <- lapply(years, function(i) 
  raster_temp[[tidyselect::vars_select(
    names(raster_temp), 
    contains(i))]])

list_temp <- lapply(list_temp, function(x) 
  stackApply(x, indices = 1, fun='mean'))

## 2.2 Yearly precip sum - growing season
### Annual GS data at raster level
list_rain <- lapply( years, function(i) 
  raster_rain[[tidyselect::vars_select(
    names(raster_rain), 
    contains(i))]])

list_rain <- pblapply(list_rain, function(x) 
  stackApply(x, indices = 1, fun='sum'))


temp <- mean(stack(list_temp), na.rm=T)
rain <- mean(stack(list_rain), na.rm=T)*10


#--- convert to an sf object ---#
temp_poly <- st_as_sf(as(temp, 'SpatialPolygonsDataFrame'))
rain_poly <- st_as_sf(as(rain, 'SpatialPolygonsDataFrame'))

st_write(temp_poly, dsn="data/climate.gpkg", layer='gs_temperature', layer_options = "OVERWRITE=YES" )
st_write(rain_poly, dsn="data/climate.gpkg", layer='gs_rain', layer_options = "OVERWRITE=YES" )


# Training data shapes
#### Load shape data ####
shape <- sf::st_read("plz_spatial/plz-gebiete.shp")
df_final <- readRDS("df_final.rds")


shape5 <- shape %>% filter(shape$plz %in% df_final$location[which(nchar(df_final$location)==5)]) 

shape4 <- shape[str_sub(shape$plz, 1, 4) %in% df_final$location[which(nchar(df_final$location)==4)],] %>% 
  mutate(plz=shape$plz[str_sub(shape$plz, 1, 4) %in% df_final$location[which(nchar(df_final$location)==4)]] %>% str_sub(1,4))

shape3 <- shape[str_sub(shape$plz, 1, 3) %in% df_final$location[which(nchar(df_final$location)==3)],] %>% 
  mutate(plz=shape$plz[str_sub(shape$plz, 1, 3) %in% df_final$location[which(nchar(df_final$location)==3)]] %>% str_sub(1,3))

shape2 <- shape[str_sub(shape$plz, 1, 2) %in% df_final$location[which(nchar(df_final$location)==2)],] %>% 
  mutate(plz=shape$plz[str_sub(shape$plz, 1, 2) %in% df_final$location[which(nchar(df_final$location)==2)]] %>% str_sub(1,2))

shape1 <- shape[str_sub(shape$plz, 1, 1) %in% df_final$location[which(nchar(df_final$location)==1)],] %>% 
  mutate(plz=shape$plz[str_sub(shape$plz, 1, 1) %in% df_final$location[which(nchar(df_final$location)==1)]] %>% str_sub(1,1))


sf_use_s2(FALSE)
shape_list <- lapply(list(shape5,
                          shape4,
                          shape3,
                          shape2,
                          shape1), function(x)
                            x %>% 
                       dplyr::select(plz) %>% 
                       group_by(plz) %>%
                       summarise(geometry = sf::st_union(geometry)) %>%
                       ungroup()
)

nfkw_raster <- rast("data/soil/NFKWe1000_250.tif")
nfkw_raster <- terra::project(nfkw_raster, crs(shape_list[[1]]))

sqr_raster <- rast("data/soil/sqr1000_250_v10.tif")
sqr_raster <- terra::project(sqr_raster, crs(shape_list[[1]]))

sqi_raster <- rast("data/soil/esdac_biomass_production_geotiff/sqi_fig5_crop1.tif")
sqi_raster <- terra::project(sqi_raster, crs(shape_list[[1]]))

DEU <- sf::st_read("plz_spatial/DEU_adm0.shp") %>% 
  st_transform(st_crs(nfkw_raster))

GEM <- sf::st_read("data/PLZ/vg250_01-01.gk3.shape.ebenen/vg250_ebenen_0101/VG250_GEM.shp")%>% 
  st_transform(st_crs(nfkw_raster))


grid_10km <- sf::st_read("data/de_grid_10km/DE_Grid_ETRS89-UTM32_10km.gpkg") %>% 
  st_transform(st_crs(nfkw_raster))

for (i in 1:5) {
  shape_list[[i]][["temp"]] <- extract(temp, shape_list[[i]], weights=TRUE, 
                                       fun=mean, na.rm=T)[,1]
  shape_list[[i]][["rain"]] <- extract(rain, shape_list[[i]], weights=TRUE, 
                                       fun=mean, na.rm=T)[,1]
  
  shape_list[[i]][["nfkw"]] <- extract(nfkw_raster, shape_list[[i]], weights=TRUE, 
                                       fun=mean, na.rm=T)[[2]]
  
  shape_list[[i]][["sqr"]] <- extract(sqr_raster, shape_list[[i]], weights=TRUE, 
                                      fun=mean, na.rm=T)[[2]]
  
  shape_list[[i]][["sqi"]] <- extract(sqi_raster, shape_list[[i]], weights=TRUE, 
                                      fun=mean, na.rm=T)[[2]]
}

grid_10km[["temp"]] <- extract(temp, grid_10km, weights=TRUE, 
                               fun=mean, na.rm=T)[,1]

grid_10km[["rain"]] <- extract(rain, grid_10km, weights=TRUE, 
                               fun=mean, na.rm=T)[,1]

grid_10km[["nfkw"]] <- extract(nfkw_raster, grid_10km, weights=TRUE, 
                               fun=mean, na.rm=T)[[2]]

grid_10km[["sqr"]] <- extract(sqr_raster, grid_10km, weights=TRUE, 
                              fun=mean, na.rm=T)[[2]]

grid_10km[["sqi"]] <- extract(sqi_raster, grid_10km, weights=TRUE, 
                              fun=mean, na.rm=T)[[2]]


grid_10km <- grid_10km %>% 
  st_transform(st_crs(sf::st_read("data/de_grid_10km/DE_Grid_ETRS89-UTM32_10km.gpkg")))

grid_mask <- st_covered_by(st_transform(grid_10km, st_crs(DEU)), DEU, sparse = F)

df_final <- readRDS("df_final.rds")

df_final$plz = df_final$location
df_final_s <- left_join(df_final, do.call(rbind, shape_list), by="plz") 
df_final <- left_join(df_final, st_drop_geometry(do.call(rbind, shape_list)), by="plz") 

saveRDS(df_final, "df_final_spatial.rds")
saveRDS(df_final_s, "df_final_spatial_s.rds")
saveRDS(grid_10km[grid_mask,], "grid_10km.rds")




# DFs  for descriptive statistics maps ####
sqi_raster <- raster("data/soil/esdac_biomass_production_geotiff/sqi_fig5_crop1.tif")

temp_poly <- st_read(dsn="data/climate.gpkg", layer='gs_temperature') %>% 
  st_transform(crs(sqi_raster))

rain_poly <- st_read(dsn="data/climate.gpkg", layer='gs_rain') %>% 
  st_transform(crs(sqi_raster))

sqi_raster <- crop(sqi_raster, temp_poly)

soil_poly <- st_as_sf(as(sqi_raster, 'SpatialPolygonsDataFrame'))
aa <- st_within(soil_poly, temp_poly)

colnames(soil_poly)[1] <- colnames(temp_poly)[1] <- colnames(rain_poly)[1] <-
  "Index"


DEU <- sf::st_read("plz_spatial/DEU_adm0.shp") %>% 
  st_transform(st_crs(soil_poly))


soil_poly_de <- st_intersection(soil_poly, DEU)

st_write(soil_poly_de, dsn="data/climate.gpkg", layer='soil_poly_de', 
         layer_options = "OVERWRITE=YES" )

