library(rgdal)
library(sf)
library(tidyverse)
#### Load shape data ####
shape_plz <- sf::st_read("GIS/plz-3stellig.shp")
shape_de <- sf::st_read("GIS/DEU_adm1.shp") %>% 
  st_transform(st_crs(shape_plz))

shapeAll <- st_join(shape_plz, shape_de, join = st_intersects, largest=T)

# shapeAll <- shapeAll %>% 
#  group_by(plz) %>%
#  summarise(geometry = sf::st_union(geometry)) %>%
#  ungroup()

east_de <- c("Berlin", "Brandenburg", "Mecklenburg-Vorpommern", 
  "Sachsen-Anhalt", "Sachsen", "Thüringen")            

shapeAll %>% 
  filter(NAME_1 %in% east_de) %>% 
  select(plz) %>% 
  st_drop_geometry() %>% 
  unique

d <- df_final$location %>% substr(1, 3)
  
east <- shapeAll %>% 
  filter(NAME_1 %in% east_de) %>% 
  select(plz) %>% 
  st_drop_geometry() %>% 
  unique %>% 
  unlist 

west <- shapeAll %>% 
  filter(!NAME_1 %in% east_de) %>% 
  select(plz) %>% 
  st_drop_geometry() %>% 
  unique %>% 
  unlist 

intersect(west,east)

write_rds(x = east, file = "data/plz_east_attribution.rds")
