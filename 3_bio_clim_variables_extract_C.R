
library(sf);  library(raster); library(tidyverse)

load("AIsland_DB.RData")

####################################################
check = env_data %>% dplyr::select(island_ID, bio_1) %>% left_join(chelsa)

#### eliminate values offshore of the mainland from mainland shapefile 

mainland = shapefile("mainland.shp")
seas_geom = geometry(mainland)

occ = read_csv("clean_occurrences.csv")

# create a shapefile of the occurrences
coords = occ[,c("decimalLatitude", "decimalLongitude")]  %>% rename(y = decimalLatitude, x = decimalLongitude)
coordinates(coords) <- ~x + y
CRS.new = "+proj=longlat +datum=WGS84 +no_defs"
proj4string(coords) <- CRS.new 

# takes a long time
inside_mainland = sp::over(coords, seas_geom)
occ$on_mainland = inside_mainland

shapefile(coords, "occurrences_shape.shp", overwrite = T)

coords <- st_read("occurrences_shape.shp")
coords = st_make_valid(coords)


coords$bio_1 = as.numeric(raster::extract(raster("CHELSA_bio1_1981-2010_V.2.1.tif"), coords, fun = max))*.1 - 273.1

coords$bio_5 = as.numeric(raster::extract(raster("CHELSA_bio5_1981-2010_V.2.1.tif"), coords, fun = mean))*.1 - 273.1

coords$bio_6 = as.numeric(raster::extract(raster("CHELSA_bio6_1981-2010_V.2.1.tif"), coords, fun = mean))*.1 - 273.1

# SO very similar! 
out = cbind(occ %>% dplyr::select(recordID:species_work), coords)

#Now write the temps into a file
write_csv(out, "clean_occ.csv")
