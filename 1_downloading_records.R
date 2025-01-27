library(sf);  library(raster); library(tidyverse)

load("AIsland_DB.RData")

raster_file <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")

b_1 = data.frame(island_ID = AIsland_shape$island_ID, 
                 bio1 = as.numeric(raster::extract(raster_file, AIsland_shape, fun = max))*.1 - 273.1
)

raster_file <- raster("CHELSA_bio5_1981-2010_V.2.1.tif")

b_5 = data.frame(island_ID = AIsland_shape$island_ID, 
                 bio5 = as.numeric(raster::extract(raster_file, AIsland_shape, fun = max))*.1 - 273.1
)

raster_file <- raster("CHELSA_bio6_1981-2010_V.2.1.tif")

b_6 = data.frame(island_ID = AIsland_shape$island_ID, 
                 bio6 = as.numeric(raster::extract(raster_file, AIsland_shape, fun = max))*.1 - 273.1
)

chelsa = b_1 %>% left_join(b_5) %>% left_join(b_6)

write_csv(chelsa, "island_temps.csv")

data = species_AIsland %>% dplyr::select(list_ID, ref_ID, species, species_work, status) %>% 
  unique() %>%
  left_join(island_lists) %>% 
  left_join(geo_data %>% dplyr::select(island_ID, name_DB)) %>% 
  left_join(island_lists_env %>% dplyr::select(-geometry, -MAT, -MATWM, -MATCM, -AP, -APWM, -APDM)) %>% 
  left_join(chelsa) %>% 
  
  filter(!str_detect(species_work, " spec\\.| sect\\.| sp\\."))


number = data  %>% dplyr::select(species_work, name_DB) %>% 
  group_by(species_work) %>% summarise(n = n())


# remove species_works that appear on less than 9 islands

number = number %>% filter(n >=6)

# calculate a min and max for MAT of each species

data1 = data %>% filter(species_work %in% number$species_work) %>% 
  
  group_by(species_work) %>% 
  
  summarise(lower_limit = min(bio1, na.rm = T), 
            upper_limit = max(bio1, na.rm = T),
            upper_limit_max = max(bio5, na.rm = T),
            lower_limit_min = min(bio6, na.rm = T),
            diff = upper_limit - lower_limit
  )

write_csv(data1, "six_or_more_islands_list_all.csv")





# read in the species list with 6 or more species on each island
x = read_csv("six_or_more_islands_list_all.csv")

#install.packages("galah")

library(galah)

# set up galah API
galah_config(email = "david.coleman@sydney.edu.au", atlas = "Australia")
galah_config(atlas = "Atlas of Living Australia")
galah_config(directory = "ALA_galah_repo")

# create blank dataframes
out = data.frame()
range_out = data.frame()

# 
for(i in 1:nrow(x)){

occ <- galah_call() |>
  galah_identify(x$species_work[i]) |>
  galah_filter(country == "Australia") |>
  galah_select(group = c("basic", "event"),
               basisOfRecord,
               fieldNotes, 
               occurrenceRemarks,
               georeferenceRemarks,
               locationRemarks,
               locality, 
               verbatimLocality, 
               verbatimEventDate,
               raw_dateIdentified, 
               raw_eventDate,
               month,
               year,
               raw_decimalLongitude, 
               raw_decimalLatitude,
               verbatimLatitude,
               verbatimLongitude,
               el874 ,
               el674)|>
  galah_apply_profile(CSDM)|> # https://support.ala.org.au/support/solutions/articles/6000240256-getting-started-with-the-data-profiles for CSDM
  atlas_occurrences()

Sys.sleep(3)

# only use resources that are part of the AVH
keep = occ %>% filter(str_detect(dataResourceName, "AVH"))

# Put in a step checking how different they are once you take off the top and bottom

# compare quantiles of all records and just the AVH records
q = quantile(occ$el874, c(.02, .98), na.rm = T) 
q1 = quantile(keep$el874, c(.02, .98), na.rm = T) 

# create a dataframe with these summarising variables
range = data.frame(taxon_name = x$species_work[i], n_all = nrow(occ), all_min = q[1], all_max = q[2], n_AVH = nrow(keep), AVH_min = q1[1], AVH_max = q1[2])

# store all these
range_out = rbind(range_out, range)
out = rbind(out, occ)

}

# Save the result
saveRDS(out, "records_1840.rds")






