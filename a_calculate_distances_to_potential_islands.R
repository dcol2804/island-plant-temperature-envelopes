##################################################################
# This code is to match up the mainland locations that lie below #
# the minimum island temperature and see how far they are        #
# from appropriate islands.                                      #
##################################################################

# load packages
library(tidyverse)
library(sf)

# load the database
load("AIsland_DB.RData")

# load the mainland occurrences
occ = readRDS("mainland population coordinates.rds")

# and the summary temp stats for each species
temp = read_csv("temperature comparison for each species1.csv")

######################
# Data explorations  #
######################

length(which(temp$lower_limit_mainland < min(temp$lower_limit_islands)))
length(which(temp$lower_limit_mainland < 13.67))
length(which(temp$lower_limit_min_mainland < temp$lower_limit_min_islands))
summary(env_data$bio_1)
summary(temp$lower_limit_islands)

# So clearly most species have a minimum temp lower than the island min temp
# as shown by this plot
par(mfrow = c(2, 1))
plot( temp$upper_limit_mainland,temp$upper_limit_islands,
      ylab = "Upper temperature limit of island populations (°C)",
      xlab = "Upper temperature limit mainland populations (°C)",
      xlim = c(4, 30), ylim = c(10, 30))
abline(a = 0, b = 1, col = "red")
plot( temp$lower_limit_mainland,temp$lower_limit_islands,
     ylab = "Lower temperature limit of island populations (°C)",
     xlab = "Lower temperature limit mainland populations (°C)",
     xlim = c(4, 30), ylim = c(10, 30))
abline(a = 0, b = 1, col = "red")

# Make a nice ggplot of the above
# load the trait data
traits = read_csv("240220_island_traits.csv")
temp = left_join(temp, traits)

p = ggplot(temp, aes(upper_limit_mainland, upper_limit_islands))+
  geom_point(aes(colour = woodiness), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic() +
  xlim(5, 30)+
  ylim(10,30)+
  xlab("Upper temperature limit mainland populations (°C)")+
  ylab("Upper temperature limit of island populations (°C)")+
  theme(legend.position = "none")

p

p1 = ggplot(temp, aes(lower_limit_mainland, lower_limit_islands))+
  geom_point(aes(colour = woodiness), alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic() +
  xlim(5, 30)+
  ylim(10,30)+
  xlab("Lower temperature limit mainland populations (°C)")+
  ylab("Lower temperature limit of island populations (°C)")+
  theme(legend.position = "none")

p1



###############################################
# Now calculate how far potential islands lie #
# from mainland occurrences                   #
###############################################

# Join the occurrences to the temperature limits for each species

occ1 = occ %>% left_join(temp)

# Clean it up and select the columns you need
occ2 = occ1 %>% mutate(diff_min_island_temp = lower_limit_islands - extracted_MAT) %>% 
  arrange(species_work, diff_min_island_temp) %>% 
  select(species_work, geometry, 
         extracted_MAT, lower_limit_islands, lower_limit_mainland, 
         diff_min_island_temp, upper_limit_mainland, upper_limit_islands)


# prepare a list of species
species = unique(occ2$species_work)
# and an empty dataframe
cold_islands_out = data.frame()

###################################################
# This loop lines up all the mainland occurrences # 
# next to each appropriate island                 #
# that the species could potentially inhabit      #
# according to their mainland occurrence range.   #
###################################################

for (i in 1:length(species)){
  
# for each species find the islands that have a MAT 
# inbetween the island and the mainland range lower limit

l_island = unique(occ2$lower_limit_islands[occ2$species_work == species[i]])
l_mainland = unique(occ2$lower_limit_mainland[occ2$species_work == species[i]])
cold_islands = env_data %>% filter(bio_1 < l_island, bio_1 > l_mainland) %>% select(island_ID, X, Y) 

# If there are no appropriate islands, go to the next. 
if (nrow(cold_islands)== 0){
  next()
}

cold_islands = cold_islands %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(4326)

# attach the species name 
cold_islands$species_work = species[i]

# Prepare the two sets of points into a dataframe
occ3 = occ2 %>% filter(species_work == species[i]) %>% select(species_work, extracted_MAT, geometry)
test = split(occ3, occ3$species_work)
test =  Map(st_combine, test)
test = do.call(c, test)
test = data.frame(species_work = species[i], test, n_pot_islands = length(which(!is.na(cold_islands$island_ID))))
test = test %>% rename(geometry_m = geometry)
cold_islands = left_join(test, cold_islands, by = "species_work")

cold_islands_out = rbind(cold_islands_out, cold_islands)

}

# now calculate the distances
out = numeric()
for (i in 1:nrow(cold_islands_out)){
  
o = as.numeric(st_distance(cold_islands_out$geometry[i], cold_islands_out$geometry_m[i]) )

out = c(out, o)

}

# and bind it to the dataframe and save!
cold_islands_out$distance = out
write_csv(cold_islands_out %>% select(-geometry, -geometry_m), "distance_to_potential_cold_islands.csv")


########################################
# do the same for the islands within   #
# the warmest fraction of the          #
# mainland population                  #
########################################

warm_islands_out = data.frame()

# plot as many as you like
for (i in 1:length(species)){
  
  # find the islands that are cold enough but don't have Poa annua
  l_island = unique(occ2$lower_limit_islands[occ2$species_work == species[i]])
  
  l_mainland = unique(occ2$lower_limit_mainland[occ2$species_work == species[i]])
  
  diff = l_mainland - l_island
  
  w_mainland = unique(occ2$upper_limit_mainland[occ2$species_work == species[i]])
  
  i_island = w_mainland + diff
  
  warm_islands = env_data %>% filter(bio_1 > i_island, bio_1 < w_mainland) %>% select(island_ID, X, Y) 
  
  if (nrow(warm_islands)== 0){
    next()
  }
  
  warm_islands = warm_islands %>% 
    st_as_sf(coords = c("X", "Y")) %>% 
    st_set_crs(4326)
  
  
  warm_islands$species_work = species[i]
  
  # Prepare the two sets of points into a dataframe
  occ3 = occ2 %>% filter(species_work == species[i]) %>% select(species_work, extracted_MAT, geometry)
  test = split(occ3, occ3$species_work)
  test =  Map(st_combine, test)
  test = do.call(c, test)
  test = data.frame(species_work = species[i], test, n_pot_islands = length(which(!is.na(warm_islands$island_ID))))
  test = test %>% rename(geometry_m = geometry)
  warm_islands = left_join(test, warm_islands, by = "species_work")
  
  warm_islands_out = rbind(warm_islands_out, warm_islands)
  
}


out1 = numeric()

for (i in 1:nrow(warm_islands_out)){
  
  o = as.numeric(st_distance(warm_islands_out$geometry[i], warm_islands_out$geometry_m[i]) )
  
  out1 = c(out1, o)
  
}

warm_islands_out$distance = out1

write_csv(warm_islands_out %>% select(-geometry, -geometry_m), "distance_to_warm_islands.csv")

########################################
# do the same for the islands within   #
# the current island range             #
#                                      #
########################################

current_islands_out = data.frame()

# plot as many as you like
for (i in 1:length(species)){
  
  # find the islands that are cold enough but don't have Poa annua
  l_island = unique(occ2$lower_limit_islands[occ2$species_work == species[i]])
  
  u_island = unique(occ2$upper_limit_islands[occ2$species_work == species[i]])
  
  current_islands = env_data %>% filter(bio_1 > l_island, bio_1 < u_island) %>% select(island_ID, X, Y) 
  
  if (nrow(current_islands)== 0){
    next()
  }
  
  current_islands = current_islands %>% 
    st_as_sf(coords = c("X", "Y")) %>% 
    st_set_crs(4326)
  
  
  current_islands$species_work = species[i]
  
  # Prepare the two sets of points into a dataframe
  occ3 = occ2 %>% filter(species_work == species[i]) %>% select(species_work, extracted_MAT, geometry)
  test = split(occ3, occ3$species_work)
  test =  Map(st_combine, test)
  test = do.call(c, test)
  test = data.frame(species_work = species[i], test, n_pot_islands = length(which(!is.na(current_islands$island_ID))))
  test = test %>% rename(geometry_m = geometry)
  current_islands = left_join(test, current_islands, by = "species_work")
  
  current_islands_out = rbind(current_islands_out, current_islands)
  
}


out2 = numeric()

for (i in 1:nrow(current_islands_out)){
  
  o = as.numeric(st_distance(current_islands_out$geometry[i], current_islands_out$geometry_m[i]) )
  
  out2 = c(out2, o)
  
}

current_islands_out$distance = out1

write_csv(current_islands_out %>% select(-geometry, -geometry_m), "distance_to_current_islands.csv")


################################################
# If you like, you can plot individual species #
################################################ 
par(mfrow = c(1,1))
# load the australia map
library(ozmaps)

# Also make a "cold groups" category to colour points appropriately
occ2 = occ2 %>% mutate(cold_groups = ifelse(diff_min_island_temp < 0, "Colder occurrences", "Normal occurrences"))



for (i in 500:520){
  
  # find the islands that are cold enough but don't have Poa annua
  u_island = temp$upper_limit_islands[temp$species_work == species[i]]
  l_island = temp$lower_limit_islands[temp$species_work == species[i]]
  l_mainland = temp$lower_limit_mainland[temp$species_work == species[i]]
  
  observed_islands = env_data %>% 
    filter(island_ID %in% species_AIsland$island_ID[species_AIsland$species_work == species[i]]) %>% 
    mutate(`Island type` = "Occupied") %>% 
    select(island_ID, X, Y, `Island type`)
  
  cold_islands = env_data %>% 
    filter(bio_1 < l_island, bio_1 > l_mainland) %>% 
    mutate(`Island type` = "Below island but within mainland niche") %>% 
    select(island_ID, X, Y, `Island type`)
  
  all_islands = env_data %>% 
    filter(bio_1 < u_island, bio_1 > l_island) %>% 
    mutate(`Island type` = "Within temperature niche") %>% 
    select(island_ID, X, Y, `Island type`)
  

  
  islands = rbind(cold_islands, all_islands, observed_islands) %>% 
    mutate(`Island type` = factor(`Island type`, c("Occupied", "Below island but within mainland niche", "Within temperature niche")))
  
  if (nrow(cold_islands) == 0){
 next()
  }
  
  mainland = occ2 %>% filter(species_work == species[i]) %>% mutate()
  mainland = cbind(mainland, st_coordinates(mainland$geometry))
  
  # plot
  library(ozmaps)
  t = ozmap_data("country")
  lin <- as.data.frame(st_coordinates(t$geometry)) 
  p = ggplot(data = lin, mapping = aes(x=X, y=Y, group = L2)) +
    geom_polygon(color = "black", fill = "white") +
    coord_quickmap() +
    theme_void() 
  
p +   geom_point(data = mainland, aes(x = X, y = Y, group = NULL)) +
  geom_point(data =islands, aes(x = X, y = Y, group = `Island type`, colour =  `Island type`))+
  theme(legend.position = "none")

  
}
