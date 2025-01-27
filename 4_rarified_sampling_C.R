library(tidyverse)
library(sf)
library(raster)

occ1 = read_csv("clean_occ.csv")

data = read_csv("six_or_more_islands_list_all.csv") %>% 

dplyr::select(species_work, status, n) %>% 
  
  filter(status == "native") %>% 
  filter(n >= 6)


# reduce the occurrences to just those species that were on more than 6 islands
data_in = occ1 %>% filter(species_work %in% unique(data$species_work))

# create a blank dataframe
rare = data.frame()

for (i in 1:nrow(data)){

#  refresh the dataframe
thousand = data.frame()

# take the observations for that species
subset = data_in %>% filter(species_work == data$species_work[i]) 

# get the total number of samples
number_of_mainland = nrow(subset)

# get the number of islands
number_of_islands = data$n[which(data$species_work == data$species_work[i])]

if (number_of_islands > number_of_mainland){
  
  to_use = number_of_mainland
  
}else{
  
  to_use = number_of_islands
  
}

for (j in 1:1000){
 
# randomly select observations equal to the number of islands
  sample = subset[sample(nrow(subset), to_use),]

sample_summary = sample  %>% summarise(lower_limit = min(bio_1, na.rm = T),
                                       upper_limit = max(bio_1, na.rm = T),
                                       median = median(bio_1, na.rm = T),
                                       mean = mean(bio_1, na.rm = T),
                                       diff = upper_limit - lower_limit,
                                       upper_limit_max = max(bio_5, na.rm = T),
                                       lower_limit_min = min(bio_6, na.rm = T))

thousand = rbind(thousand, sample_summary)

}



total_summary = thousand %>% 
  summarise(across(1:length(thousand),  list(mean = mean, sd = sd)))

total_summary = total_summary %>% 
  mutate(species_work = data$species_work[i], number_of_islands = number_of_islands, number_of_mainland = number_of_mainland)

rare = rbind(rare, total_summary)

}

rare = rare %>% filter(!is.na(median_mean))

#Store the result
write_csv(rare, "six_islands_rarified_sampling.csv")





