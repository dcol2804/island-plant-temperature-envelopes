

########################################
# Now compare the typical distances to #
# where the occurrences are and to the # 
# binomial model                       #
########################################
# load the packages
library(tidyverse)
library(sf)
library(RColorBrewer)
# load the database
load("AIsland_DB.RData")

# Manipulate the distances to the same units as the binomial model
t = read_csv("distance_to_potential_cold_islands.csv")
t = t %>% mutate(km = distance/1000)
t = t %>% mutate(log_10_km = log10(km))
summary(t$log_10_km)

# need to get the distances of occurrences and the absences and plot them over the top. 
status2 = read_rds("distances_all.rds")

status2$numeric = ifelse(status2$present == T, 1, 0)
status2$original_out = status2$out
status2$out = log10(status2$out/1000)

# take out problem species and observations
prob = read_csv("excluded species.csv")
arch = env_data %>% filter(arch_lvl_2 %in% c(35, 36)) %>% left_join(geo_data)
status2 = status2 %>% filter(island_ID != 154, !name_DB %in% arch$name_DB, !species_work %in% prob$species)

# 
with_occ = rbind(status2 %>% filter(present == TRUE) %>% 
                   dplyr::select(species_work, island_ID, out) %>% mutate(Dataset = "Observed islands"), 
                 t %>% dplyr::select(species_work, island_ID, log_10_km) %>% 
                   rename(out = log_10_km) %>% mutate(Dataset = "Potential cool\nedge islands"))

#r = read_csv("distance_to_warm_islands.csv")
#q = r
q = read_csv("distance_to_current_islands.csv")
q = q %>% mutate(km = distance/1000)
q = q %>% mutate(log_10_km = log10(km))
summary(q$log_10_km)

with_occ = rbind(with_occ, 
                 q %>% dplyr::select(species_work, island_ID, log_10_km) %>% 
                   rename(out = log_10_km) %>% mutate(Dataset = "Potential islands within\ntemperature range"))

ggplot(with_occ, aes(out, fill = Dataset))+
  geom_density(alpha = 0.5)


ggplot(with_occ, aes(10^out, fill = Dataset))+
  geom_histogram(colour = "black", position="dodge", alpha = 0.5) +
  xlab("Distance from nearest mainland occurrence (km)")+
  ylab("Count of species/island combinations")+
  scale_y_log10()+
  theme_classic()
# Describe the n of each population in the figure description. 

#################################################################
# This is where I compare the mean and median of the amount of cold islands and distance to cold islands
# to the difference in cold temperatures. You would think those species not filling their climate niche would have less suitable islands nearby
################################################################


all = with_occ %>% filter(Dataset == "Potential islands within\ntemperature range") %>% filter(out < 2.5) %>% group_by(species_work) %>% summarise(n_all = n())
cold = with_occ %>% filter(Dataset == "Potential cool\nedge islands") %>% filter(out < 2.5) %>% group_by(species_work) %>% summarise(n_cold = n())
col = cold %>% group_by(species_work) %>% summarise(n = n())
#with_occ3 = left_join(all, cold) %>% mutate(ratio_n = n_cold/n_all) %>% left_join(temp %>% select(species_work, diff))


################ For plotting the various groups!! ########################

# This line selects your cutoff
data = with_occ %>% left_join(env_data %>% dplyr::select(island_ID, X, Y)) %>% 
  filter(out < 2.8) %>% 
 dplyr::select(-island_ID, -out)

# you need the mainland occurrences read in  and selecting just their coordinates. 
occ = read_csv("clean_occurrences.csv")
occ = occ %>% 
  mutate(Dataset = "mainland") %>% 
  rename(X = decimalLongitude, Y = decimalLatitude) %>% 
  dplyr::select(species_work, Dataset, X, Y)

data = data %>% rbind(occ)


# plot
library(ozmaps)
t = ozmap_data("country")
lin <- as.data.frame(st_coordinates(t$geometry)) %>% filter(X > 110, X< 155)
p = ggplot(data = lin, mapping = aes(x=X, y=Y, group = L2)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void() 


data = data %>% mutate(Dataset = factor(Dataset, levels = c("mainland", "Potential islands within\ntemperature range",  "Potential cool\nedge islands", "Observed islands")))

data = data %>% arrange(Dataset)
#tropical species
# Crotalaria montana

# temperate species
# Clematis microphylla is an interesting one
#Hydrocotyle alata
# Ruppia polycarpa
# Euchiton japonicus
# Patersonia fragilis
# to choose species
longitude = left_join(species_AIsland, env_data %>% select(island_ID, X)) %>% group_by(species_work) %>% summarise(mean_long = mean(X))
test = left_join(data %>% group_by(species_work) %>% summarise(n = n()), data %>% filter(Dataset == "Potential cool\nedge islands") %>% group_by(species_work) %>% summarise(n_cold = n())) %>% 
  left_join(read_csv("240220_island_traits.csv") %>% select(species_work, woodiness, plant_growth_form)) %>% left_join(longitude)

crot = data %>% filter(species_work == "Patersonia fragilis") 

p1  = p+
  geom_point(data =crot, aes(x = as.numeric(X), y = as.numeric(Y), group = Dataset, colour =  Dataset))+
  theme(legend.position = "none", plot.title = element_text(face = "italic", hjust = 0.55))+
  scale_color_manual(values = c("black", "#7570B3", "#1B9E77", "#E7298A" ))+
  ggtitle("Patersonia fragilis")

#Eucalyptus oraria

#Planchonella pohlmaniana
ac = data %>% filter(species_work == "Planchonella pohlmaniana")
p2 = p +
  geom_point(data =ac, aes(x = as.numeric(X), y = as.numeric(Y), group = Dataset, colour =  Dataset))+
  theme(legend.position = "none", plot.title = element_text(face = "italic", hjust = 0.55))+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", "#7570B3", "#1B9E77", "#E7298A" ))+
  ggtitle("Planchonella pohlmaniana")

#Hibbertia racemosa
ac = data %>% filter(species_work == "Hibbertia racemosa")
p3 = p +
  geom_point(data =ac, aes(x = as.numeric(X), y = as.numeric(Y), group = Dataset, colour =  Dataset))+
  theme(legend.position = "none", plot.title = element_text(face = "italic", hjust = 0.55))+
  theme(legend.position = "none")+
  scale_color_manual(values = c("black", "#7570B3", "#1B9E77", "#E7298A" ))+
  ggtitle("Hibbertia racemosa")

with_occ_final = with_occ %>% filter(out < 2.15) %>% group_by(Dataset, species_work) %>% summarise(mean = mean(out), median = median(out), n = n())

t.test(median ~ Dataset, data = with_occ_final %>% filter(Dataset != "Observed islands"))
tcrit = quantile(with_occ$out[with_occ$Dataset == "Observed islands"], .9, na.rm = T)

p4 = ggplot(with_occ_final, aes(x = Dataset, y = 10^mean, fill = Dataset)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylab("Mean distance to nearest \nmainland occurrence (km)")+
  scale_y_log10()+
  theme(axis.text.x = element_text(colour = "black", size = 11), legend.position = "none")+
  scale_fill_manual(values = c("#E7298A", "#1B9E77","#7570B3"  ))
  library(cowplot)
p5 = plot_grid(p1, p2, p3, p4)
p5
library(austraits)
a = load_austraits(version = get_version_latest()) 
chory = a$traits %>% filter(trait_name == "dispersal_syndrome", taxon_name %in% unique(data$species_work))
output1 = data.frame()
sp = unique(chory$taxon_name)
# for each of the categorical traits...
for (i in 1:length(sp)){
  
  # get austraits pg for this taxa and the trait, remove NA values and make a table of the counts for each value
  x = chory %>% filter(taxon_name == sp[i]) 
  
  x = x %>% dplyr::select(value) %>% unique()
  
  # make a character string made up of each unique trait value
  y = x$value %>% str_split(pattern = " ") %>% unlist() %>% sort() %>% str_c() %>% unique()
  y = paste0(y, collapse = " ")
  
  # make a row of pg made up of the taxon name, the trait name and the trait value character above (y). The units will be blank.
  z = data.frame(taxon_name = sp[i], trait_name = "dispersal_syndrome", trait_values = y)
  
  #glue it to the pgframe
  output1 = rbind(output1, z)
  
}



output2 = output1 %>% left_join(read_csv("dispersals.csv")) 

t = output2 %>% filter(!is.na(dispersal_group))

# look at dispersal
dispersals = t %>% rename(species_work = taxon_name) %>% left_join(with_occ_final%>% filter(Dataset == "Observed islands")) %>% 
 left_join(read_csv("temperature comparison for each species1.csv")) %>% 
 filter(dispersal_group != "hydrochory") %>% left_join(unique(species_AIsland %>% dplyr::select(species_work, status))) %>% 
 left_join(read_csv("C:/Users/MQ20182357/Macquarie University/Macquarie/Trait analysis/240220_island_traits.csv"))
dispersals = dispersals %>%  mutate(dispersal_group = as.factor(dispersal_group)) %>% filter(!mean %in% boxplot.stats(dispersals$mean)$out, !min %in% boxplot.stats(dispersals$min)$out) %>% 
 mutate(fruit_fleshiness = ifelse(fruit_fleshiness == "dry fleshy", "dry", fruit_fleshiness))

p1 = ggplot(dispersals, aes(x = dispersal_group, y = min, fill = dispersal_group)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()+
  ylim(c(-2.5, 7.5))+
  xlab("")+
  ylab("Difference at the cool edge (°C)")+
  theme(axis.text.x = element_text(colour = "black",  size = 11), legend.position = "none")

p3 = ggplot(dispersals, aes(x = dispersal_group, y = 10^mean, fill = dispersal_group)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_classic()+
  ylim(c(1, 2))+
  xlab("")+
  scale_y_log10(limits = c(10, 130))+
  ylab("Mean distance to nearest mainland occurrence (km)")+
  theme( axis.text.x = element_text(colour = "black", size = 11),legend.position = "none")


  
  
library(RColorBrewer)

dispersals2 = with_occ_final%>% filter(Dataset == "Observed islands") %>% 
  left_join(read_csv("temperature comparison for each species1.csv")) %>% 
  left_join(read_csv("C:/Users/MQ20182357/Macquarie University/Macquarie/Trait analysis/240220_island_traits.csv")) %>% 
mutate(fruit_fleshiness = ifelse(fruit_fleshiness == "dry fleshy", "dry", fruit_fleshiness))

table(dispersals2$fruit_fleshiness)

p2 = ggplot(dispersals2 %>% filter(!is.na(fruit_fleshiness)), aes(x = fruit_fleshiness, y = min, fill = fruit_fleshiness)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  ylim(c(-2.5, 7.5))+
  ylab("")+
  theme(axis.text.x = element_text(colour = "black", size = 11), legend.position = "none")+
  scale_fill_manual(values = c( "#4d8c57",  "navy"))

p4 = ggplot(dispersals2%>% filter(!is.na(fruit_fleshiness)), aes(x = fruit_fleshiness, y = 10^mean, fill = fruit_fleshiness)) +
  geom_boxplot()+
  theme_classic()+
  xlab("")+
  scale_y_log10(limits = c(10, 130))+
  ylab("")+
  theme(axis.text.x = element_text(colour = "black", size = 11), legend.position = "none")+
  scale_fill_manual(values = c( "#4d8c57",  "navy"))



library(cowplot)
plot_grid(p1, p2, p3, p4,  nrow = 2, rel_widths = c(2,1),  align = 'hv', labels = c("A", "B", "C", "D"), label_x = c(0.1, 0.2, 0.1, 0.2), label_size = 20)

# remove outliers
res_aov <- aov(mean ~ fruit_fleshiness,
               data = dispersals
)

summary(res_aov)

hist(res_aov$residuals)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

# Tukey HSD test:
library(agricolae)

post_test <- HSD.test(res_aov,
                  "dispersal_group", group = T)
post_test$groups
post_test <- glht(res_aov, linfct = mcp(dispersal_group = "Tukey"))
summary(post_test)

# now do distance per species
all1 = with_occ %>% filter(Dataset == "Potential islands within\ntemperature range") %>% group_by(species_work) %>% summarise(mean_all = mean(out, na.rm = T), median_all = median(out, na.rm = T))
cold1 = with_occ %>% filter(Dataset == "Potential cool\nedge islands ") %>% group_by(species_work) %>% summarise(mean_cold = mean(out, na.rm = T), median_cold = median(out, na.rm = T))
#with_occ4 = left_join(all1, cold1) %>% mutate(ratio_d = mean_cold/mean_all) %>% left_join(temp %>% select(species_work, diff))                                                                                                        




#############################################################################


