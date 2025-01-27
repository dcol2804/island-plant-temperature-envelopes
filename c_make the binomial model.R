
library(tidyverse)
# to remove scientific notation
options(scipen=999)


# This greatly speeds things up
status2 = read_rds("distances_all.rds")

status2$numeric = ifelse(status2$present == T, 1, 0)
status2$original_out = status2$out
status2$out = log10(status2$out/1000)

load("AIsland_DB.Rdata")

# take out problem species and observations
prob = read_csv("excluded species.csv")
arch = env_data %>% filter(arch_lvl_2 %in% c(35, 36)) %>% left_join(geo_data)
status2 = status2 %>% filter(island_ID != 154, !name_DB %in% arch$name_DB, !species_work %in% prob$species)

############ EAST COAST ONLY LINE ########################################
# We now need to get just species-island combinations that are on the east coast!
#status2 = status2 %>% left_join(env_data %>% select(island_ID, X)) %>% filter(X> 143)


########### HERBACEOUS ONLY LINE ########################################
# Try making a model with seed mass
traits = read_csv("240220_island_traits.csv")

status2 = status2 %>% left_join(traits) 

status2 = status2 %>% mutate(seed_dry_mass_log = log10(seed_dry_mass), plant_height_log = log10(plant_height))



# Compare with the distance to mainland
status2 = status2 %>% left_join(env_data %>% dplyr::select(island_ID, dist_AUS, area)) %>% arrange(dist_AUS)

## Add on temperature as a comparison
t = read_csv("six_islands_rarified_sampling.csv")

status2 = status2 %>% 
  left_join(t %>% dplyr::select(species_work, mean_mean)) %>% 
  left_join(env_data %>% dplyr::select(island_ID, bio_1)) %>% 
  mutate(temp_dist = abs(bio_1 - mean_mean))

quantile(status2$original_out[status2$numeric == 1], .01)
length(which(status2$dist_AUS > status2$original_out))

# None are further than the distance to the mainland 


# look at range of 
summary(status2$out)

### Correlation coefficient between the two models!
cor(status2$out, status2$temp_dist,  use = "complete.obs")
# CReate the log odds
h = status2 %>%
  mutate(breaks = cut(out, breaks=seq(-1.5,3.7,0.1), labels=seq(-1.4,3.7,0.1), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) 

h1 = pivot_wider(h, names_from = numeric, values_from = n)

h1 = h1 %>% mutate(odds = `1`/`0`, total_obs = `1` + `0`)
h1 = h1 %>% filter(odds < 1)
# Take out the strange categories - 
h1 = h1 %>% filter(!is.na(`1`), !is.na(`0`))

# MAKE THE MODEL
model <- glm(numeric ~ out, family = binomial(link = "probit"), data = status2)
summary(model)

# predict over the range
pred = data.frame(x = seq(from = -.9, to = 3.5, by = 0.1))
pred$y = predict(object = model, newdata = list(out = pred$x), type = "response")

library(emmeans)
yhat.df <- emmeans(model, ~out, at=list(out=seq(-0.9,3.8,by=.01)), type='response') %>%
  as.data.frame()

# original plot!
plot(h1$breaks, h1$odds, xlab = "log of distance (km) to nearest mainland occurrence", ylab = "The odds of species occurrence at that distance", ylim = c(0, 1))
lines(pred$x, pred$y, col = "red")

# Now make the histogram for the ggplot

h2 = status2 %>% group_by(numeric) %>%
  mutate(breaks = cut(out, breaks=seq(-0.2,3.8,0.1), labels=seq(-0.1,3.8,0.1), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) %>%
  mutate(pct = ifelse(numeric==0, 1 - n/sum(n) - .4,  1 - n/sum(n)- .4)) 
sum(h2$n)
h2 = h2 %>% filter(numeric == 1, !is.na(breaks))

quantile(status2$out, .25)

ggplot() +
  
  geom_point(data = h1, aes(x = 10^breaks, y = odds ))+
  geom_segment(data=h2, size=4, show.legend=FALSE,
               aes(x=10^breaks, xend=10^breaks, y=0.6, yend=pct), colour = "#E7298A") +
  geom_line(data = pred %>% filter(y < 0.6), 
            aes(x = 10^x,y = y), colour="grey50", lwd=1) +
  geom_ribbon(data=yhat.df %>% filter(prob < 0.59), aes(x = 10^out, xmin = 10^out, ymin=asymp.LCL, xmax = out, ymax=asymp.UCL), fill='salmon', alpha=.4)+
  scale_x_log10(limits = c(0.5, 3500)) +
  xlab("Distance to nearest mainland occurrence record (km)") +
  ylab("Probability of a species \noccurrence record on an island") +
  theme_classic(base_size=12) + 
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0, 0.61), breaks = c(0, .1, .2, .3, .4, .5))


# Calculate the pseudo R-squared
# https://r.qcbs.ca/workshop06/book-en/binomial-glm.html
(model$null.deviance - model$deviance)/model$null.deviance


##

# MAKE THE MODEL
model1 <- glm(numeric ~ temp_dist, family = binomial(link = "probit"), data = status2)
summary(model1)

# predict over the range
pred1 = data.frame(x = seq(from = 0, to = 21, by = 0.5))
pred1$y = predict(object = model1, newdata = list(temp_dist = pred1$x), type = "response")

#
library(emmeans)
yhat.df1 <- emmeans(model1, ~ temp_dist, at=list(temp_dist=seq(0, 21, by=.5)), type='response') %>%
  as.data.frame()

summary(status2$temp_dist)
# CReate the log odds
h = status2 %>%
  mutate(breaks = cut(temp_dist, breaks=seq(0,21,0.5), labels=seq(0.5,21,0.5), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) 

h1 = pivot_wider(h, names_from = numeric, values_from = n)

h1 = h1 %>% mutate(odds = `1`/`0`, total_obs = `1` + `0`)
h1 = h1 %>% filter(odds < 1)

# Take out the strange categories - 
h1 = h1 %>% filter(!is.na(`1`), !is.na(`0`))

h2 = status2 %>% group_by(numeric) %>%
  mutate(breaks = cut(temp_dist, breaks=seq(0,21,0.2), labels=seq(0.2,21,0.2), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) %>%
  mutate(pct = ifelse(numeric==0,  n/sum(n),  1 - n/sum(n))) 
sum(h2$n)
h2 = h2 %>% filter(numeric == 1, !is.na(breaks))



plot(h1$breaks, h1$odds)
lines(pred1$x, pred1$y, col = "red")
ggplot() +
  
  geom_point(data = h1, aes(x = breaks, y = odds))+
  geom_segment(data=h2, size=4, show.legend=FALSE,
               aes(x=breaks, xend=breaks, y=numeric, yend=pct, colour=factor(numeric))) +
  geom_line(data = pred1, 
            aes(x = x,y = y), colour="grey50", lwd=1) +
  geom_ribbon(data=yhat.df1, aes(x = temp_dist, xmin = temp_dist, ymin=asymp.LCL, xmax = temp_dist, ymax=asymp.UCL), fill='salmon', alpha=.4)+
  scale_x_continuous(limits=c(0, 16)) +
  xlab("Difference between mean MAT of species and islands (°C)") +
  ylab("Probability of species occurrence on islands") +
  theme_classic(base_size=12) + 
  theme(legend.position = "none")

(model1$null.deviance - model1$deviance)/model1$null.deviance

# Now both
# MAKE THE MODEL
model2 <- glm(numeric ~ temp_dist+out, family = binomial(link = "probit"), data = status2)
summary(model2)

(model2$null.deviance - model2$deviance)/model2$null.deviance

DescTools::PseudoR2(model, which = "all")



summary(status2$seed_dry_mass_log)

# CReate the log odds
h = status2 %>%
  mutate(breaks = cut(seed_dry_mass_log, breaks=seq(-3.6,5.6,0.1), labels=seq(-3.5,5.6,0.1), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) 

h1 = pivot_wider(h, names_from = numeric, values_from = n)

h1 = h1 %>% mutate(odds = `1`/`0`, total_obs = `1` + `0`)
h1 = h1 %>% filter(odds < 1)
# Take out the strange categories - 
h1 = h1 %>% filter(!is.na(`1`), !is.na(`0`))

plot(h1$breaks, h1$odds, xlab = "log10 Seed dry mass (g)", ylab = "Probability of species existing on island")

# Create the log odds
h = status2 %>%
  mutate(breaks = cut(plant_height_log, breaks=seq(-3.6,5.6,0.1), labels=seq(-3.5,5.6,0.1), 
                      include.lowest=TRUE),
         breaks = as.numeric(as.character(breaks))) %>%
  group_by(numeric, breaks) %>% 
  summarise(n = n()) 

h1 = pivot_wider(h, names_from = numeric, values_from = n)

h1 = h1 %>% mutate(odds = `1`/`0`, total_obs = `1` + `0`)
h1 = h1 %>% filter(odds < 1)
# Take out the strange categories -
h1 = h1 %>% filter(!is.na(`1`), !is.na(`0`))
plot(h1$breaks, h1$odds, xlab = "log10 Plant height (m)", ylab = "Probability of species existing on island")

# Try making a model of just woody versus non-woody for the distance

# Look at chi squared groupings for woody and fleshy
library("gplots")
# 1. 
s = status2 %>% select(woodiness, fruit_fleshiness) %>% 
  filter(fruit_fleshiness != "semi-woody", fruit_fleshiness != "dry fleshy") %>% 
  table()
# 2. convert the data as a table
dt <- as.table(as.matrix(s))
# 3. Graph
balloonplot(t(dt), main ="Categorical traits", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# 
chisq <- chisq.test(dt)
chisq
model3 = glm(numeric ~ out*plant_height_log, family = binomial(link = "probit"), data = status2)
summary(model3)
(model3$null.deviance - model3$deviance)/model3$null.deviance
