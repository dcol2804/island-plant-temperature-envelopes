
library(tidyverse)

load("AIsland_DB.Rdata")


#### read in the two kinds of data
six = read_csv("six_islands_rarified_sampling.csv")

all_envelopes = read_csv("six_or_more_island_list_all.csv") %>% 
  filter(species_work %in% six$species_work) %>% 
  dplyr::select(species_work, lower_limit,upper_limit, upper_limit_max, lower_limit_min,  diff)

# rename for wide format
oldnames = c("lower_limit","upper_limit", "upper_limit_max", "lower_limit_min",  "diff")
newnames_i = str_c(oldnames, "_islands")
newnames_m = str_c(oldnames, "_mainland")
all_envelopes_1 = all_envelopes %>% rename_at(vars(oldnames), ~ newnames_i) 
six_1 = six %>% dplyr::select(species_work, number_of_islands, number_of_mainland, lower_limit_mean, upper_limit_mean, upper_limit_max_mean, lower_limit_min_mean, diff_mean) %>% 
     rename(lower_limit = lower_limit_mean, 
            upper_limit = upper_limit_mean,  
            upper_limit_max = upper_limit_max_mean, 
            lower_limit_min = lower_limit_min_mean,
            diff = diff_mean) %>% 
  rename_at(vars(oldnames), ~ newnames_m) %>% 
  left_join(read_csv("six_islands_rarified_sampling.csv") %>% 
  dplyr::select(species_work, lower_limit_sd, upper_limit_sd, upper_limit_max_sd, lower_limit_min_sd, diff_sd))



comp = left_join(all_envelopes_1, six_1)


comp = comp %>% filter(lower_limit_sd != 0, upper_limit_max_sd != 0) %>%
  
                mutate(min = lower_limit_islands - lower_limit_mainland,
                       max = upper_limit_islands - upper_limit_mainland,
                       abs_max = upper_limit_max_islands - upper_limit_max_mainland,
                       abs_min = lower_limit_min_islands - lower_limit_min_mainland,
                       diff = diff_islands - diff_mainland,
                       lower_limit_std = min/lower_limit_sd,
                       upper_limit_std = max/upper_limit_sd,
                       upper_limit_max_std = abs_max/upper_limit_max_sd,
                       lower_limit_min_std = abs_min/lower_limit_min_sd,
                       diff_std = diff/diff_sd) 


native = species_AIsland %>% filter(status == "native") %>% dplyr::select(species_work) %>% unique()
comp = comp %>% filter(species_work %in% native$species_work)

write_csv(comp, "temperature comparison for each species.csv")

# Also look at the absolute (non-rarified) temperature difference
occ4 = read_csv("quick_version_six.csv")


comp = comp %>% left_join(occ4) %>% mutate(
  min_a = lower_limit_islands - lower_limit,
  max_a = upper_limit_islands - upper_limit,
  abs_max_a = upper_limit_max_islands - upper_limit_max,
  abs_min_a = lower_limit_min_islands - lower_limit_min,
  diff_a = diff_islands - (upper_limit- lower_limit)
)

comp = comp %>% mutate(upper_limit_std = ifelse(is.infinite(upper_limit_std), 0.0000000001, upper_limit_std),
                       lower_limit_min_std = ifelse(is.infinite(lower_limit_min_std), 0.0000000001, lower_limit_min_std))
##########################################

# delete after checking
#comp = comp %>% filter(species_work %in% number$species_work)

summary = data.frame(do.call(cbind, lapply(comp %>% dplyr::select(lower_limit_islands, lower_limit_mainland, lower_limit,
                                                                  upper_limit_islands, upper_limit_mainland, upper_limit,
                                                                  upper_limit_max_islands, upper_limit_max_mainland, upper_limit_max,
                                                                  lower_limit_min_islands, lower_limit_min_mainland, lower_limit_min,
                                                                  
                                                                  min:diff_a), summary))) %>% t() %>% as.data.frame()

summary$p.value = c(NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,

                    round(t.test(comp$lower_limit_islands, comp$lower_limit_mainland)[["p.value"]], 5),
                    round(t.test(comp$upper_limit_islands, comp$upper_limit_mainland)[["p.value"]], 5),
                    round(t.test(comp$upper_limit_max_islands, comp$upper_limit_max_mainland)[["p.value"]], 5),
                    round(t.test(comp$lower_limit_min_islands, comp$lower_limit_min_mainland)[["p.value"]], 5),
                    round(t.test(comp$diff_islands, comp$diff_mainland)[["p.value"]], 5),
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    round(t.test(comp$lower_limit_islands, comp$lower_limit)[["p.value"]], 5),
                    round(t.test(comp$upper_limit_islands, comp$upper_limit)[["p.value"]], 5),
                    round(t.test(comp$upper_limit_max_islands, comp$upper_limit_max)[["p.value"]], 5),
                    round(t.test(comp$lower_limit_min_islands, comp$lower_limit_min)[["p.value"]], 5),
                    round(t.test(comp$diff_islands, comp$upper_limit- comp$lower_limit)[["p.value"]], 5))


standard_dev = data.frame(stat = c("min", "max", "abs_max", "abs_min", "diff",
                                   "min_a", "max_a", "abs_max_a", "abs_min_a", "diff_a"), 
                          sd = c(sd(comp$min), sd(comp$max), sd(comp$abs_max), sd(comp$abs_min), sd(comp$diff),
                                 sd(comp$min_a), sd(comp$max_a), sd(comp$abs_max_a), sd(comp$abs_min_a), sd(comp$diff_a)))

write.csv(summary, "standard effect size summary1.csv")


larger_range = comp %>% filter(diff > 0.5) %>% dplyr::select(species_work, number_of_islands, number_of_mainland, diff)


# have a look at the growth forms with the largest envelopes
gf = read_csv("240220_island_traits.csv")

comp = comp%>% left_join(gf %>% dplyr::select(species_work, fruit_fleshiness, life_history, woodiness, leaf_mass_per_area)) %>% 
  mutate(woodiness = ifelse(woodiness == "semi-woody", "herbaceous", woodiness))

# plot the difference in MAT by woodiness

library(RColorBrewer)
brewer.pal(n = 5, "Dark2")
library(ggside)


upper = ggplot(comp %>% filter(!is.na(woodiness)), aes(x = upper_limit_mainland, y = upper_limit_islands 
                         )
)+
  geom_point(alpha = 0.5, aes(colour = woodiness))+
  theme_classic() +
  scale_y_continuous(limits = c(5, 30), breaks = c(5, 10, 15, 20, 25, 30))+
  scale_x_continuous(limits = c(5, 30), breaks = c(5, 10, 15, 20, 25, 30))+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
  xlab("Warm edge for mainland populations (°C)")+
  ylab("Warm edge for island populations (°C)")+
  scale_colour_manual(values = c("#56B4E9","#D55E00"))+
  geom_smooth(method = "lm", fill = NA, colour = "black", linewidth = 0.75)+
  theme(legend.position = "none")

  


upper

model = lm(lower_limit_islands ~ lower_limit_mainland, comp %>% filter(!is.na(woodiness)))
summary(model)

#Calculate test statistic
SSE0   <- sum((comp$upper_limit_islands-comp$upper_limit_mainland)^2);
SSEA   <- sum(model$residuals^2);
F_STAT <- ((nrow(comp)-2)/2)*((SSE0 - SSEA)/SSEA);
P_VAL  <- pf(q = F_STAT, df1 = 2, df2 = nrow(comp)-2, lower.tail = FALSE)


lower = ggplot(comp%>% filter(!is.na(woodiness)), aes(x = lower_limit_mainland, y = lower_limit_islands 
                     )
           )+
  geom_point(alpha = 0.5, aes(colour = woodiness))+
  theme_classic() +
  scale_y_continuous(limits = c(5, 30), breaks = c(5, 10, 15, 20, 25, 30))+
  scale_x_continuous(limits = c(5, 30), breaks = c(5, 10, 15, 20, 25, 30))+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
  xlab("Cool edge for mainland populations (°C)")+
  ylab("Cool edge for island populations (°C)")+
  scale_colour_manual(values = c("#56B4E9","#D55E00"))+
  geom_smooth(method = "lm", fill = NA, colour = "black", linewidth = 0.75)+
  theme(legend.position = "none")

lower

model = lm(lower_limit_islands ~ lower_limit_mainland, comp)
summary(model)
newdata = data.frame(lower_limit_mainland = 18)
predict(model, newdata)

length(which(comp$lower_limit_islands > comp$lower_limit_mainland))
length(which(comp$upper_limit_islands < comp$upper_limit_mainland))/nrow(comp)

upper_box = ggplot(comp%>% filter(!is.na(woodiness)), aes(x = woodiness, y = max, fill = woodiness))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("#56B4E9","#D55E00"))+
  theme(legend.position = "none")+
  ylab("Difference at the warm edge (°C)")+
  xlab("")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  geom_hline(yintercept = 0, linetype = "dashed")

upper_box

lower_box = ggplot(comp%>% filter(!is.na(woodiness), min> -6), aes(x = woodiness, y = min, fill = woodiness))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("#56B4E9","#D55E00"))+
  theme(legend.position = "none")+
  ylab("Difference at the cool edge (°C)")+
  xlab("Herbaceous          Woody")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  geom_hline(yintercept = 0, linetype = "dashed")

lower_box

woody_box = ggplot(comp%>% filter(!is.na(woodiness), min> -6), aes(x = woodiness, y = diff_mainland, fill = woodiness))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c("#56B4E9","#D55E00"))+
  theme(legend.position = "none")+
  ylab("Difference at the cool edge (°C)")+
  xlab("Herbaceous          Woody")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  geom_hline(yintercept = 0, linetype = "dashed")

woody_box

library(cowplot)

plot_grid(upper, upper_box, lower, lower_box, nrow = 2, rel_widths = c(1.5,1),  align = 'hv')

# native and exotic

comp = comp %>% left_join(species_AIsland %>% dplyr::select(species_work, status)) %>% unique()

box_plot4 <- ggplot(comp%>% filter(!is.na(status)), aes(x = factor(reorder(status, min)), y = min, fill = status))+
  geom_boxplot(alpha = 0.4)+
  theme_classic() + 
  xlab("Native Introduced")+
  ylab("min MAT") +
  stat_summary(fun.data = get_box_stats_diff, geom = "text", hjust = 0.5, vjust = 0.9)+
  theme(legend.position = "horizontal")

box_plot4  


