
library(tidyverse)

########################################################################


##################### Read in the enormous R objects ##################
df_raw = rbind(readRDS("records_1193.rds"), readRDS("records_1840.rds"))

# filter to just herbarium records
df = df_raw %>% filter(str_detect(dataResourceName, "AVH"))

# take out records without georeferences
df = df %>% filter(!is.na(decimalLatitude))

# check the names ALA came up with in the search
x = read_csv("six_or_more_islands_list_all.csv")

# make a new column with clean names
df = df %>% mutate(species_work = str_replace(scientificName, " subsp\\..*| var\\..*| f\\..*| sp\\..*", ""))
madeup = df %>% filter(!species_work %in% x$species_work) %>% dplyr::select(scientificName) %>% unique()

# ok, fix up the species you want to keep manually
corrections = data.frame(ALA_name = c("Cyanothamnus anemonifolius", "Vernonia cinerea", "Chenopodium trigonon", "Eragrostis amabilis", "Critesion murinum", "Sarcocornia quinqueflora", "Heterozostera nigricaulis"),
                         AIslands_name = c("Boronia anemonifolia", "Cyanthillium cinereum", "Einadia trigonos", "Eragrostis tenella", "Hordeum leporinum", "Salicornia quinqueflora", "Zostera muelleri")
                         )

# attach on the corrections table
df = df %>% left_join(corrections, by = c("species_work" = "ALA_name"))
df = df %>% mutate(species_work = ifelse(!is.na(AIslands_name), AIslands_name, species_work)) %>% dplyr::select(-AIslands_name)

# get rid of any weird observations, so all names are now from the original list
df = df %>% filter(species_work %in% x$species_work)

# Duplicate of name and location - remove. Not so important as we're only looking at the top and bottom but still..
df = df %>% group_by(scientificName, decimalLatitude, decimalLongitude, month, year) %>% distinct() %>% ungroup()

# Remove observations of cultivated origin, either flagged as such in the AVH or containing the search terms cultivat* and/or garden in a free-text search of the collection record (1.9%, n=82,290 records),

k = df %>% filter(str_detect(locality, "[Gg]arden [Ii]sland|[Gg]ardeners [Bb]ay|Gardens Homestead|Bundaleer Gardens|Gardens Road|Garden Cove"))
g = df %>% filter(str_detect(locality, "CULTIVAT|[Cc]ultivat|GARDEN|[Gg]arden|[Pp]lanted")|
                  str_detect(occurrenceRemarks, "CULTIVAT|[Cc]ultivat|GARDEN|[Gg]arden|[Pp]lanted"),
                  !recordID %in% k$recordID)

df1 = df %>% filter(!recordID %in% g$recordID)

#### OK you have a clean dataset! #####

write_csv(df1, "clean_occurrences.csv")
