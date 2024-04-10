library (tidyverse)
library (readr)
library(readxl)

# Reading in data ----

# Snow fence data 
toolik <- read.csv('data/toolik.csv')

toolik <- toolik %>% 
  # delete unnecessary columns 
  select ( -c (ID..Full, Date.Sampled, Year.Sampled, Species, X.C)) %>% 
  # rename columns
  rename (doy = Julian.Date, plot = Plot, treat = Snow.Zone, n= X.N, plant_type = Functional.Group) %>% 
  # reorder columns 
  relocate (plot, .after = doy) %>% 
  mutate (plant_type =ifelse(plant_type == "Deciduous Shrub", "Deciduous shrub", plant_type))


# graphing differences in N between different plant functional types 
(species <- ggplot (toolik, aes (x = plant_type, y = n)) +
  geom_boxplot(fill = '#a6d3a1')+
    theme_classic()
)

ggsave(species, filename = "graphs/species.png")

# graphing differences in how plant functional types respond to snow treatment 
(treatment_species <- ggplot (toolik, aes (x = treat, y = n, fill = plant_type)) +
    geom_boxplot() +
    theme_classic()
)

ggsave(treatment_species, filename = "graphs/treatment_species.png")
