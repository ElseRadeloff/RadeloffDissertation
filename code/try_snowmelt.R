#install.packages ("rtry")
#getwd()


#library (rtry)

#path_to_data <- system.file("data", "TRY_data.txt", package = "rtry")
#path_to_data
#?system.file

#try_year <- rtry_import ("data/TRY_data.txt")

#rm (try_year)

try <- try_n %>% 
  # select columns to match Madi's data 
  select (SpeciesName, ObsDataID, StdValue, PlantGrowthForm, Lat, Lon, data_source, Genus, Reference) %>% 
  # rename columns to match Madi's data 
  rename (lat = Lat,
          lon = Lon,
          functional_group = PlantGrowthForm,
          sample_id = ObsDataID,
          n = StdValue,
          species = SpeciesName, 
          reference = Reference) %>% 
  # remove observations of trees and lichen
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  # remove observations without coordinates associated 
  filter (!is.na(lat)) %>% 
  filter (genus != "Agrostis"
          & genus != "Dactylis"
          & genus != "Melica"
          & genus != "Phalaris"
          & genus != "Trichophorum")

#write_xlsx(try, "data/try_year.xlsx")

try_year <- read_excel("data/try_year.xlsx")

try_year_filtered <- try_year %>% 
  filter (!is.na(year)) %>% 
  filter (year != "??")  %>% 
  filter (year != "1970-2009")
  #filter (year > 2000)
unique (try_year_filtered$lat)
unique (try_year_filtered$year)

try_snowmelt <- read_excel("data/try_snowmelt.xlsx")
ttt_snowmelt <- read_excel("data/ttt_snowmelt.xlsx")

try_snowmelt <- try_snowmelt %>% 
  select (species, year, snowmelt_date, sample_id, data_source) %>% 
  filter (!is.na(snowmelt_date)) %>% 
  mutate (snowmelt_date = yday(snowmelt_date)) %>% 
  #mutate(sample_id = as.character(sample_id)) %>% 
  left_join (n_precip_zones, by = "sample_id") %>% 
  select (-c(species.y, data_source.y)) %>% 
  rename (species = species.x,
          data_source = data_source.x)

ttt_snowmelt <- ttt_snowmelt %>% 
  select (species, year, snowmelt_date, sample_id, data_source) %>% 
  filter (!is.na(snowmelt_date)) %>% 
  mutate (snowmelt_date = yday(snowmelt_date)) %>% 
  #mutate(sample_id = as.character(sample_id)) %>% 
  left_join (n_precip_zones, by = "sample_id") %>% 
  select (-c(species.y, data_source.y)) %>% 
  rename (species = species.x,
          data_source = data_source.x)

madi_snowmelt <- n_precip_zones %>% 
  filter (data_source == "Madi") %>% 
  mutate (snowmelt_date = case_when (
    grepl (60.98, lat) ~ 167,
    grepl (69.5971, lat) ~ 158)) %>% 
  mutate (year = "2021 - 2022")

obs_snowmelt <- rbind (try_snowmelt, ttt_snowmelt, madi_snowmelt) %>% 
  filter (!is.na(functional_group)) %>% 
  mutate (leafn = n/10)
  

obs_snowmelt_count <- obs_snowmelt %>% 
  group_by (functional_group, snowmelt_date) %>% 
  tally()

# model 
obs_snowmelt_mod <- brm (leafn ~ snowmelt_date * functional_group + (1|genus), data = obs_snowmelt)

summary (obs_snowmelt_mod)
plot (obs_snowmelt_mod)
pairs (obs_snowmelt_mod)
pp_check(obs_snowmelt_mod)

random_effects_obs_snowmelt <- ranef(obs_snowmelt_mod)
print(random_effects_obs_snowmelt)

print(summary(obs_snowmelt_mod), digits = 4)

# save model 
#saveRDS(obs_snowmelt_mod, "models/obs_snowmelt_mod.RDS")

obs_snowmelt_mod <- readRDS("models/obs_snowmelt_mod.RDS")

# dummy data frame
obs_snowmelt_mod_data <- expand_grid(snowmelt_date = seq(100, 215, by = 5), 
                                   n = seq(0, 5, by = 0.5),
                                   functional_group = levels (as.factor(obs_snowmelt$functional_group)))
#genus = levels (as.factor(n_precip_zones$genus)))

obs_snowmelt_mod_pred <- obs_snowmelt_mod %>% 
  epred_draws(newdata = obs_snowmelt_mod_data, allow_new_levels = TRUE)

date <- c("Feb 19", "April 9", "May 29", "July 18")

# plot model 
(obs_snowmelt_mod_fit <- ggplot() +
    geom_point(data = obs_snowmelt, aes(x = snowmelt_date, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
  #add_predicted_draws(obs_snowmelt_mod) %>%  # adding the posterior distribution
   # ggplot(aes(x = snowmelt_date, y = n, color = ordered (functional_group), fill = ordered (functional_group))) +  
    #stat_lineribbon(aes(y = .prediction)) +
    #geom_point(data = obs_snowmelt_n, size = 3) +   # raw data
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    xlab("Snowmelt Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85),
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (obs_snowmelt_mod_fit, filename = "graphs/obs_snowmelt_mod_fit_pft.png")

date <- c("Feb 19", "April 9", "May 29", "July 18")

# plot model without legend, axes lables, etc for clarity when combining
(obs_snowmelt_plot_simple <- ggplot() +
    geom_point(data = obs_snowmelt, aes(x = snowmelt_date, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    #add_predicted_draws(obs_snowmelt_mod) %>%  # adding the posterior distribution
    # ggplot(aes(x = snowmelt_date, y = n, color = ordered (functional_group), fill = ordered (functional_group))) +  
    #stat_lineribbon(aes(y = .prediction)) +
    #geom_point(data = obs_snowmelt_n, size = 3) +   # raw data
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    #xlab("Snowmelt date (doy)") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text (size = 15),
          axis.title.x = element_blank(),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.y = element_text(margin = margin(r = 20))))
