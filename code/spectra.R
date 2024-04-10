# working with spectral n measurements 
# From Townsend lab/Kyle's data from the ABoVE project 

# Else Radeloff 
# Feb 23, 2024
# Undergraduate dissertation 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Loading loading ----

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# libraries

library (tidyverse)
library (readr)
library (geodata)
library (terra)
library (readxl)
library (writexl)
library (brms)
library (tidybayes)
library (lubridate)
library (ggeffects)
library (gridExtra)

# data 

above_plots <- read_csv("data/ABoVE_plots.csv")
above_points <- read_csv("data/ABoVE_points.csv")

dry_spectra <- read_csv("data/spectra_traits_dry.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Data Wrangling ----

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# adjusting column and plot names 
#spectra <- dry_spectra %>% 
#  select (Sample, t_mean_Nitrogen, ) %>% 
#  rename (sample = Sample, n = t_mean_Nitrogen) %>% 
#  # remove 'ABV_' from start 
#  mutate (plot = str_remove (sample, "ABV_")) %>% 
#  # then make it only the first 8-9 characters 
#  mutate (plot = substr(plot, 1, 9))

# selecting and renaming columns in the plot coordinates spreadsheet 
#points <- above_points %>% 
#  select (Name, Lat, Lon) %>% 
#  rename (plot = Name, lat = Lat, lon = Lon)

# selecting and renaming columns in the spreadsheet w plot level info 
#plots <- above_plots %>% 
#  select (title, lat_9_Plot_Center, long_9_Plot_Center, `10_Plot_Type`, `44_Species_1`, `46_Species_2`, `48_Species_3`) %>% 
#  rename (plot = title, 
#          lat = lat_9_Plot_Center, 
#          lon = long_9_Plot_Center, 
#          plot_type = `10_Plot_Type`, 
#          species1 = `44_Species_1`, 
#          species2=`46_Species_2`, 
#          species3 = `48_Species_3`) %>% 
#  mutate (species = ifelse (plot_type == "Single Species Patch Tree/Shrub", species1, NA)) %>% 
#  mutate (species = ifelse (plot_type == "Single Species Box Ground", species1, species)) 



# adding the spectra information to the list of coordinates 
#spectra_points <- spectra %>% 
#  left_join (points, by = 'plot') #%>% 
  #filter (!is.na(lat))

#no_points <- spectra_points %>% 
#  filter (is.na(lat))

# adding spectra info to the plot level data 
#spectra_plots <- spectra %>% 
#  left_join (plots, by = 'plot') #%>% 
  #filter (!is.na(lat)) 

# turning it into an excel spreadsheet so I can add species info 

#write_xlsx(spectra_plots, "data/above_plots_March7.xlsx")

#write_xlsx(points, "data/above_points_2.xlsx")
#write_xlsx(spectra_plots, "data/above_plots_species.xlsx")


# loading the data back in after editing in excel 
labled_plots  <- read_excel("data/above_plots_March7.xlsx")

labled_plots <- labled_plots %>% 
  filter (!is.na(plant_type)) %>% 
  filter (plant_type != "tree") %>% 
  select (!c(species1, species2, species3)) %>% 
  mutate (plant_type = (ifelse(plant_type == "eergreen shrub", "evergreen shrub", plant_type)))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## Add precip data ----

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load WorldClim data with the geodata library 

precip_year <- worldclim_global (var = "prec", res = 10, path = 'data')

precip_year_1km <- worldclim_global (var = "prec", res = 0.5, path = 'data')


#Define projections
WGSCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

months <- c(10,11,12,1,2,3,4)

precip_winter <- precip_year[[months]]
precip_winter_1km <- precip_year_1km[[months]]

precip_1km <- mean(precip_winter_1km)
#crs(precip) 


# make a vector layer of the coordinates  
spectra_vect_points <- vect (labled_plots, geom=c("lon", "lat"), crs=WGSCRS)

# make three separate vector layers based on n concentration 

plots_avg_n <- labled_plots %>% 
  group_by (lat, lon) %>% 
  summarise (avg_n = mean(n))

spectra_high_n_points <- plots_avg_n %>% 
  filter (avg_n >3)

spectra_med_n_points <- plots_avg_n %>% 
  filter (avg_n >2) %>% 
  filter (avg_n <3)

spectra_low_n_points <- plots_avg_n %>% 
  filter (avg_n <2)

spectra_high_n_vect <- vect (spectra_high_n_points, geom=c("lon", "lat"), crs=WGSCRS)

spectra_med_n_vect <- vect (spectra_med_n_points, geom=c("lon", "lat"), crs=WGSCRS)

spectra_low_n_vect <- vect (spectra_low_n_points, geom=c("lon", "lat"), crs=WGSCRS)

# extract mean precipitation 
spectra_points_precip_1km <- terra::extract(precip_1km, spectra_vect_points)

# add precipitation data to trait datasheet 
spectra_precip_1km <- labled_plots %>% 
  mutate (ID = as.numeric(interaction(lat, lon, drop=TRUE))) %>% 
  left_join (spectra_points_precip_1km, by = "ID") %>% 
  rename (mean_precip = mean)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Add arctic zone 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load arctic zones map (high arctic, low arctic, sub arctic)
zones <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")
#crs (zones)

# changing projections to a top down view of the arctic to match zones map

#re-projecting raster into top down view of the Arctic 
precip_proj <- terra::project(precip, "EPSG:3408")
#crs (precip_proj)

# re-projecting coordinates to the top down view of arctic 
spectra_vect_points_proj <- project (spectra_vect_points, "EPSG:3408")
#crs(spectra_vect_points_proj)

spectra_high_n_proj <- project (spectra_high_n_vect, "EPSG:3408")
spectra_med_n_proj <- project (spectra_med_n_vect, "EPSG:3408")
spectra_low_n_proj <- project (spectra_low_n_vect, "EPSG:3408")

# extract zone 
spectra_points_zone <- terra::extract(zones, spectra_vect_points_proj) %>% 
  select (id.y, Zone) %>% 
  rename (ID = id.y, zone = Zone)

# add zone data to trait datasheet 
spectra_precip_zones <- spectra_precip_1km %>% 
  left_join (spectra_points_zone, by = "ID") %>% 
  # removing points below sub arctic 
  filter (!is.na(zone)) %>% 
  mutate (zone2 = (ifelse(zone == "High arctic", "Low arctic", zone))) %>% 
  filter (plant_type != "fern" 
          & plant_type != "lichen"
          & plant_type != "moss"
          & plant_type != "herb") %>% 
  # remove undersampled genus's
  filter (Genus != "Chamaedaphne"
          & Genus != "Oxytropis"
          & Genus != "Rosa"
          & Genus != "Sheperdia"
          & !is.na (Genus))


#write_xlsx(spectra_precip_zones, "data/spectra_points.xlsx")


  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Taking a look at the data ----

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Scatter plot of mean precipitation vs. leaf nitrogen 
(precip_vs_n_spectra <- ggplot (spectra_precip_zones, aes(x=mean_precip, y = n, col = plant_type)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)+
    theme_classic() +
    xlab ("Mean Precipitation") +
    ylab ("Nitrogen Content")) 

ggsave(precip_vs_n_spectra, filename = "graphs/precip_vs_spectral_n.png")


# Final precip model ----

spectra_precip_mod <- brm (n ~ mean_precip * plant_type + (1|Genus), data = spectra_precip_zones)
# random slope not an option

summary (spectra_precip_mod)

plot (spectra_precip_mod)
pairs (spectra_precip_mod)
pp_check(spectra_precip_mod)

random_effects_genus <- ranef(spectra_precip_mod)
print(random_effects_genus)

# save model 
#saveRDS(spectra_precip_mod, "models/spectra_precip_mod.RDS")

spectra_precip_mod <- readRDS("models/spectra_precip_mod.RDS")

# high res model
spectra_precip_1km_mod <- brm (n ~ mean_precip * plant_type + (1|Genus), data = spectra_precip_zones)


summary (spectra_precip_1km_mod)

plot (spectra_precip_1km_mod)
pairs (spectra_precip_1km_mod)
pp_check(spectra_precip_1km_mod)

random_effects_genus <- ranef(spectra_precip_1km_mod)
print(random_effects_genus)

# save model 
saveRDS(spectra_precip_1km_mod, "models/spectra_precip_1km_mod.RDS")

spectra_precip_1km_mod <- readRDS("models/spectra_precip_1km_mod.RDS")

# Plot model results ----

spectra_precip_mod_data <- expand_grid(mean_precip = seq(4, 36, by = 1), 
                                    n = seq(0, 5, by = 0.5),
                                    plant_type = levels (as.factor(spectra_precip_zones$plant_type)),
                                    Genus = levels (as.factor(spectra_precip_zones$Genus)))

spectra_precip_mod_pred <- spectra_precip_1km_mod %>% 
  epred_draws(newdata = spectra_precip_mod_data, allow_new_levels = TRUE)


(spectra_precip_1km_mod_fit <- ggplot () +
    geom_point(data = spectra_precip_zones, aes(x = mean_precip, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +   # raw data
    stat_lineribbon(data = spectra_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (plant_type), fill = ordered (plant_type)), .width = c(.95), # regression line and CI
                   alpha = 0.25) +
   scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
   scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
   ylab("Leaf Nitrogen Concentration (%)") +  
   xlab("Mean Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,90)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5)) +
   theme_classic() +
   theme(legend.title = element_blank(),
         legend.position = "none",
         axis.text = element_text (size = 15),
         axis.title=element_text(size=14,face="bold"),
         legend.text = element_text (size = 15)))
  


ggsave (spectra_precip_1km_mod_fit, filename = "graphs/spectra_precip_1km_mod_fit_pft.png")

#ggsave (spectra_precip_mod_fit, filename = "accidental_art/spectra_precip_mod_fit_more_colors.png")

# put the two graphs (also obs model) on top of eachother in the same figure 
(precip_combined_plot <- grid.arrange(obs_precip_plot_simple, spectra_precip_1km_mod_fit,  ncol = 1))

ggsave (precip_combined_plot, filename = "graphs/precip_combined_plot.png")



# Snowmelt model ----

# adding snowmelt data back in
spectra_snowmelt_full <- read_excel("data/snowmelt.xlsx")

spectra_snowmelt <- spectra_snowmelt_full %>% 
  select (plot, lat, lon, snowmelt_date) %>% 
  mutate (snowmelt_date = yday(snowmelt_date))

spectra_snowmelt_n <- spectra_precip_zones %>% 
  left_join(spectra_snowmelt, by = "plot") %>% 
  filter (!is.na(snowmelt_date)) 

spectra_snowmelt_count <- spectra_snowmelt_n %>% 
  group_by (plant_type) %>% 
  tally ()


# model 
spectra_snowmelt_mod <- brm (n ~ snowmelt_date * plant_type + (1|Genus), data = spectra_snowmelt_n)
  
summary (spectra_snowmelt_mod)

plot (spectra_snowmelt_mod)
pairs (spectra_snowmelt_mod)
pp_check(spectra_snowmelt_mod)

random_effects_spectra_snowmelt <- ranef(spectra_snowmelt_mod)
print(random_effects_spectra_snowmelt)

# save model 
#saveRDS(spectra_snowmelt_mod, "models/spectra_snowmelt_mod.RDS")

spectra_snowmelt_mod  <- readRDS("models/spectra_snowmelt_mod.RDS")

print(summary(spectra_snowmelt_mod), digits = 4)

# plot model snowmelt model ----

spectra_snowmelt_mod_data <- expand_grid(snowmelt_date = seq(75, 190, by = 1), 
                                       n = seq(0, 5, by = 0.5),
                                       plant_type = levels (as.factor(spectra_snowmelt_n$plant_type)))

spectra_snowmelt_mod_pred <- spectra_snowmelt_mod %>% 
  epred_draws(newdata = spectra_snowmelt_mod_data, allow_new_levels = TRUE)

date <- c("Feb 19", "April 9", "May 29", "July 18")

(spectra_snowmelt_mod_fit <- ggplot() +
    #add_predicted_draws(spectra_snowmelt_mod) %>%  # adding the posterior distribution
    #ggplot(aes(x = snowmelt_date, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +  
    #stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), # regression line and CI
   #                 alpha = 1/4) +
    geom_point(data = spectra_snowmelt_n, aes(x = snowmelt_date, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +   # raw data
    stat_lineribbon(data = spectra_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (plant_type), fill = ordered (plant_type)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_x_continuous(expand = c(0, 0), limits = c(50,225), breaks = seq(50, 225, by = 50), labels = date) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") +  
    xlab("Snowmelt Date") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
         axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
         axis.title.y = element_text(margin = margin(r = 20))))

ggsave (spectra_snowmelt_mod_fit, filename = "graphs/spectra_snowmelt_mod_fit_pft.png")


# put the two graphs on top of eachother in the same figure 
(snowmelt_combined_plot <- grid.arrange(
  obs_snowmelt_plot_simple, 
    #ggtitle ("A") +
  spectra_snowmelt_mod_fit,
   # ggtitle ("B") +  
  ncol = 1))

ggsave (snowmelt_combined_plot, filename = "graphs/snowmelt_combined_plot.png")

# whittaker plot ----

# download and extract worldclim temp data

temp_months <- worldclim_global (var = "tavg", res = 10, path = 'data')

temp <- mean (temp_months)

arctic_spectra_temp <- terra::extract(temp, spectra_vect_points)

# add temp data to trait datasheet 
spectra_precip_temp <- spectra_precip_zones %>% 
  mutate (ID = row_number()) %>%  
  left_join (arctic_spectra_temp, by = "ID") %>% 
  rename (temp = mean)



spectra_common_species <- spectra_precip_zones %>% 
  group_by (species) %>% 
  tally () %>% 
  filter (n > 1) %>% 
  pull (species)

spectra_species_avg <- spectra_precip_temp %>% 
  filter(species %in% spectra_common_species) %>% 
  group_by (plant_type, Genus, species) %>% 
  summarise(
    avg_nitrogen = mean(n),
    avg_precipitation = mean(mean_precip),
    avg_temp = mean (temp)
  ) %>% 
  filter (!is.na (avg_temp))

(whittaker_spectra <- ggplot (data = spectra_species_avg, aes (x = avg_temp, 
                                                   y = avg_precipitation,  
                                                   col = plant_type)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    xlab ("Average Annual Temperature (°C)") +
    ylab ("Annual Preciptitaion (mm)") +
    geom_point(aes(size = avg_nitrogen)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,120)) +
    theme_classic()+
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave(whittaker_spectra, filename = "graphs/whittaker_spectra.png")

(precip_species_avg_n_plot <- ggplot (data = spectra_species_avg, aes (x = avg_precipitation, 
                                                                      y = avg_nitrogen, 
                                                                      col = plant_type)) +
  geom_point (size = 3) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration (%)") +
    xlab ("Annual Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,3.5)) +
  theme_classic ()+
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (precip_species_avg_n_plot, filename = "graphs/precip_species_avg_n_spectra_plot.png")


## extracting info for phenology conceptual diagram ----

above_date <- above_plots %>% 
  mutate (doy = yday(mdy(`7_Date`)))

avg_sampling_doy <- mean(above_date$doy)
# 210
avg_sampling_date <- as.Date (avg_sampling_doy)
# July 29th 

earliest_snowmelt <- as.Date(min (spectra_snowmelt_n$snowmelt_date))
# 80, aka March 22

latest_snowmelt <- as.Date(max (spectra_snowmelt_n$snowmelt_date))
# 181, aka July 1st 

