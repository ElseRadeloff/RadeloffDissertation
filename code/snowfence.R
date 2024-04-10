# Snowfence data wrangling, modelling and graphing
# Else Radeloff 
# 26 Jan 2024

# Create combined snowfence data table 

library (tidyverse)
library (readr)
library (lubridate)
library (lme4)
library (brms)  # for bayesian models 
library (car)  # for aic i think (?)
library (stargazer)  # for results tables 
library (ggeffects)
library (AICcmodavg)
library (tidybayes)


# Reading in data ----

# Snow fence data 
toolik_raw <- read.csv('data/toolik.csv')
greenland_raw <- read_csv('data/greenland.csv')
svalbard_raw <- read_csv("data/svalbard.csv")

# Cleaning toolik data and creating new 'toolik' object for graphing
toolik <- toolik_raw %>% 
  # delete unnecessary columns 
  select ( -c (ID..Full, Date.Sampled, X.C)) %>% 
  # rename columns
  rename (doy = Julian.Date, plot = Plot, treat = Snow.Zone, n= X.N, plant_type = Functional.Group, year = Year.Sampled, species = Species) %>% 
  # standardizing plant type names 
  mutate (plant_type =ifelse(plant_type == "Deciduous Shrub", "Deciduous shrub", plant_type)) %>% 
  # deal with implicit nesting of plots within treatments 
  mutate (plot = ifelse(treat == "snow", paste0(plot, "_s"), plot)) %>%  # mark snow plots with a '_s'
  mutate (plot = ifelse(treat == "ambient", paste0(plot, "_a"), plot)) %>%   # mark ambient plots with a '_a'
  filter (treat != "Low")

# Further cleaning to match greenland data 
toolik_cleaned <- toolik %>% 
  # delete unnecessary columns 
  select ( -c (plant_type)) %>% 
  # reorder columns 
  relocate (plot, .after = doy) %>% 
  # add location column that says toolik 
  add_column (location = 'toolik') 


# Creating a toolik dataframe with just salix 
toolik_salix <- toolik_raw %>% 
  # filtering to just salix rows 
  filter (Species == "Salix") %>% 
  # delete unnecessary columns 
  select ( -c (ID..Full, Date.Sampled, Functional.Group, Species, Year.Sampled, X.C)) %>%  
  # rename columns
  rename (doy = Julian.Date, plot = Plot, treat = Snow.Zone, n= X.N) %>% 
  # Remove -snow treatment
  filter ( ! treat == "Low") %>% 
  # Rename snow treatments to match greenland data
  mutate(treat = ifelse(treat == "Intermediate", "snow", treat)) %>% 
  mutate(treat = ifelse(treat == "Ambient", "ambient", treat)) %>% 
  # add location column that says toolik 
  add_column (location = 'toolik')

# Cleaning greenland data - just adding location so they can be combined w/ toolik
greenland <- greenland_raw %>% 
  add_column (location = 'greenland')

# Cleaning svalbard data 
svalbard <- svalbard_raw %>% 
  # delete unnecessary columns 
  select ( c(Date, Fence, Regime, N)) %>% 
  # rename columns 
  rename (doy = Date, plot = Fence, treat = Regime, n= N) %>% 
  # convert dates to doy 
  mutate (doy = yday(doy)) %>% 
  # add location column 
  add_column (location = 'svalbard') 

# Combining greenland and toolik but only salix data
snowfence_salix <- rbind(greenland, toolik_salix, svalbard)

snowfence_salix <- snowfence_salix %>% 
  # remove the 'low' snow treatment from toolik for comparability
  filter ( ! treat == "Low") %>% 
  # rename snow treatments for consistency 
  mutate(treat = ifelse(treat == "Intermediate", "snow", treat)) %>% 
  mutate(treat = ifelse(treat == "Ambient", "ambient", treat)) %>% 
  mutate (treat = ifelse(treat == "Normal", "ambient", treat)) %>% 
  mutate (treat = ifelse(treat == "Deep", "snow", treat)) %>% 
  # need to deal with implicit nesting in the plot column
  mutate (plot = ifelse(location == "greenland", paste0(plot, "g"), plot)) %>% # mark greenland plots with a "g"
  mutate (plot = ifelse(location == "svalbard", paste0(plot, "s"), plot)) %>%  # mark svalbard plots with a "s"
  mutate (plot = ifelse(location == "toolik", paste0(plot, "t"), plot)) %>%  # mark toolik plots with a "t"
  mutate (plot = ifelse(treat == "snow", paste0(plot, "_s"), plot)) %>%  # mark snow plots with a '_s'
  mutate (plot = ifelse(treat == "ambient", paste0(plot, "_a"), plot)) %>%   # mark ambient plots with a '_a'
# so a plot that is '4g_s' is a plot 4 at greenland in snow treatment
# and plot '1s_a' is plot 1 at svalbard in an ambient snow treatment 
# although the svalbard plots already had a more unique naming system
  # adding a categorical 'season' to replace doy as a random effect 
  mutate (season = ifelse (doy > 222 , "late", NA)) %>% 
  mutate (season = ifelse (doy <= 222 , "mid", season)) %>% 
  mutate (season = ifelse (doy <= 200 , "early", season))

#table (snowfence_salix$season)  # yay roughly equal numbers in each season




#### exploratory plots ----

# Differences in N between ambient and +snow treatments 
(treatment_boxplot <- ggplot (snowfence_salix, aes (x = treat, y = n)) +
   geom_boxplot( fill = "pink") +
   theme_classic()+
   xlab ("Snow Treatment") +
   ylab ("Nitrogen Percentage")
)
ggsave(treatment_boxplot, filename = "graphs/treatment_boxplot.png")

# Differences in N between snow treatments, separated by location (Toolik v Greenland)
(loc_boxplot <- ggplot (snowfence_salix, aes (x = treat, y = n, fill = location)) +
    geom_boxplot () +
    theme_classic())

ggsave(loc_boxplot, filename = "graphs/loc_boxplot.png")

# Scatter plot of N across the season, separated in color by treatment 
(scatter <- ggplot (snowfence_salix, aes ( x = doy, y = n, col = treat))+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE, aes(group = treat)) +
    theme_classic()
)

ggsave(scatter, filename = "graphs/scatter.png")

# plotting just the upper quantile of nitrogen levels 
upper_quantile_data <- snowfence_salix %>%
  filter(n > 0.75)  # 0.75 = quantile --> will plot top quarter of n levels 

(scatter_upper_quantile <- ggplot (upper_quantile_data, aes ( x = doy, y = n, col = treat))+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE, aes(group = treat)) +
    theme_classic()
)
# kind of looks the same 

(scatter_curvy <- ggplot (snowfence_salix, aes ( x = doy, y = n, col = treat))+
    geom_point()+
    geom_smooth(method = "loess", se = FALSE, aes(group = treat)) +
    theme_classic()
)

ggsave(scatter_curvy, filename = "graphs/scatter_curvy.png")

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


########################################################

### Models ####

# nitrogen versus snow (H1)
mod_snow <- lm (n ~ treat, data = snowfence)
summary(mod_snow)
plot (mod_snow)

# nitrogen versus snow for willows (H1)
mod_snow_salix <- lm (n ~ treat, data = snowfence_salix)
summary (mod_snow_salix)
plot(mod_snow_salix)

# nitrogen versus snow and site (H5)
snow_site <- lm (n ~ treat + location, data = snowfence)
summary (snow_site)
plot(snow_site)

# nitrogen versus snow and site with interaction (H5)
snow_site2 <- lm (n ~ treat * location, data = snowfence)
summary (snow_site2)
plot (snow_site2)

# nitrogen versus snow and plant functional type (H2)
snow_plant <- lm (n ~ treat + plant_type, data = toolik)
summary (snow_plant)
plot (snow_plant)

# nitrogen versus snow and plant functional type with interaction (H3)
snow_plant2 <- lm (n~ treat * plant_type, data = toolik)
summary (snow_plant2)
plot (snow_plant2)

# nitrogen versus snow and date (H4) (across the growing season)
snow_date <- lm (n ~ treat +doy, data = snowfence)
summary (snow_date)
plot (snow_date)

# nitrogen versus snow and date with interaction (H4)
snow_date2 <- lm (n ~ treat * doy, data = snowfence)
summary (snow_date2)
plot (snow_date2)

# nitrogen versus snow and date interacting but just willows 
snow_date_salix <- lm (n ~ treat * doy, data = snowfence_salix)
summary (snow_date_salix)
plot (snow_date_salix)


snow_date_plant <- lm (n ~ treat + doy + plant_type, data = toolik)
summary (snow_date_plant)
plot (snow_date_plant)


snow_date_plant2 <- lm (n ~ treat + doy * plant_type, data = toolik)
summary (snow_date_plant2)
plot (snow_date_plant2)


snow_date_plant3 <- lm (n ~ treat * doy + plant_type, data = toolik)
summary (snow_date_plant3)
plot (snow_date_plant3)

snow_date_plant4 <- lm (n ~ treat * doy * plant_type, data = toolik)
summary(snow_date_plant4)
plot(snow_date_plant4)

AIC (snow_date_plant, snow_date_plant2, snow_date_plant3, snow_date_plant4)

n_doy_mod <- lm (n~doy, data = snowfence_salix)
summary (n_doy_mod)
# intercept = 7.2
# slope - -0.21

#### Models take 2 ----

## HYPOTHESIS 1 ----

## Leaf concentrations will be higher where snow is deeper across species and sites

# mixed effects model 
H1 <- lmer (n ~ treat + (1|doy) + (1|location/plot), data = snowfence_salix)
# boundary (singular) fit: see help('isSingular') when I included (n|location/plot)
# multi-collinearity problem ?
# making it random intercepts instead of slopes fixed it 

# tried taking out doy as per Isaac's advice but got boundary fit singular 

H1_reduced <- lmer (n ~ 1 + (1|doy) + (1|location/plot), data = snowfence_salix)

summary (H1)
plot (H1)  # nice cloud of points 

anova(H1)
anova(H1, H1_reduced)  # they are significantly different 
                       # --> treatment has a significant effect ? 

stargazer(H1, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# results table as a html file 
stargazer(H1, type="html",
          out="results/H1.htm")

H1_residuals <- resid(H1) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H1_residuals) # p-value = 0.7272 --> pass --> residuals = normal

qqnorm(H1_residuals)
qqline (H1_residuals)

### Other model structures 

H1_location_plot <- lmer (n ~ treat + (1|location/plot), data = snowfence_salix)
# boundary fit singular even though it's simpler ??

H1_doy_plot <- lmer (n ~ treat + (1|doy) + (1|plot), data = snowfence_salix)

H1_plot <- lmer (n ~ treat +  (1|plot), data = snowfence_salix)

H1_location <- lmer (n ~ treat +  (1|location), data = snowfence_salix)

#H1_plot_slope <- lmer (n ~ treat +  (n|plot), data = snowfence_salix)
# error in eval_f : downdated Vtv is not positive definite

H1_no_random <- lm (n ~ treat, data = snowfence_salix)

H1_fixed_doy <- lmer (n ~ treat + doy + (1|location/plot), data = snowfence_salix)

H1_season_location_plot <- lmer (n ~ treat + (1|season) + (1|location/plot), data = snowfence_salix)
# boundary fit singular 

H1_season_plot <- lmer (n ~ treat + (1|season) + (1|plot), data = snowfence_salix)

AIC(H1, 
    H1_location,  
    H1_doy_plot, 
    H1_location_plot, 
    H1_plot, 
    H1_no_random, 
    H1_fixed_doy,
    H1_season_location_plot,
    H1_season_plot)

plot (H1_fixed_doy)  # looks good 
summary (H1_fixed_doy)
anova(H1_fixed_doy)

# same original model but bayesian - not good lots of warnings 
H1_bayes <- brm(n ~ treat + (1|doy) + (1|location/plot), 
                data = snowfence_salix,
                chains = 3,
                iter = 3000, warmup = 1000)
pairs (H1_bayes)  # accidental art ?

## HYPOTHESIS 2 ----
## Leaf N concentrations vary by plant functional type 

# linear mixed effects model 
H2 <- lmer(n ~ plant_type + (1|treat) + (1|doy), data = toolik)

H2_treat <- lmer (n~ plant_type + (1|treat), data = toolik)

H2_no_random <- lm (n ~ plant_type, data = toolik)

H2_plot <- lmer (n ~ plant_type + (1|plot), data = toolik)
# not converging, singular fit boundary 
# when I include plot as a random effect 
# or include year/doy 

H2_fixed_doy <- lmer(n ~ plant_type + doy + (1|treat), data = toolik)

H2_season <- lmer(n ~ plant_type + (1|treat) + (1|season), data = toolik)  

# diagnostics
plot (H2)  # v shaped
plot (H2_treat) # less variance on the left than on the right 
plot (H2_no_random)# qq plot is a bit funky but everything else looks okay 
plot (H2_fixed_doy)  # still slight v/u - greater variance on the right 
plot (H2_season) # still a u with more variance on the right 

# results 
summary (H2)
anova (H2)

summary (H2_treat)
anova (H2_treat)

summary (H2_no_random) # p values all really really low --> highly significant 
anova (H2_no_random)

summary (H2_fixed_doy)
anova (H2_fixed_doy)

summary (H2_season)
anova (H2_season)


stargazer(H2, type="html",
          out="results/H2.htm")

H2_residuals <- resid(H2) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H2_residuals) # p-value = 2.79 e-05 --> does NOT pass
hist (H2_residuals)

qqnorm(H2_residuals)
qqline (H2_residuals) # qq plot looks fine ? 

AIC (H1, H2_treat, H2_no_random, H2_fixed_doy, H2_season)

# try bayesian why not
H2_bayes <- brm(n ~ plant_type + (1|treat) + (1|plot), 
                data = toolik,
                chains = 3,
                iter = 3000, warmup = 1000)
# 150 divergent transitions ....
# removed doy and still 98 divergent transitions 

pairs (H2_bayes)
plot (H2_bayes)

H2_bayes_plot <- brm(n ~ plant_type + (1|plot), 
                     data = toolik,
                     chains = 3,
                     iter = 3000, warmup = 1000)

## HYPOTHESIS 3: Graminoids + deciduous shrubs will have a greater response to deeper snow  

H3 <- lmer (n~treat*plant_type + (1|doy) , data =toolik)
# had to get rid of plot as a random effect to make it run
# also made doy a random intercept instead of random slope 

H3.2 <- lm (n~treat*plant_type , data =toolik)

# diagnostics 
plot (H3)  # v shaped trend
plot (H3.2)  # qq plot looks funky 


H3_residuals <- resid(H3) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H3_residuals) # p-value = 0.0003 --> does NOT pass
hist (H3_residuals) # honestly looks great, idk what's going on

H3.2_residuals <- resid (H3.2)
shapiro.test (H3.2_residuals)  # p value = 3.563e-16 --> passes 

# results 
summary (H3)
anova (H3)

summary (H3.2)

# AHH bayesian 
H3_bayes <- brm(n~treat*plant_type + (1|doy), 
                data = toolik,
                chains = 3,
                iter = 3000, warmup = 1000)
# no divergent transitions so let's complexify

H3_bayes2 <- brm(n~treat*plant_type + (1|year/doy), 
                 data = toolik,
                 chains = 3,
                 iter = 3000, warmup = 1000)
# 770 divergent transitions and lots of other warnings 

H3_bayes3 <- brm(n~treat*plant_type + (1|doy) + (1|plot), 
                 data = toolik,
                 chains = 3,
                 iter = 3000, warmup = 1000)
# no divergent transitions ?!
plot (H3_bayes3)  # fuzzy caterpillars!!! Except for sd_plot_Intercept which is at the bottom and distribution is left-skewed/right tail
pairs (H3_bayes3)  # too busy can't read anything 

H3_bayes4 <- brm(n~treat*plant_type + (1|doy) + (1|plot) + (1|species), 
                 data = toolik,
                 chains = 3,
                 iter = 3000, warmup = 1000)
# 62 divergent transitions 
pairs (H3_bayes4)
plot(H3_bayes4)  # pretty fuzzy caterpillars 
# sd_plot_intercept and sd_doy intercept were crawling on the ground

## HYPOTHESIS 4: the difference in n concentrations between +snow and -snow will be higher at the start of the season 

H4 <- lmer (n~treat*doy +(1|location), data = snowfence_salix)
# having a random slope made boundary fit singular 
# also having location/plot made boundary fit singular

H4_plot <- lmer (n~treat * doy + (1|plot), data = snowfence_salix)

summary (H4)
plot (H4) # nice cloud of points ! 
anova (H4)

summary (H4_plot)
plot (H4_plot)  # some more variance on the right side 
anova (H4_plot)

# results table as a html file 
stargazer(H4, type="html",
          out="results/H4.htm")

stargazer(H4_plot, type="html",
          out="results/H4_plot.htm")

H4_residuals <- resid(H4) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H4_residuals) # p-value = 0.05056 --> BARELY passes
hist (H4_residuals) # looks fine to me 

H4_plot_residuals <- resid(H4_plot) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H4_plot_residuals) # p-value = 0.001359 --> passes
hist (H4_plot_residuals)  # okay but not perfect 

H4_bayes <- brm(n~treat*doy +(1|location), 
                data = snowfence_salix,
                chains = 3,
                iter = 3000, warmup = 1000)
# nope
# 183 divergent transitions 
plot (H4_bayes)  # look okay-ish
# sd_location_intercept maybe a bit low for the caterpillar and a bit of a right tail 
pairs (H4_bayes)
# I think treatsnow:doy and treatsnow correlate 
summary (H4_bayes)



## HYPOTHESIS 5: Leaf n concentrations will respond more to snow in low arctic sites 

H5 <- lmer (n~treat *location + (1|doy) + (1|plot), data = snowfence_salix)
# random slopes made boundary fit singular 

summary (H5)
plot (H5) # nice cloud of points 

# results table as a html file 
stargazer(H5, type="html",
          out="results/H5.htm")

H5_residuals <- resid(H5) # extract residuals 
# Shapiro-wilk test 
shapiro.test(H5_residuals) # p-value = 0.3146 --> passes
hist (H5_residuals)  # a-okay 

H5_bayes <- brm (n~treat *location + (1|doy) + (1|plot),
                 data = snowfence_salix,
                 chains = 3,
                 iter = 3000, warmup = 1000)
# ran just fine 

summary (H5_bayes)
plot (H5_bayes) # yay i love caterpillars 
pairs (H5_bayes)
# snow:svalbard and snow seem correlated, as well as snow:toolik and snow
# also snow:svalbard and snow:toolik but that's less strong 

#H5_bayes2 <- brm (n~treat *location + (n|doy) + (1|plot),
#                data = snowfence_salix,
#               chains = 3,
#              iter = 3000, warmup = 1000)
# 4296 divergent transitions lol 

#### Models take 3 ----
# And hopefully final take 

# Location model ----

location_mod <- brm(n ~ treat * location + (1|plot) + (1|doy), data = snowfence_salix)

summary (location_mod)

plot (location_mod)
pairs (location_mod)
pp_check(location_mod)  # posterior predictive checks

random_effects_snowfence_location <- ranef(location_mod)
print(random_effects_snowfence_location)

# save model 
saveRDS(location_mod, "models/snowfence_location_mod.RDS")

location_mod <- readRDS ("models/snowfence_location_mod.RDS")


# split into separate locations to check understanding ----
  # one model for svalbard, one for toolik, one for greenland 

# greenland 
snowfence_greenland <- snowfence_salix %>% 
  filter (location == "greenland")

greenland_mod <- brm(n ~ treat + (1|plot) + (1|doy), data = snowfence_greenland)

summary (greenland_mod)

# svalbard 
snowfence_svalbard <- snowfence_salix %>% 
  filter (location == "svalbard")

svalbard_mod <- brm(n ~ treat + (1|plot) + (1|doy), data = snowfence_svalbard)

summary (svalbard_mod)

# toolik 
snowfence_toolik <- snowfence_salix %>% 
  filter (location == "toolik")

toolik_mod <- brm(n ~ treat + (1|plot) + (1|doy), data = snowfence_toolik)

summary (toolik_mod)

# Species model ----

species_mod <- brm(n ~ treat * plant_type + (1|plot) + (1|doy), data = toolik)

summary (species_mod)

plot (species_mod)  # caterpillar plots
pp_check(species_mod)  # posterior predictive checks

# save model 
saveRDS(species_mod, "models/snowfence_pft_mod.RDS")

species_mod <- readRDS ('models/snowfence_pft_mod.RDS')

# split into separate models for different species for understanding ----

# sedges 
toolik_sedge <- toolik %>% 
  filter (plant_type == "Sedge")

sedge_mod <- brm(n ~ treat + (1|plot) + (1|doy), data = toolik_sedge)
summary (sedge_mod)

# deviduous shrubs
toolik_shrub <- toolik %>% 
  filter (plant_type == "Deciduous shrub")

shrub_mod <- brm(n ~ treat + (1|plot) + (1|doy), data = toolik_shrub)
summary (shrub_mod)

# evergreen shrubs
toolik_evergreen <- toolik %>% 
  filter (plant_type == "Evergreen dwarf shrub")

evergreen_mod <- brm (n ~ treat + (1|plot) + (1|doy), data = toolik_evergreen)
summary (evergreen_mod)

# Graphing model reults ---- 

# location mod
location_mod <- readRDS("models/snowfence_location_mod.RDS")

# extract fixed effects
location_mod_fit <- as.data.frame(fixef(location_mod))
location_mod_fit2 <- location_mod_fit %>% rownames_to_column("Treatment")
location_mod_fit3 <- location_mod_fit2
# adding things together
# adding location effect to interactive snow treatments at locations 
location_mod_fit3$Estimate[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "treatsnow:locationsvalbard"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationsvalbard'] + location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard']

location_mod_fit3$Estimate[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "treatsnow:locationtoolik"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'locationtoolik'] + location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'treatsnow:locationtoolik']

# adding intercept to everything 
location_mod_fit3$Estimate <- location_mod_fit3$Estimate + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Estimate[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] /2

location_mod_fit3$Q2.5 <- location_mod_fit3$Q2.5 + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Q2.5[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Q2.5[location_mod_fit3$Treatment == 'Intercept'] - location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept']

location_mod_fit3$Q97.5 <- location_mod_fit3$Q97.5 + location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept'] 
location_mod_fit3$Q97.5[location_mod_fit3$Treatment == "Intercept"] <- location_mod_fit3$Q97.5[location_mod_fit3$Treatment == 'Intercept'] - location_mod_fit3$Estimate[location_mod_fit3$Treatment == 'Intercept']

# renaming treatments 
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'Intercept'] <- 'Greenland Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'locationsvalbard'] <- 'Svalbard Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'locationtoolik'] <- 'Toolik Field station Ambient Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow'] <- 'Greenland Deep Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow:locationsvalbard'] <- 'Svalbard Deep Snow'
location_mod_fit3$Treatment[location_mod_fit3$Treatment == 'treatsnow:locationtoolik'] <- 'Toolik Field Station Deep Snow'

location_mod_fit3 <- location_mod_fit3 %>% 
  mutate (Location  = case_when (
    Treatment == "Greenland Ambient Snow" ~ "Greenland",
    Treatment == "Greenland Deep Snow" ~ "Greenland",
    Treatment == "Svalbard Ambient Snow" ~ "Svalbard",
    Treatment == "Svalbard Deep Snow" ~ "Svalbard",
    Treatment == "Toolik Field station Ambient Snow" ~ "Toolik",
    Treatment == "Toolik Field Station Deep Snow" ~ "Toolik",
    TRUE ~ Treatment
  )) %>% 
  mutate (Snow  = case_when (
    Treatment == "Greenland Ambient Snow" ~ "Ambient",
    Treatment == "Greenland Deep Snow" ~ "Deep",
    Treatment == "Svalbard Ambient Snow" ~ "Ambient",
    Treatment == "Svalbard Deep Snow" ~ "Deep",
    Treatment == "Toolik Field station Ambient Snow" ~ "Ambient",
    Treatment == "Toolik Field Station Deep Snow" ~ "Deep",
    TRUE ~ Treatment
  ))

# plotting!!!!!!!!!!!!!!!!!!
(location_mod_plot <- ggplot (location_mod_fit3, aes (x = Location, y = Estimate, color = Treatment)) +
    geom_point (size = 7,  position = position_dodge(width = 0.5)) +
    geom_errorbar (aes(ymin = Q2.5, ymax = Q97.5), size = 1.5, width = 0.5,  position = 'dodge') +
    scale_color_manual(values = c("#E283AA", "#882255", "#A0CCFF", "#6699CC", "#CE7B75", "#661100")) +
    theme_classic()+
    ylab ("Leaf Nitrogen Concentration (%)") +
    theme(legend.title = element_blank(),
          axis.text = element_text (size = 18),
          axis.title=element_text(size=18,face="bold"),
          legend.text = element_text (size = 18),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave(location_mod_plot, filename = "graphs/snowfence_location_mod_plot.png")  


# species mod

species_mod <- readRDS ("models/snowfence_pft_mod.RDS")

species_mod_fit <- as.data.frame(fixef(species_mod))
species_mod_fit2 <- species_mod_fit %>% rownames_to_column("Treatment")
species_mod_fit3 <- species_mod_fit2 %>% 
  # add intercept to everything
  mutate (Estimate = case_when(
    #Treatment == "treatIntermediate" 
     Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Estimate + Estimate[Treatment == "Intercept"],
    TRUE ~ Estimate  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
    mutate (Estimate = case_when (
      Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
      ~ Estimate + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
      TRUE ~ Estimate
    )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Estimate = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Estimate + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Estimate )) %>% 
  ## same but for Q2.5 (lower bound)
  mutate (Q2.5 = case_when(
    Treatment == "treatIntermediate" 
    | Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Q2.5 + Estimate[Treatment == "Intercept"],
    TRUE ~ Q2.5  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
  mutate (Q2.5 = case_when (
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
    ~ Q2.5 + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q2.5
  )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Q2.5 = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Q2.5 + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q2.5 )) %>% 
  ## same but for Q97.5
  mutate (Q97.5 = case_when(
    Treatment == "treatIntermediate" 
    | Treatment == 'plant_typeEvergreendwarfshrub' 
    | Treatment == 'plant_typeSedge'
    ~ Q97.5 + Estimate[Treatment == "Intercept"],
    TRUE ~ Q97.5  
  )) %>% 
  # adding evergreen intercept and deciduous slope to interactive evergreen term
  mutate (Q97.5 = case_when (
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"
    ~ Q97.5 + Estimate[Treatment == "plant_typeEvergreendwarfshrub"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q97.5
  )) %>% 
  # adding sedge intercept and deciduous slope to interactive sedge term 
  mutate (Q97.5 = case_when (
    Treatment == "treatIntermediate:plant_typeSedge"
    ~ Q97.5 + Estimate[Treatment == "plant_typeSedge"] + Estimate[Treatment == "treatIntermediate"],
    TRUE ~ Q97.5 )) %>% 
  mutate (Estimate = case_when(
    Treatment == "treatIntermediate" 
    ~ Estimate + Estimate[Treatment == "Intercept"],
    TRUE ~ Estimate )) %>% 
  mutate (Treatment = case_when(
    Treatment == "Intercept" ~ "Deciduous shrub ambient snow",
    Treatment == "treatIntermediate" ~ "Deciduous shrub deep snow",
    Treatment == "plant_typeEvergreendwarfshrub" ~ "Evergreen shrub ambient snow",
    Treatment == "plant_typeSedge" ~ "Graminoid ambient snow",
    Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub" ~ "Evergreen shrub deep snow",
    Treatment == "treatIntermediate:plant_typeSedge" ~ "Graminoid deep snow",
    TRUE ~ Treatment)) %>% 
  mutate (plant_type = case_when(
    Treatment == "Deciduous shrub ambient snow" ~ "Deciduous shrub",
    Treatment == "Deciduous shrub deep snow" ~ "Deciduous shrub",
    Treatment == "Evergreen shrub ambient snow" ~ "Evergreen shrub",
    Treatment == "Evergreen shrub deep snow" ~ "Evergreen shrub",
    Treatment == "Graminoid ambient snow" ~ "Graminoid",
    Treatment == "Graminoid deep snow" ~ "Graminoid",
    TRUE ~ Treatment)) %>% 
  mutate (Snow = case_when(
    Treatment == "Deciduous shrub ambient snow" ~ "Ambient",
    Treatment == "Deciduous shrub deep snow" ~ "Deep",
    Treatment == "Evergreen shrub ambient snow" ~ "Ambient",
    Treatment == "Evergreen shrub deep snow" ~ "Deep",
    Treatment == "Graminoid ambient snow" ~ "Ambient",
    Treatment == "Graminoid deep snow" ~ "Deep",
    TRUE ~ Treatment))

#####
# adding the interactive terms 
species_mod_fit3$Estimate[species_mod_fit3$Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeEvergreendwarfshrub"] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeEvergreendwarfshrub'] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']
species_mod_fit3$Q2.5[species_mod_fit3$Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeEvergreendwarfshrub"] + species_mod_fit3$Q2.5[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeEvergreendwarfshrub'] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']
species_mod_fit3$Q97.5[species_mod_fit3$Treatment == "treatIntermediate:plant_typeEvergreendwarfshrub"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeEvergreendwarfshrub"] + species_mod_fit3$Q97.5[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeEvergreendwarfshrub'] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']

species_mod_fit3$Estimate[species_mod_fit3$Treatment == "treatIntermediate:plant_typeSedge"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeSedge"] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeSedge'] + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']
species_mod_fit3$Q2.5[species_mod_fit3$Treatment == "treatIntermediate:plant_typeSedge"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeSedge"] + species_mod_fit3$Q2.5[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeSedge']+ species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']
species_mod_fit3$Q97.5[species_mod_fit3$Treatment == "treatIntermediate:plant_typeSedge"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == "plant_typeSedge"] + species_mod_fit3$Q97.5[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeSedge']+ species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'treatsnow']

# adding intercept to everything 
species_mod_fit3$Estimate <- species_mod_fit3$Estimate + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept'] 
species_mod_fit3$Estimate[species_mod_fit3$Treatment == "Intercept"] <- species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept'] /2

species_mod_fit3$Q2.5 <- species_mod_fit3$Q2.5 + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept'] 
species_mod_fit3$Q2.5[species_mod_fit3$Treatment == "Intercept"] <- species_mod_fit3$Q2.5[species_mod_fit3$Treatment == 'Intercept'] - species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept']

species_mod_fit3$Q97.5 <- species_mod_fit3$Q97.5 + species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept'] 
species_mod_fit3$Q97.5[species_mod_fit3$Treatment == "Intercept"] <- species_mod_fit3$Q97.5[species_mod_fit3$Treatment == 'Intercept'] - species_mod_fit3$Estimate[species_mod_fit3$Treatment == 'Intercept'] 
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'Intercept'] <- 'Deciduous shrubs ambient snow'
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'treatIntermediate'] <- 'Deciduous shrubs deep snow'
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'plant_typeEvergreendwarfshrub'] <- 'Evergreen shrubs ambient snow'
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'plant_typeSedge'] <- 'Sedges ambient snow'
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeEvergreendwarfshrub'] <- 'Evergreen shrubs deep snow'
species_mod_fit3$Treatment[species_mod_fit3$Treatment == 'treatIntermediate:plant_typeSedge'] <- 'Sedges deep snow'

#####
(species_mod_plot <- ggplot (species_mod_fit3, aes (x = plant_type, y = Estimate, color = Treatment)) +
    geom_point (size = 7,  position = position_dodge(width = 0.5)) +
    geom_errorbar (aes(ymin = Q2.5, ymax = Q97.5), size = 1.5, width = 0.5,  position = 'dodge') +
   scale_color_manual(values = c("#72C182", "#117733", "#928BD9", "#332288", "#F292E0", "#AA4499")) +
   xlab ("Plant Functional Type") + 
   ylab ("Leaf Nitrogen Concentration (%)") +
    theme_classic()) +
  theme(legend.title = element_blank(),
        legend.position = "right",  # Move legend to the right
        axis.text = element_text (size = 18),
        axis.title = element_text(size=18,face="bold"),
        legend.text = element_text (size = 18),
        axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
        axis.title.y = element_text(margin = margin(r = 20)))

ggsave(species_mod_plot, filename = "graphs/snowfence_species_mod_plot.png")  


# plotting again but trying to spread the data first ----

species_mod <- readRDS ('models/snowfence_pft_mod.RDS')
summary (species_mod)

#species_mod <- brm(n ~ treat * plant_type + (1|plot) + (1|doy), data = toolik)

species_mod_fit <- as.data.frame(fixef(species_mod))

snowfence_pft_mod_data <- expand_grid (treat = levels (as.factor(species_mod_fit$treat)),
                                         n = seq(0, 5, by = 0.5),
                                         plant_type = levels (as.factor(species_mod_fit$plant_type)))

snowfence_pft_mod_pred <- species_mod %>% 
  epred_draws(newdata = snowfence_pft_mod_data, allow_new_levels = TRUE)

(spectra_snowmelt_mod_fit <- ggplot() +
    geom_point(data = toolik, aes(x = treat, y = n, color = ordered (plant_type), fill = ordered (plant_type))) +   # raw data
    #geom_errorbar (data = snowfence_pft_mod_pred, aes(y = .epred, x = treat, color = ordered (plant_type), fill = ordered (plant_type)) ) +
    #stat_lineribbon(data = spectra_snowmelt_mod_pred, aes(y = .epred, x = snowmelt_date, color = ordered (plant_type), fill = ordered (plant_type)), .width = c(.95), # regression line and CI
    #                alpha = 0.25) +
    #geom_point(data = spectra_snowmelt_n, size = 3) +   # raw data
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Percentage Nitrogen (%)") +  
    xlab("Treatment") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))


# looking at day of year graph ----

snowfence_doy_mod_data <- expand_grid (doy = seq (150, 260, by = 7),
                                       location = levels (as.factor(snowfence_salix$location)),
                                       treat = levels (as.factor(snowfence_salix$treat)),
                                       n = seq(0, 5, by = 0.5))

snowfence_doy_mod_pred <- location_mod %>% 
  epred_draws(newdata = snowfence_doy_mod_data, allow_new_levels = TRUE)

(spectra_snowmelt_mod_fit <- ggplot() +
    geom_point(data = snowfence_salix, aes(x = doy, y = n, color = ordered (treat), fill = ordered (treat))) +   # raw data
    stat_lineribbon(data = snowfence_doy_mod_pred, aes(y = .epred, x = doy, color = ordered (treat), fill = ordered (treat)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    ylab("Percentage Nitrogen (%)") +  
    xlab("Day of year") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))


