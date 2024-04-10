# Else Radeloff 
# Feb 9th 2024

# Looking at leaf n vs. precipitation (and eventually snowmelt date)
# data from TTT and TRY databases, and Madelaine Anderson 
# Precipitation data from WorldClim 

# Loading packages and data ---- 

library (terra)
library (tidyverse)
library (readxl)
library (geodata)
library (readr)
library (lme4)
library (stargazer)
library(nortest)
library (brms)
library (tidybayes)
library (lubridate)

# load TRY and TTT data

load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/try_for_else (2).RData")

#load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/ttt_for_else (1).RData")

TTT_cleaned_dataset_v1 <- read_csv("data/TTT_cleaned_dataset_v1.csv")

#  load Madi's data
madi_raw <- read_csv("data/madi.csv")

# load functional trait groupings 

func_groups <- read.csv ("data/func_groups.csv")

# load WorldClim data with the geodata library 

#precip_year <- worldclim_global (var = "prec", res = 10, path = 'data')

precip_year_1km <- worldclim_global (var = "prec", res = 0.5, path = 'data')

months <- c(10,11,12,1,2,3,4)

precip_winter <- precip_year[[months]]

precip_winter_1km <- precip_year_1km[[months]]

precip <- mean(precip_winter)
precip_1km <- mean(precip_winter_1km)

# load arctic zones map (high arctic, low arctic, sub arctic)
high_low_arc <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")


# load landcover data (trees) with geodata lib 
trees <- landcover (var = "trees", path = 'C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data')

# snowfence site locations (for mapping)
snowfence_site <- read_xlsx ("data/snowfence_sites.xlsx")

#Define projections
WGSCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


### Data Wrangling ----
 
# coordinates from google maps 
# 60°58'48.4"N 138°24'07.1"W - Kluane
# lat = 60.98, lon = 128.4019 Kluane 
# 69°34'45.9"N 138°53'38.2"W - Qikiqtaruk 
# lat = 69.5791, lon = 138.8939 Qikiqtaruk 


madi <- madi_raw %>% 
  # adding lat and lon by site 
  # not sure these points are correct - need to check that I converted right 
  mutate (lat = case_when (
    grepl ("Kluane Plateau", site) ~ 60.98,
    grepl ("Qikiqtaruk", site) ~ 69.5971)) %>% 
  mutate (lon = case_when (
    grepl ("Kluane Plateau", site) ~ -138.5971,
    grepl ("Qikiqtaruk", site) ~ -138.8939)) %>% 
  # adding deciduous / evergreen info to shrub data 
  mutate (functional_group = case_when(
    grepl ("Cassiope", latin.genus) ~ "evergreen shrub",
    grepl ("Salix", latin.genus) ~ "deciduous shrub",
    grepl ("Betula", latin.genus) ~ "deciduous shrub",
    grepl ("Dryas integrifolia Vahl", species) ~ "evergreen shrub",
    TRUE ~ functional_group)) %>% 
  # change forbs to herbs to match spreadsheet
   mutate (functional_group = ifelse (functional_group == "forb", "herb", functional_group)) %>% 
  # removing columns to match the ttt and try data 
  select (-c(latin.species, measurement.date, site, percent_C, ...1)) %>% 
  # renaming nitrogen column
  rename (n = percent_N) %>% 
  rename (Genus = latin.genus) %>% 
  # changing the units so they match ttt and try 
  mutate (n = n *10) %>% 
  filter (functional_group != "herb") %>% 
  # adding a column for data source 
  mutate (data_source = "Madi")
  
  
# merge sheets so that each observation has a plant funcitonal type associated with it 
ttt_n <- TTT_cleaned_dataset_v1 %>% 
  # select rows with nitrogen observations 
  filter (Trait == "Leaf nitrogen (N) content per leaf dry mass") %>% 
  # select only 'control' plots
  filter(Treatment != "warmed" 
         & Treatment != "N fertilized"
         & Treatment != "greenhouse"
         & Treatment != "N + warmed") %>% 
  # rename species column to match spreadsheet w/ pfts 
  rename (Species = AccSpeciesName) %>% 
  # join ttt data with pft spreadsheet to assign a functional group to each observation 
  left_join (func_groups, by = "Species") %>% 
  # remove trees 
  filter (PlantGrowthForm != "tree") %>% 
  # add deciduou/ evergreen info
  mutate (PlantGrowthForm = ifelse (PlantGrowthForm == "shrub", paste(LeafPhenology, PlantGrowthForm), PlantGrowthForm)) %>% 
  # add a column for data source 
  mutate (data_source = "Tundra Trait Team") %>% 
  # assign plant funcitonal group to species not specified in spreadsheet 
  mutate (PlantGrowthForm = case_when(
    grepl ("Lysimachia europaea", Species) ~ "herb",
    grepl ("Poa sp.", Species) ~ "graminoid",
    grepl ("Betula divaricata", Species) ~ "deciduous shrub",
    grepl ("Alchemilla sp", Species) ~ "herb",
    grepl ("Lactuca alpina", Species) ~ "herb",
    grepl ("Eutrema edwardsii", Species) ~ "herb",
    grepl ("Erigeron humilis", Species) ~ "herb",
    grepl ("Minuartia rossii", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Koenigia hadacii", Species) ~ "herb",
    grepl ("Saxifraga flagellaris", Species) ~ "herb",
    grepl ("Taraxacum brachyceras", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Cochlearia groenlandica", Species) ~ "herb",
    grepl ("Festuca sp.", Species) ~ "graminoid",
    grepl ("Potentilla rubricaulis", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Dupontia sp.", Species) ~ "graminoid",
    grepl ("Pedicularis dasyantha", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene uralensis", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Draba sp.", Species) ~ "herb",
    grepl ("Draba hirta", Species) ~ "herb",
    grepl ("Saxifraga hieraciifolia", Species) ~ "herb",
    grepl ("Saxifraga tenuis", Species) ~ "herb",
    grepl ("Carex marina", Species) ~ "graminoid",
    grepl ("Campanula gieseckeana", Species) ~ "herb",
    grepl ("Saxifraga foliolosa", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Carex sp.", Species) ~ "graminoid",
    grepl ("Salix sp.", Species) ~ "shrub",
    grepl ("Valeriana capitata", Species) ~ "herb",
    grepl ("Sedum roseum", Species) ~ "herb",
    grepl ("Taraxacum sp.", Species) ~ "herb",
    grepl ("Harrimanella hypnoides", Species) ~ "herb",
    grepl ("Persicaria vivipara", Species) ~ "herb",
    grepl ("Ranunculus pygmaeus", Species) ~ "herb",
    grepl ("Papaver radicatum", Species) ~ "herb",
    grepl ("Potentilla pulchella", Species) ~ "herb",
    grepl ("Polemonium boreale", Species) ~ "herb",
    grepl ("Chrysosplenium tetrandrum", Species) ~ "herb",
    grepl ("Ranunculus spitsbergensis", Species) ~ "herb",
    grepl ("Taraxacum arcticum", Species) ~ "herb",
    grepl ("Antennaria alpina", Species) ~ "herb",
    grepl ("Rubus chamaemorous", Species) ~ "herb",
    grepl ("Andromeda polifolia", Species) ~ "deciduous shrub",
    grepl ("Linnaea borealis", Species) ~ "herb",
    grepl ("Artemisia ordosica", Species) ~ "shrub",
    grepl ("Artemisia wellbyi", Species) ~ "shrub",
    grepl ("Lespedeza davurica", Species) ~ "herb",
    grepl ("Orthilia secunda", Species) ~ "herb",
    grepl ("Rubus saxatilis", Species) ~ "deciduous shrub",
    grepl ("Euphorbia clusiaefolia", Species) ~ "herb",
    grepl ("Symphyotrichum oolentangiense", Species) ~ "herb",
    grepl ("Helianthemum bicknellii", Species) ~ "herb",
    grepl ("Artemisia ludoviciana", Species) ~ "herb",
    grepl ("Helianthemum nummularium", Species) ~ "evergreen shrub",
    grepl ("Linum suffruticosum", Species) ~ "herb",
    grepl ("Rubus vestitus", Species) ~ "deciduous shrub",
    grepl ("Rubus ulmifolius", Species) ~ "deciduous srhub",
    grepl ("Solanum dulcamara", Species) ~ "herb",
    grepl ("Rubus pubescens", Species) ~ "herb",
    grepl ("Aralia nudicaulis", Species) ~ "herb",
    grepl ("Aralia racemosa", Species) ~ "herb",
    grepl ("Pedicularis canadensis", Species) ~ "herb",
    grepl ("Rubus caesius", Species) ~ "deciduous shrub",
    grepl ("Rubus canascens", Species) ~ "deciduous shrub",
    grepl ("Acinos alpinus", Species) ~ "herb",
    grepl ("Euphorbia characias", Species) ~ "herb",
    grepl ("Plomis fructicosa", Species) ~ "evergreen shrub",
    grepl ("Psoralea bituminosa", Species) ~ "herb",
    grepl ("Stackhousia brunonis", Species) ~ "herb",
    grepl ("Isotropis cuneifolia", Species) ~ "herb",
    grepl ("Mitchella repens", Species) ~ "herb",
    grepl ("Rubus idaeus", Species) ~ "deciduous shrub",
    grepl ("Andromeda glaucophylla", Species) ~ "evergreen shrub",
    grepl ("Rubus hawaiiensis", Species) ~ "herb",
    grepl ("Rubus plicatus", Species) ~ "deciduous shrub",
    grepl ("Rubus hispidus", Species) ~ "deciduous shrub",
    grepl ("Eremophila glabra", Species) ~ "deciduosus shrub",
    grepl ("Acamptopappus shockleyi", Species) ~ "herb",
    grepl ("Thymus praecox", Species) ~ "evergreen shrub",
    grepl ("Genista tinctoria", Species) ~ "deciduous shrub",
    grepl ("Rubus odoratus", Species) ~ "deciduous shrub",
    grepl ("Rubus allegheniensis", Species) ~ "deciduous shrub",
    grepl ("Petalostemum candidum", Species) ~ "herb",
    grepl ("Reynoutria japonica", Species) ~ "herb",
    grepl ("Rubus ursinus", Species) ~ "herb",
    grepl ("Lobelia yuccoides", Species) ~ "deciduous shrub",
    grepl ("Reynoutria sachalinensis", Species) ~ "herb",
    grepl ("Ageratina adenophora", Species) ~ "deciduous shrub",
    grepl ("Rubus arcticus", Species) ~ "deciduous shrub",
    grepl ("Betula nana", Species) ~ "deciduous shrub",
    grepl ("Betula fruticosa", Species) ~ "deciduous shrub",
    grepl ("Betula glandulosa", Species) ~ "deciduous shrub",
    grepl ("Betula pumila", Species) ~ "deciduous shrub",
    grepl ("Dryas octopetala", Species) ~ "evergreen shrub",
    grepl ("Rubus chamaemorus", Species) ~ "herb",
    grepl ("Salix glauca", Species) ~ "deciduous shrub",
    grepl ("Alnus incana", Species) ~ "deciduous shrub",
    grepl ("Sorbus aucuparia", Species) ~ "tree",
    grepl ("Juniperus communis", Species) ~ "evergreen shrub",
    grepl ("Salix pulchra", Species) ~ "deciduous shrub",
    grepl ("Celmisia brevifolia", Species) ~ "herb",
    grepl ("Celmisia sessiliflora", Species) ~ "herb",
    grepl ("Senecio kolenatianus", Species) ~ "herb",
    grepl ("Thymus nummularius", Species) ~ "herb",
    grepl ("Alchemilla fallax", Species) ~ "herb",
    grepl ("Hedysarum hedysaroides", Species) ~ "herb",
    grepl ("Podospermum canum", Species) ~ "herb",
    grepl ("Primula integrifolia", Species) ~ "herb",
    TRUE ~ PlantGrowthForm))

# merge sheets so that each observation has a plant functional type associated with it 
try_n <- mTRY2 %>% 
  # select only nitrogen measurements 
  filter (DataName == "Leaf nitrogen content per dry mass (Nmass)") %>%
  # rename species column so it matches plant functional group sheet
  rename (Species = AccSpeciesName) %>%   
  # merge sheets so that each observation has a plant functional type associated with it 
  left_join (func_groups, by = "Species") %>% 
  # add a column for data source 
  mutate (data_source = "TRY") %>% 
  # remove observations without a species 
  filter (Species != "Unidentified species") %>% 
  # remove trees 
  filter (PlantGrowthForm != "tree") %>% 
  # add deciduou/ evergreen info
  mutate (PlantGrowthForm = ifelse (PlantGrowthForm == "shrub", paste(LeafPhenology, PlantGrowthForm), PlantGrowthForm)) %>% 
  # assign plant growth forms for the species not in the spreadsheet already
  mutate (PlantGrowthForm = case_when(
    grepl ("Lysimachia europaea", Species) ~ "herb",
    grepl ("Poa sp.", Species) ~ "graminoid",
    grepl ("Poa", Species) ~ "graminoid",
    grepl ("Betula divaricata", Species) ~ "deciduous shrub",
    grepl ("Alchemilla sp", Species) ~ "herb",
    grepl ("Lactuca alpina", Species) ~ "herb",
    grepl ("Eutrema edwardsii", Species) ~ "herb",
    grepl ("Erigeron humilis", Species) ~ "herb",
    grepl ("Minuartia rossii", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Koenigia hadacii", Species) ~ "herb",
    grepl ("Saxifraga flagellaris", Species) ~ "herb",
    grepl ("Taraxacum brachyceras", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Cochlearia groenlandica", Species) ~ "herb",
    grepl ("Festuca sp.", Species) ~ "graminoid",
    grepl ("Potentilla rubricaulis", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Dupontia sp.", Species) ~ "graminoid",
    grepl ("Pedicularis dasyantha", Species) ~ "herb",
    grepl ("Ranunculus sulphureus", Species) ~ "herb",
    grepl ("Braya glabella", Species) ~ "herb",
    grepl ("Silene uralensis", Species) ~ "herb",
    grepl ("Silene involucrata", Species) ~ "herb",
    grepl ("Draba sp.", Species) ~ "herb",
    grepl ("Draba hirta", Species) ~ "herb",
    grepl ("Saxifraga hieraciifolia", Species) ~ "herb",
    grepl ("Saxifraga tenuis", Species) ~ "herb",
    grepl ("Carex marina", Species) ~ "graminoid",
    grepl ("Campanula gieseckeana", Species) ~ "herb",
    grepl ("Saxifraga foliolosa", Species) ~ "herb",
    grepl ("Arenaria pseudofrigida", Species) ~ "herb",
    grepl ("Puccinelia sp.", Species) ~ "graminoid",
    grepl ("Carex sp.", Species) ~ "graminoid",
    grepl ("Salix sp.", Species) ~ "deciduous shrub",
    grepl ("Valeriana capitata", Species) ~ "herb",
    grepl ("Sedum roseum", Species) ~ "herb",
    grepl ("Taraxacum sp.", Species) ~ "herb",
    grepl ("Harrimanella hypnoides", Species) ~ "herb",
    grepl ("Festuca ovina subvar. novae-zelandiae", Species) ~ "herb",
    grepl ("Agrostis", Species) ~ "graminoid",
    grepl ("Stipa", Species) ~ "graminoid",
    grepl ("Marram", Species) ~ "graminoid",
    grepl ("Agropyron", Species) ~ "graminoid",
    grepl ("Festuca", Species) ~ "graminoid",
    grepl ("Koeleria", Species) ~ "graminoid",
    grepl ("Hierochloe", Species) ~ "graminoid",
    grepl ("Bromus", Species) ~ "graminoid",
    grepl ("Elymus", Species) ~ "graminoid",
    grepl ("Panicum", Species) ~ "graminoid",
    grepl ("Bouteloua", Species) ~ "graminoid",
    grepl ("Eragrostis", Species) ~ "graminoid",
    grepl ("Betula", Species) ~ "deciduous shrub",
    grepl ("Ledum palustre subsp. groenlandicum", Species) ~ "evergreen shrub",
    grepl ("Empetrum nigrum subsp. hermaphroditum", Species) ~ "evergreen shrub",
    grepl ("Salix", Species) ~ "deciduous shrub",
    grepl ("VACCINIUM VITIS-IDAEA", Species) ~ "evergreen shrub",
    grepl ("Carex", Species) ~ "graminoid",
    grepl ("Picea", Species) ~ "tree",
    grepl ("Luzula", Species) ~ "graminoid",
    grepl ("Vaccinium", Species) ~ "deciduous shrub",
    grepl ("Betula pubescens var. pumila", Species) ~ "tree",
    grepl ("ARCTOSTAPHYLOS UVA-URSI", Species) ~ "evergreen shrub",
    grepl ("Kobresia", Species) ~ "graminoid",
    grepl ("Aster", Species) ~ "herb",
    grepl ("Triticum", Species) ~ "graminoid",
    grepl ("Larix x eurolepis", Species) ~ "tree",
    grepl ("Potentilla bifurca", Species) ~ "herb",
    grepl ("Potentilla fructicosa", Species) ~ "herb",
    grepl ("Saussurea", Species) ~ "herb",
    grepl ("Rubus", Species) ~ "deciduous shrub",
    grepl ("Papaver radicatum", Species) ~ "herb",
    grepl ("Oxytropis deflexa", Species) ~ "herb",
    grepl ("Crepis sibirica", Species) ~ "herb",
    grepl ("Paeonia anomala", Species) ~ "herb",
    grepl ("Artemisia norvegica", Species) ~ "herb",  
    grepl ("Stellaria bungeana", Species) ~ "herb",
    grepl ("Persicaria vivipara", Species) ~ "herb",
    grepl ("Astragalus umbellatus", Species) ~ "herb",
    grepl ("Dryas octopetala", Species) ~ "deciduous shrub",
   grepl ("Rubus chamaemorous", Species) ~ "herb",
   grepl ("Andromeda polifolia", Species) ~ "deciduous shrub",
   grepl ("Linnaea borealis", Species) ~ "herb",
   grepl ("Artemisia ordosica", Species) ~ "deciduous shrub",
   grepl ("Artemisia wellbyi", Species) ~ "deciduous shrub",
   grepl ("Lespedeza davurica", Species) ~ "herb",
   grepl ("Orthilia secunda", Species) ~ "herb",
   grepl ("Rubus saxatilis", Species) ~ "deciduous shrub",
   grepl ("Euphorbia clusiaefolia", Species) ~ "herb",
   grepl ("Symphyotrichum oolentangiense", Species) ~ "herb",
   grepl ("Helianthemum bicknellii", Species) ~ "herb",
   grepl ("Artemisia ludoviciana", Species) ~ "herb",
   grepl ("Helianthemum nummularium", Species) ~ "evergreen shrub",
   grepl ("Linum suffruticosum", Species) ~ "herb",
   grepl ("Rubus vestitus", Species) ~ "deciduous shrub",
   grepl ("Rubus ulmifolius", Species) ~ "deciduous srhub",
   grepl ("Solanum dulcamara", Species) ~ "herb",
   grepl ("Rubus pubescens", Species) ~ "herb",
   grepl ("Aralia nudicaulis", Species) ~ "herb",
   grepl ("Aralia racemosa", Species) ~ "herb",
   grepl ("Pedicularis canadensis", Species) ~ "herb",
   grepl ("Rubus caesius", Species) ~ "deciduous shrub",
   grepl ("Rubus canascens", Species) ~ "deciduous shrub",
   grepl ("Acinos alpinus", Species) ~ "herb",
   grepl ("Euphorbia characias", Species) ~ "herb",
   grepl ("Phlomis fruticosa", Species) ~ "evergreen shrub",
   grepl ("Psoralea bituminosa", Species) ~ "herb",
   grepl ("Stackhousia brunonis", Species) ~ "herb",
   grepl ("Isotropis cuneifolia", Species) ~ "herb",
   grepl ("Mitchella repens", Species) ~ "herb",
   grepl ("Rubus idaeus", Species) ~ "deciduous shrub",
   grepl ("Andromeda glaucophylla", Species) ~ "evergreen shrub",
   grepl ("Rubus hawaiiensis", Species) ~ "herb",
   grepl ("Rubus plicatus", Species) ~ "deciduous shrub",
   grepl ("Rubus hispidus", Species) ~ "deciduous shrub",
   grepl ("Eremophila glabra", Species) ~ "deciduosus shrub",
   grepl ("Acamptopappus shockleyi", Species) ~ "herb",
   grepl ("Thymus praecox", Species) ~ "evergreen shrub",
   grepl ("Genista tinctoria", Species) ~ "deciduous shrub",
   grepl ("Rubus odoratus", Species) ~ "deciduous shrub",
   grepl ("Juniperus communis", Species) ~ "evergreen shrub",
   grepl ("Rubus allegheniensis", Species) ~ "deciduous shrub",
   grepl ("Petalostemum candidum", Species) ~ "herb",
   grepl ("Reynoutria japonica", Species) ~ "herb",
   grepl ("Rubus ursinus", Species) ~ "herb",
   grepl ("Lobelia yuccoides", Species) ~ "deciduous shrub",
   grepl ("Reynoutria sachalinensis", Species) ~ "herb",
   grepl ("Ageratina adenophora", Species) ~ "deciduous shrub",
   grepl ("Rubus arcticus", Species) ~ "deciduous shrub",
   grepl ("Betula nana", Species) ~ "deciduous shrub",
   grepl ("Betula fruticosa", Species) ~ "deciduous shrub",
   grepl ("Betula glandulosa", Species) ~ "deciduous shrub",
   grepl ("Betula pumila", Species) ~ "deciduous shrub",
   grepl ("Salix glauca", Species) ~ "deciduous shrub",
   grepl ("Alnus incana", Species) ~ "deciduous shrub",
   grepl ("Sorbus aucuparia", Species) ~ "tree",
   grepl ("Juniperus comunis", Species) ~ "evergreen shrub",
   grepl ("Salix pulchra", Species) ~ "deciduous shrub",
   grepl ("Dryas octopetala", Species) ~ "evergreen shrub",
   grepl ("Rubus chamaemorus", Species) ~ "herb",
    TRUE ~ PlantGrowthForm)) 


ttt <- ttt_n %>% 
  # selecting columns to match Madi's data 
  select(Species, IndividualID, Value,PlantGrowthForm,  Latitude, Longitude, data_source, Genus) %>% 
 # renaming columns to match Madi's data 
   rename (lat = Latitude,
          lon = Longitude,
          functional_group = PlantGrowthForm,
          sample_id = IndividualID,
          n = Value,
          species = Species) %>% 
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  filter (!is.na(lat))

try <- try_n %>% 
  # select columns to match Madi's data 
  select (SpeciesName, ObsDataID, StdValue, PlantGrowthForm, Lat, Lon, data_source, Genus) %>% 
  # rename columns to match Madi's data 
  rename (lat = Lat,
          lon = Lon,
          functional_group = PlantGrowthForm,
          sample_id = ObsDataID,
          n = StdValue,
          species = SpeciesName) %>% 
  # remove observations of trees and lichen
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  # remove observations without coordinates associated 
  filter (!is.na(lat))

# combining ttt, try and Madi's data into one big sheet 
obs_n <- rbind (ttt, try, madi) %>% 
  rename (genus = Genus) %>% 
  filter (genus != "Agrostis"
          & genus != "Dactylis"
          & genus != "Melica"
          & genus != "Phalaris"
          & genus != "Trichophorum")

# exploring number of observations per group 
#test_obs <- obs_n %>% 
#  group_by (genus) %>% 
#  tally()

#test_obs

obs_n_sites <- obs_n %>% 
  group_by (lat, lon) %>% 
  summarise (avg_n = mean (n)) 
#str(obs_n_)

obs_high_n <- obs_n_sites %>% 
  filter (avg_n>30)

obs_med_n <- obs_n_sites %>% 
  filter (avg_n>20) %>% 
  filter (avg_n<30)

obs_low_n <- obs_n_sites %>% 
  filter (avg_n<20)

obs_sites <- obs_n %>% 
  group_by (lat, lon) %>% 
  mutate (ID = row_number()) %>% 
  ungroup ()

# make a vector layer of the coordinates  ----
n_points <- vect (obs_n, geom=c("lon", "lat"), crs=WGSCRS)
snowfence_points <- vect (snowfence_site, geom=c("lon", "lat"), crs = WGSCRS)

points_high_n <- vect (obs_high_n, geom=c("lon", "lat"), crs = WGSCRS)
points_med_n <- vect (obs_med_n, geom=c("lon", "lat"), crs = WGSCRS)
points_low_n <- vect (obs_low_n, geom=c("lon", "lat"), crs = WGSCRS)

### Extract precip data for data locations ----

# extract mean precipitation 
arctic_precip <- terra::extract(precip, n_points)
arctic_precip_1km <- terra::extract(precip_1km, n_points)


# add precipitation data to trait datasheet 
precip_trait <- obs_n %>% 
  mutate (ID = row_number()) %>% 
  left_join (arctic_precip, by = "ID") %>% 
  rename (mean_precip = mean) %>% 
  filter (!is.na(mean_precip))

precip_trait_1km <- obs_n %>% 
  mutate (ID = row_number()) %>% 
  left_join (arctic_precip_1km, by = "ID") %>% 
  rename (mean_precip = mean) %>% 
  filter (!is.na(mean_precip))


# Add arctic zone 

# load arctic zones map (high arctic, low arctic, sub arctic)
zones <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")
#crs (zones)



# changing projections to a top down view of the arctic to match zones map

#re-projecting raster into top down view of the Arctic 
precip_proj <- terra::project(precip, "EPSG:3408")
#crs (precip_proj)

# re-projecting coordinates to the top down view of arctic 
n_points_proj <- project (n_points, "EPSG:3408")
snowfence_points_proj <- project (snowfence_points, "EPSG:3408")

high_n_proj <- project (points_high_n, "EPSG:3408")
med_n_proj <- project (points_med_n, "EPSG:3408")
low_n_proj <- project (points_low_n, "EPSG:3408")

# extract zone 
n_points_zone <- terra::extract(zones, n_points_proj) %>% 
  select (id.y, Zone) %>% 
  rename (ID = id.y, zone = Zone)

# add zone data to trait datasheet 
n_precip_zones <- precip_trait %>%  
  left_join (n_points_zone, by = "ID") %>% 
  # removing points below sub arctic 
  #filter (!is.na(zone)) %>% 
  mutate (zone2 = (ifelse(zone == "High arctic", "Low arctic", zone))) #%>% 


# add zone data to trait datasheet 
n_precip_zones_1km <- precip_trait_1km %>%  
  left_join (n_points_zone, by = "ID") %>% 
  # removing points below sub arctic 
  #filter (!is.na(zone)) %>% 
  mutate (zone2 = (ifelse(zone == "High arctic", "Low arctic", zone))) %>% 
  mutate (leafn = n/10)


obs_points <- n_precip_zones %>% 
  group_by (lat, lon) %>% 
  tally ()

#write_xlsx(obs_points, "data/observation_snowmelt.xlsx")


#### Plots ----

# boxplot of N across plant functional groups 
(plant_type_boxplot <- ggplot (obs_n, aes (x = functional_group, y = n)) +
    geom_boxplot( fill = "light blue") +
    theme_classic()+
    xlab ("Plant Functional Group") +
    ylab ("Nitrogen")
)

ggsave(plant_type_boxplot, filename = "graphs/plant_type_boxplot.png")

# Map ----

# Limiting how high precipitation can go to highlight the changes in precip at the lower end
precip_scaled <- clamp (precip_proj, 0, 150)

# rectangle that defines the area we will crop to (xmin, xmax, ymin, ymax)
extent <- ext(-4000000, 4000000, -4000000, 4000000)
precip_crop <- crop(precip_scaled, extent)
pal <- colorRampPalette(c("lightblue", "darkblue"))

#precip_map <- plot (precip_scaled)
precip_cropped_map <- plot (precip_crop, col = pal(20), axes = FALSE)
# add lat lon lines (graticules)
precip_cropped_map <- lines(graticule(lon=seq(0, 360, by=20), lat=seq(0, 90, by=30), crs="EPSG:3408"), col="black")
precip_cropped_map <- lines(graticule(lon=20, lat=seq(0, 90, by=20), crs="EPSG:3408"), col="black")
# add obs data points
precip_cropped_map <- points (high_n_proj, cex = 2, col = "#CC6677")
precip_cropped_map <- points (med_n_proj, cex = 1.2, col = "#CC6677")
precip_cropped_map <- points (low_n_proj, cex = 0.7, col = "#CC6677")
#adding spectra points
precip_cropped_map <- points (spectra_high_n_proj, cex = 2, col = "goldenrod2")
precip_cropped_map <- points (spectra_med_n_proj, cex = 1.2, col = "goldenrod2")
precip_cropped_map <- points (spectra_low_n_proj, cex = 0.7, col = "goldenrod2")
# snowfence sites 
precip_cropped_map <- points (snowfence_points_proj, cex = 1.5, col = "#661100", bg = "#661100", pch = 23)

legend(x = 1800000, y = 4000000,
       legend = c("High Leaf N (>3%)", "Med Leaf N (2-3%)", "Low Leaf N (<2%)", "Observational Data", 
                  "Spectra Data", "Snowfence Points"),
       pch = c(21, 21, 21, 21, 21, 23),
       col = c("black", "black", "black", "#CC6677", "goldenrod2",  "#661100"),
       pt.bg = c("black", "black", "black", "#CC6677", "goldenrod2",  "#661100"),
       bg = "white",
       pt.cex = c(2, 1.2, 0.7, 2, 2, 1.5)
)


ggsave (precip_cropped_map, filename = "graphs/precip_map.png")


# Load your vector data
vector_data <- read.csv("path/to/your/vector/file.csv")

# Add vector points to the plot
map_plot <- map_plot +
  geom_point(data = vector_data, aes(x = lon, y = lat, color = variable_name), size = 2) +  # Replace "lon", "lat", and "variable_name" with appropriate column names from your vector data
  scale_color_manual(values = c("red", "blue", "green")) +  # Adjust point colors as needed
  labs(color = "Your Variable Name")  


# Scatter plot of mean precipitation vs. leaf nitrogen 
(precip_vs_n <- ggplot (precip_trait, aes(x=mean_precip, y = n, col = functional_group)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)+
    theme_classic() +
    xlab ("Mean Precipitation") +
    ylab ("Nitrogen Content")) 

ggsave(precip_vs_n, filename = "graphs/precip_vs_n.png")

# latitude versus nitrogen 
(lat_vs_n <- ggplot (precip_trait, aes(x=lat, y = n, col = functional_group)) +
    geom_point() +
    theme_classic() +
    geom_smooth (method = lm, se = FALSE) +
    xlab ("Latitude") +
    ylab ("Nitrogen Content"))
# no clear trend 

# does precipitation change with latitude? 
(lat_vs_precip <- ggplot (precip_trait, aes(x=lat, y = mean_precip, col = data_source)) +
    geom_point() +
    theme_classic() +
    geom_smooth (method = lm, se = FALSE) +
    xlab ("Latitude") +
    ylab ("Average Precipitation"))
# no clear trend yay


### Models ----

# PFT

#precip_pft_lm <- lmer (n ~ mean_precip * functional_group + (1|species) + (1|zone), data = n_precip_zones)
# boundary fit singular when I let functional group / species be a random slope 
# warning: fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

#plot(precip_pft_lm)
#summary (precip_pft_lm)

# bayesian version 


# with zone 
#precip_pft_zone_mod <- brm (n ~ mean_precip * functional_group + (1|zone2), data = n_precip_zones)
# deleting species for now because I haven't filtered those/ decided if I can do that in the spectra data

#summary (precip_pft_zone_mod)
#plot (precip_pft_zone_mod)
#pp_check (precip_pft_zone_mod)

# it says its unhappy and there were 247 divergent transitions so not ideal
# biggest rhat is 1.03
# ESS of intercept is 127 so a bit low...

# it was happier when I did the original zones (high, low, sub) instead of grouping high and low together 

#random_effects <- ranef(precip_pft_zone_mod)
#print(random_effects)

# Final model ----

precip_pft_mod <- brm (n ~ mean_precip * functional_group + (1|genus), data = n_precip_zones)


summary (precip_pft_mod)
plot (precip_pft_mod) # yay happy fuzzy caterpillars 
pp_check (precip_pft_mod)

random_effects_obs_genus <- ranef(precip_pft_mod)
print(random_effects_obs_genus)

# happy model - no divergent transitions, rhats at 1 and lots of ess

#saveRDS(precip_pft_mod, "models/obs_precip_mod.RDS")

precip_pft_mod_10km <- readRDS("models/obs_precip_mod.RDS")

# high res model 
precip_pft_mod_1km <- brm (leafn ~ mean_precip * functional_group + (1|genus), data = n_precip_zones_1km)

summary (precip_pft_mod_1km)
plot (precip_pft_mod_1km) # yay happy fuzzy caterpillars 
pp_check (precip_pft_mod_1km)

random_effects_obs_genus_1km <- ranef(precip_pft_mod_1km)
print(random_effects_obs_genus_1km)

#saveRDS(precip_pft_mod_1km, "models/obs_precip_mod_1km.RDS")

precip_pft_mod_1km <- readRDS("models/obs_precip_mod_1km.RDS")



# plotting model results ----

obs_precip_mod_data <- expand_grid(mean_precip = seq(0, 90, by = 5), 
                                       n = seq(0, 5, by = 0.5),
                                       functional_group = levels (as.factor(n_precip_zones$functional_group)))
                                       #genus = levels (as.factor(n_precip_zones$genus)))

obs_precip_mod_pred <- precip_pft_mod_1km %>% 
  epred_draws(newdata = obs_precip_mod_data, allow_new_levels = TRUE)

(obs_precip_mod_1km_fit <- ggplot() +
    geom_point(data = n_precip_zones_1km, aes(x = mean_precip, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
   ylab("Leaf Nitrogen Concentration (%)") + 
   xlab("Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,75)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5)) +
   theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85),
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15)))

ggsave (obs_precip_mod_1km_fit, filename = "graphs/obs_precip_mod_1km_fit.png")

(obs_precip_plot_simple <- ggplot() +
    geom_point(data = n_precip_zones_1km, aes(x = mean_precip, y = leafn, color = ordered (functional_group), fill = ordered (functional_group))) +   # raw data
    stat_lineribbon(data = obs_precip_mod_pred, aes(y = .epred, x = mean_precip, color = ordered (functional_group), fill = ordered (functional_group)), .width = c(.95), # regression line and CI
                    alpha = 0.25) +
    scale_fill_manual(values = c("#117733", "#332288", "#AA4499")) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab("Leaf Nitrogen Concentration (%)") + 
    xlab("Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,90)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text = element_text (size = 15),
          axis.title.x = element_blank(),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15)))

### whittaker plot attempt ----

# download and extract worldclim temp data

temp_months <- worldclim_global (var = "tavg", res = 10, path = 'data')

temp <- mean (temp_months)

precip_annual <- mean (precip_year_1km)

arctic_temp <- terra::extract(temp, n_points)
arctic_annual_precip <- terra::extract(precip_annual, n_points)

# add temp data to trait datasheet 
n_precip_temp <- n_precip_zones_1km %>% 
  mutate (ID = row_number()) %>%  
  left_join (arctic_temp, by = "ID") %>% 
  rename (temp = mean) %>% 
  left_join (arctic_annual_precip, by = "ID") %>% 
  rename (annual_precip = mean)

  

obs_common_species <- n_precip_zones_1km %>% 
  group_by (species) %>% 
  tally () %>% 
  filter (n > 5) %>% 
  pull (species)

obs_species_avg <- n_precip_temp %>% 
  filter(species %in% obs_common_species) %>% 
  group_by (functional_group, genus, species) %>% 
  summarise(
    avg_nitrogen = mean(leafn),
    avg_precipitation = mean(annual_precip),
    avg_temp = mean(temp, na.rm = TRUE))


(obs_whittaker <- ggplot (data = obs_species_avg, aes (x = avg_temp, 
                                                   y = avg_precipitation, 
                                                   col = functional_group)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
  geom_point(aes (size = avg_nitrogen)) +
    xlab ("Average Annual Temperature (°C)") +
    ylab ("Annual Preciptitaion (mm)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0,120)) +
  theme_classic()+ 
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (obs_whittaker, filename = "graphs/obs_whittaker.png")
  
(precip_species_avg_n_obs_plot <- ggplot (data = obs_species_avg, aes (x = avg_precipitation, 
                                                                       y = avg_nitrogen, 
                                                                       col = functional_group)) +
    geom_point (size = 3) +
   # stat_ellipse(geom = "polygon", aes(x=avg_precipitation, y=avg_nitrogen, 
    #                                   fill = functional_group, 
     #                                  alpha = 0.2, size = 0.2)) +
    scale_color_manual(values = c("#117733", "#332288", "#AA4499")) +
    ylab ("Leaf Nitrogen Concentration (%)") +
    xlab ("Annual Precipitation (mm)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0,100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,3.5)) +
    theme_classic () +
    theme(legend.title = element_blank(),
          legend.position = "right",  # Move legend to the right
          axis.text = element_text (size = 15),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text (size = 15),
          axis.title.x = element_text(margin = margin(t = 20)),  # Adjust x-axis label margin
          axis.title.y = element_text(margin = margin(r = 20))))

ggsave (precip_species_avg_n_obs_plot, filename = "graphs/precip_species_avg_n_obs_plot.png")

