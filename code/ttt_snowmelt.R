TTT_cleaned_dataset_v1 <- read_csv("data/TTT_cleaned_dataset_v1.csv")

unique (TTT_cleaned_dataset_v1$Trait)

# load arctic zones map (high arctic, low arctic, sub arctic)
high_low_arc <- vect ("data/high_low_arctic_boundaries/Arctic_Zones_complete_polygons.shp")


TTT_year <- TTT_cleaned_dataset_v1 %>% 
  filter (Trait == "Leaf nitrogen (N) content per leaf dry mass") %>% 
  filter (Latitude > 60) %>% 
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
    TRUE ~ PlantGrowthForm)) %>% 
  # selecting columns to match Madi's data 
  select(Species, Value, Latitude, Longitude, PlantGrowthForm, ObsDataID, data_source, Genus, Year) %>% 
  # renaming columns to match Madi's data 
  rename (lat = Latitude,
          lon = Longitude,
          functional_group = PlantGrowthForm,
          sample_id = ObsDataID,
          n = Value,
          year = Year,
          species = Species) %>% 
  filter (functional_group != "tree"
          & functional_group != "lichen"
          & functional_group != "herb"
          & functional_group != "fern"
          & functional_group != "moss") %>% 
  filter (!is.na(lat))

ttt_points <- TTT_year %>% 
  select (species, IndividualID, lat, lon, year, Genus, functional_group, n, data_source)

#ttt_points_count <- ttt_points %>% 
#  filter (year != 1995) %>% 
#  group_by(lat, lon, year) %>% 
#  tally ()

unique (ttt_points$lat)

write_xlsx(ttt_points, "data/ttt_snowmelt_meta.xlsx")


