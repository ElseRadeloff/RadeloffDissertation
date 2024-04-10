# load data 

load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/try_for_else (2).RData")

load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/ttt_for_else (1).RData")

setwd("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/Dissertation")

# load libraries 
library (tidyverse)


# exploring data structure 
head (mTRY2)
head(try.ttt2)
# I think I'll start with try.ttt2 for now because I see the coordinates in there

unique(try.ttt2$TraitShort)
# traits I have: SLA, LeafN, Leaf.d15N, LeafC, C.N.ratio

# make a new dataframe with just leaf N to see how many observations there are 
ttt.n <- try.ttt2 %>% 
  filter (TraitShort == "LeafN") %>%  # select rows with nitrogen observations 
  mutate (plant_type = case_when(
      grepl ("Eriophorum", AccSpeciesName) ~ "graminoid",
      grepl ("Dryas", AccSpeciesName) ~ "evergreen",
      grepl ("Oxyria", AccSpeciesName) ~ "forb",
      grepl ("Salix", AccSpeciesName) ~ "shrub",
      grepl ("Betula", AccSpeciesName) ~ "shrub",
      grepl ("Vaccinium", AccSpeciesName) ~ "graminoid",
      grepl ("Cassiope", AccSpeciesName) ~ "evergreen",
      grepl ("Empetrum", AccSpeciesName) ~ "evergreen",
      grepl ("Festuca", AccSpeciesName) ~ "graminoid",
      grepl ("Angelica", AccSpeciesName) ~ "forb",
      grepl ("Lycopodium", AccSpeciesName) ~ "forb",  # clubmoss??
      grepl ("Lysimachia", AccSpeciesName) ~ "forb",
      grepl ("Silene", AccSpeciesName) ~ "forb",
      grepl ("Milium", AccSpeciesName) ~ "graminoid",
      grepl ("Alnus", AccSpeciesName) ~ "tree",
      grepl ("Phyllodoce", AccSpeciesName) ~ "evergreen",
      grepl ("Euphrasia", AccSpeciesName) ~ "forb",
      grepl ("Sorbus", AccSpeciesName) ~ "shrub",
      grepl ("Gymocarpium", AccSpeciesName) ~ "forb",
      grepl ("Geranium", AccSpeciesName) ~ "forb",
      grepl ("Populus", AccSpeciesName) ~ "tree",
      grepl ("Rumex", AccSpeciesName) ~ "forb",
      grepl ("Anthoxanthum", AccSpeciesName) ~ "graminoid",
      grepl ("Melampyrum", AccSpeciesName) ~ "forb",
      grepl ("Astragalus", AccSpeciesName) ~ "forb",  # ??
      
      grepl ("Juniperus", AccSpeciesName) ~ "evergreen",
      grepl ("Crepis", AccSpeciesName) ~ "forb",
      grepl ("Pedicularis", AccSpeciesName) ~ "forb",
      grepl ("Carex", AccSpeciesName) ~ "graminoid",
      grepl ("Rhododendron", AccSpeciesName) ~ "evergreen",
      grepl ("Tofieldia", AccSpeciesName) ~ "forb",
      grepl ("Equisetum", AccSpeciesName) ~ "forb",
      grepl ("Bartsia", AccSpeciesName) ~ "forb",
      grepl ("Arctous", AccSpeciesName) ~ "shrub",
      grepl ("Poa", AccSpeciesName) ~ "graminoid",
      grepl ("Ledum", AccSpeciesName) ~ "evergreen",
      grepl ("Larix", AccSpeciesName) ~ "tree",
      grepl ("Calamagrostis", AccSpeciesName) ~ "graminoid",
      grepl ("Pinus", AccSpeciesName) ~ "tree",
      grepl ("Arctagrostis", AccSpeciesName) ~ "graminoid",
      grepl ("Alopercus", AccSpeciesName) ~ "graminoid",
      grepl ("Huperzia", AccSpeciesName) ~ "forb",  # clubmoss?? 
      grepl ("Luzula", AccSpeciesName) ~ "graminoid",
      grepl ("Persicaria", AccSpeciesName) ~ "forb",
      grepl ("Kobresia", AccSpeciesName) ~ "graminoid",
      grepl ("Andromeda", AccSpeciesName) ~ "evergreen",
      grepl ("Agrostis", AccSpeciesName) ~ "graminoid",
      grepl ("Alchemilla", AccSpeciesName) ~ "forb",
      grepl ("Antennaria", AccSpeciesName) ~ "forb",
      grepl ("Anthriscus", AccSpeciesName) ~ "forb",
      grepl ("Campanula", AccSpeciesName) ~ "forb",
      grepl ("Cerastium", AccSpeciesName) ~ "forb",
      grepl ("Lactuca", AccSpeciesName) ~ "forb",
      grepl ("Cornus", AccSpeciesName) ~ "forb",  # ???
      
      grepl ("Deschampsia", AccSpeciesName) ~ "graminoid",
      grepl ("Epilobium", AccSpeciesName) ~ "forb",
      grepl ("Gentiana", AccSpeciesName) ~ "forb",
      grepl ("Gnaphalium", AccSpeciesName) ~ "forb",
      grepl ("Hieracium", AccSpeciesName) ~ "forb",
      grepl ("Hierochloe", AccSpeciesName) ~ "graminoid",
      grepl ("Linnaea", AccSpeciesName) ~ "shrub", ### check
      grepl ("Phleum", AccSpeciesName) ~ "graminoid",
      grepl ("Polemonium", AccSpeciesName) ~ "forb",
      grepl ("Potentilla", AccSpeciesName) ~ "forb",
      grepl ("Pyrola", AccSpeciesName) ~ "forb",  # evergreen herbaceous??
      grepl ("Ranunculus", AccSpeciesName) ~ "forb",
      grepl ("Saussurea", AccSpeciesName) ~ "forb",
      grepl ("Selaginella", AccSpeciesName) ~ "forb",  # clubmoss??
      grepl ("Sibbaldia", AccSpeciesName) ~ "forb",
      grepl ("Solidago", AccSpeciesName) ~ "forb",
      grepl ("Thalictrum", AccSpeciesName) ~ "forb",
      grepl ("Trollius", AccSpeciesName) ~ "forb",
      grepl ("Veronica", AccSpeciesName) ~ "forb",
      grepl ("Viola", AccSpeciesName) ~ "forb",
      grepl ("Eutrema", AccSpeciesName) ~ "forb",
      grepl ("Saxifraga", AccSpeciesName) ~ "forb",
      grepl ("Silene", AccSpeciesName) ~ "forb",
      grepl ("Cerastium", AccSpeciesName) ~ "forb",
      grepl ("Trisetum", AccSpeciesName) ~ "forb",
      grepl ("Erigeron", AccSpeciesName) ~ "forb",
      grepl ("Minuartia", AccSpeciesName) ~ "forb",
      grepl ("Luetkea", AccSpeciesName) ~ "evergreen", ## ??
      grepl ("Arenaria", AccSpeciesName) ~ "forb",
      grepl ("Braya", AccSpeciesName) ~ "forb",
      grepl ("Mertensia", AccSpeciesName) ~ "forb",
      grepl ("Cystopteris", AccSpeciesName) ~ "forb",
      grepl ("Koenigia", AccSpeciesName) ~ "forb",
      grepl ("Sagina", AccSpeciesName) ~ "forb",
      grepl ("Taraxacum", AccSpeciesName) ~ "forb",
      grepl ("Papaver", AccSpeciesName) ~ "forb",
      grepl ("Potentilla", AccSpeciesName) ~ "forb",
      grepl ("Cochlearia", AccSpeciesName) ~ "graminoid",
      grepl ("Stellaria", AccSpeciesName) ~ "forb",
      grepl ("Arabis", AccSpeciesName) ~ "forb",
      grepl ("Puccinelia", AccSpeciesName) ~ "graminoid",
      grepl ("Cardamine", AccSpeciesName) ~ "forb",
      grepl ("Juncus", AccSpeciesName) ~ "graminoid",  # called an herb in 'flora of the canadiaan arctic archipelago'
      grepl ("Dupontia", AccSpeciesName) ~ "graminoid",
      grepl ("Draba", AccSpeciesName) ~ "forb",
      grepl ("Polemonium", AccSpeciesName) ~ "forb",
      grepl ("Chrysosplenium", AccSpeciesName) ~ "forb",
      grepl ("Arctophila", AccSpeciesName) ~ "graminoid",
      grepl ("Campanula", AccSpeciesName) ~ "forb",
      grepl ("Honckenya", AccSpeciesName) ~ "forb",
      grepl ("Petasites", AccSpeciesName) ~ "forb",
      grepl ("Rubus", AccSpeciesName) ~ "forb",  # cloudberry! 
      grepl ("Valeriana", AccSpeciesName) ~ "forb",
      grepl ("Botrychium", AccSpeciesName) ~ "forb",
      grepl ("Gymnadenia", AccSpeciesName) ~ "forb",  # orchid
      grepl ("Loiseleuria", AccSpeciesName) ~ "evergreen",
      grepl ("Erigeron", AccSpeciesName) ~ "forb",
      grepl ("Diapensia", AccSpeciesName) ~ "evergreen",
      grepl ("Pinus", AccSpeciesName) ~ "tree",
      grepl ("Pinguicula", AccSpeciesName) ~ "forb",
      grepl ("Sedum", AccSpeciesName) ~ "forb",  # evergreen perennial? 
      grepl ("Alchemilla", AccSpeciesName) ~ "forb",
      grepl ("Parnassia", AccSpeciesName) ~ "forb",
      grepl ("Antennaria", AccSpeciesName) ~ "forb",
      grepl ("Harrimanella", AccSpeciesName) ~ "shrub",
      grepl ("Alopecurus", AccSpeciesName) ~ "graminoid",
      grepl ("Gymnocarpium", AccSpeciesName) ~ "forb",
      ))

# need to decide whether club mosses are mosses or forbs - one time i called them moss, and another i called them clubmoss 

# testing for n/a rows where I missed a species
ttt.n.test <- try.n %>% 
  filter (is.na (plant_type))

unique (ttt.n$AccSpeciesName)
unique(ttt.n$plant_type)

# looks like coordinates are for a whole site, not necessarily where they sampled the plants
  # low resolution ? 

(ttt_plant_type_boxplot <- ggplot (ttt.n, aes (x = plant_type, y = StdValue)) +
    geom_boxplot( fill = "light blue") +
    theme_classic()+
    xlab ("Plant Functional Group") +
    ylab ("Nitrogen")
)

ggsave(ttt_plant_type_boxplot, filename = "graphs/ttt_plant_type_boxploe.png")


# TRY data ----

head (mTRY2)

unique (mTRY2$TraitShort)
#"Leaf nitrogen (N) content per leaf dry mass"
# "Leaf nitrogen (N) content per leaf area"    
# "Leaf carbon/nitrogen (C/N) ratio" 

try.n <- mTRY2 %>% 
  filter (TraitShort == "Leaf nitrogen (N) content per leaf dry mass") %>% 
  filter(!is.na(Lat)) %>% 
  mutate (plant_type = case_when(
    grepl ("Eriophorum", AccSpeciesName) ~ "graminoid",
    grepl ("Dryas", AccSpeciesName) ~ "evergreen",
    grepl ("Oxyria", AccSpeciesName) ~ "forb",
    grepl ("Salix", AccSpeciesName) ~ "shrub",
    grepl ("Betula", AccSpeciesName) ~ "shrub",
    grepl ("Vaccinium", AccSpeciesName) ~ "graminoid",
    grepl ("Cassiope", AccSpeciesName) ~ "evergreen",
    grepl ("Empetrum", AccSpeciesName) ~ "evergreen",
    grepl ("Festuca", AccSpeciesName) ~ "graminoid",
    grepl ("Angelica", AccSpeciesName) ~ "forb",
    grepl ("Lycopodium", AccSpeciesName) ~ "forb",  # clubmoss??
    grepl ("Lysimachia", AccSpeciesName) ~ "forb",
    grepl ("Silene", AccSpeciesName) ~ "forb",
    grepl ("Milium", AccSpeciesName) ~ "graminoid",
    grepl ("Alnus", AccSpeciesName) ~ "tree",
    grepl ("Phyllodoce", AccSpeciesName) ~ "evergreen",
    grepl ("Euphrasia", AccSpeciesName) ~ "forb",
    grepl ("Sorbus", AccSpeciesName) ~ "shrub",
    grepl ("Gymocarpium", AccSpeciesName) ~ "forb",
    grepl ("Geranium", AccSpeciesName) ~ "forb",
    grepl ("Populus", AccSpeciesName) ~ "tree",
    grepl ("Rumex", AccSpeciesName) ~ "forb",
    grepl ("Anthoxanthum", AccSpeciesName) ~ "graminoid",
    grepl ("Melampyrum", AccSpeciesName) ~ "forb",
    grepl ("Astragalus", AccSpeciesName) ~ "forb",  # ??
    
    grepl ("Juniperus", AccSpeciesName) ~ "evergreen",
    grepl ("Crepis", AccSpeciesName) ~ "forb",
    grepl ("Pedicularis", AccSpeciesName) ~ "forb",
    grepl ("Carex", AccSpeciesName) ~ "graminoid",
    grepl ("Rhododendron", AccSpeciesName) ~ "evergreen",
    grepl ("Tofieldia", AccSpeciesName) ~ "forb",
    grepl ("Equisetum", AccSpeciesName) ~ "forb",
    grepl ("Bartsia", AccSpeciesName) ~ "forb",
    grepl ("Arctous", AccSpeciesName) ~ "shrub",
    grepl ("Poa", AccSpeciesName) ~ "graminoid",
    grepl ("Ledum", AccSpeciesName) ~ "evergreen",
    grepl ("Larix", AccSpeciesName) ~ "tree",
    grepl ("Calamagrostis", AccSpeciesName) ~ "graminoid",
    grepl ("Pinus", AccSpeciesName) ~ "tree",
    grepl ("Arctagrostis", AccSpeciesName) ~ "graminoid",
    grepl ("Alopercus", AccSpeciesName) ~ "graminoid",
    grepl ("Huperzia", AccSpeciesName) ~ "forb",  # clubmoss?? 
    grepl ("Luzula", AccSpeciesName) ~ "graminoid",
    grepl ("Persicaria", AccSpeciesName) ~ "forb",
    grepl ("Kobresia", AccSpeciesName) ~ "graminoid",
    grepl ("Andromeda", AccSpeciesName) ~ "evergreen",
    grepl ("Agrostis", AccSpeciesName) ~ "graminoid",
    grepl ("Alchemilla", AccSpeciesName) ~ "forb",
    grepl ("Antennaria", AccSpeciesName) ~ "forb",
    grepl ("Anthriscus", AccSpeciesName) ~ "forb",
    grepl ("Campanula", AccSpeciesName) ~ "forb",
    grepl ("Cerastium", AccSpeciesName) ~ "forb",
    grepl ("Lactuca", AccSpeciesName) ~ "forb",
    grepl ("Cornus", AccSpeciesName) ~ "forb",  # ???
    
    grepl ("Deschampsia", AccSpeciesName) ~ "graminoid",
    grepl ("Epilobium", AccSpeciesName) ~ "forb",
    grepl ("Gentiana", AccSpeciesName) ~ "forb",
    grepl ("Gnaphalium", AccSpeciesName) ~ "forb",
    grepl ("Hieracium", AccSpeciesName) ~ "forb",
    grepl ("Hierochloe", AccSpeciesName) ~ "graminoid",
    grepl ("Linnaea", AccSpeciesName) ~ "shrub", ### check
    grepl ("Phleum", AccSpeciesName) ~ "graminoid",
    grepl ("Polemonium", AccSpeciesName) ~ "forb",
    grepl ("Potentilla", AccSpeciesName) ~ "forb",
    grepl ("Pyrola", AccSpeciesName) ~ "forb",  # evergreen herbaceous??
    grepl ("Ranunculus", AccSpeciesName) ~ "forb",
    grepl ("Saussurea", AccSpeciesName) ~ "forb",
    grepl ("Selaginella", AccSpeciesName) ~ "forb",  # clubmoss??
    grepl ("Sibbaldia", AccSpeciesName) ~ "forb",
    grepl ("Solidago", AccSpeciesName) ~ "forb",
    grepl ("Thalictrum", AccSpeciesName) ~ "forb",
    grepl ("Trollius", AccSpeciesName) ~ "forb",
    grepl ("Veronica", AccSpeciesName) ~ "forb",
    grepl ("Viola", AccSpeciesName) ~ "forb",
    grepl ("Eutrema", AccSpeciesName) ~ "forb",
    grepl ("Saxifraga", AccSpeciesName) ~ "forb",
    grepl ("Silene", AccSpeciesName) ~ "forb",
    grepl ("Cerastium", AccSpeciesName) ~ "forb",
    grepl ("Trisetum", AccSpeciesName) ~ "forb",
    grepl ("Erigeron", AccSpeciesName) ~ "forb",
    grepl ("Minuartia", AccSpeciesName) ~ "forb",
    grepl ("Luetkea", AccSpeciesName) ~ "evergreen", ## ??
    grepl ("Arenaria", AccSpeciesName) ~ "forb",
    grepl ("Braya", AccSpeciesName) ~ "forb",
    grepl ("Mertensia", AccSpeciesName) ~ "forb",
    grepl ("Cystopteris", AccSpeciesName) ~ "forb",
    grepl ("Koenigia", AccSpeciesName) ~ "forb",
    grepl ("Sagina", AccSpeciesName) ~ "forb",
    grepl ("Taraxacum", AccSpeciesName) ~ "forb",
    grepl ("Papaver", AccSpeciesName) ~ "forb",
    grepl ("Potentilla", AccSpeciesName) ~ "forb",
    grepl ("Cochlearia", AccSpeciesName) ~ "graminoid",
    grepl ("Stellaria", AccSpeciesName) ~ "forb",
    grepl ("Arabis", AccSpeciesName) ~ "forb",
    grepl ("Puccinelia", AccSpeciesName) ~ "graminoid",
    grepl ("Cardamine", AccSpeciesName) ~ "forb",
    grepl ("Juncus", AccSpeciesName) ~ "graminoid",  # called an herb in 'flora of the canadiaan arctic archipelago'
    grepl ("Dupontia", AccSpeciesName) ~ "graminoid",
    grepl ("Draba", AccSpeciesName) ~ "forb",
    grepl ("Polemonium", AccSpeciesName) ~ "forb",
    grepl ("Chrysosplenium", AccSpeciesName) ~ "forb",
    grepl ("Arctophila", AccSpeciesName) ~ "graminoid",
    grepl ("Campanula", AccSpeciesName) ~ "forb",
    grepl ("Honckenya", AccSpeciesName) ~ "forb",
    grepl ("Petasites", AccSpeciesName) ~ "forb",
    grepl ("Rubus", AccSpeciesName) ~ "forb",  # cloudberry! 
    grepl ("Valeriana", AccSpeciesName) ~ "forb",
    grepl ("Botrychium", AccSpeciesName) ~ "forb",
    grepl ("Gymnadenia", AccSpeciesName) ~ "forb",  # orchid
    grepl ("Loiseleuria", AccSpeciesName) ~ "evergreen",
    grepl ("Erigeron", AccSpeciesName) ~ "forb",
    grepl ("Diapensia", AccSpeciesName) ~ "evergreen",
    grepl ("Pinus", AccSpeciesName) ~ "tree",
    grepl ("Pinguicula", AccSpeciesName) ~ "forb",
    grepl ("Sedum", AccSpeciesName) ~ "forb",  # evergreen perennial? 
    grepl ("Alchemilla", AccSpeciesName) ~ "forb",
    grepl ("Parnassia", AccSpeciesName) ~ "forb",
    grepl ("Antennaria", AccSpeciesName) ~ "forb",
    grepl ("Harrimanella", AccSpeciesName) ~ "shrub",
    grepl ("Alopecurus", AccSpeciesName) ~ "graminoid",
    grepl ("Gymnocarpium", AccSpeciesName) ~ "forb",
    grepl ("Picea", AccSpeciesName) ~ "tree",
    grepl ("Rosa", AccSpeciesName) ~ "",
    grepl ("Spiraea", AccSpeciesName) ~ "",
    grepl ("Eri", AccSpeciesName) ~ "graminoid",
    grepl ("Achillea", AccSpeciesName) ~ "",
    grepl ("Caltha", AccSpeciesName) ~ "",
    grepl ("Diphasiastrum", AccSpeciesName) ~ "",
    grepl ("Drosera", AccSpeciesName) ~ "",
    grepl ("Elymus", AccSpeciesName) ~ "",
    grepl ("Filipendula", AccSpeciesName) ~ "",
    grepl ("Lathyrus", AccSpeciesName) ~ "",
    grepl ("Matteuccia", AccSpeciesName) ~ "",
    grepl ("Orthilia", AccSpeciesName) ~ "",
    grepl ("Polytrichum", AccSpeciesName) ~ "",
    grepl ("Rhianthus", AccSpeciesName) ~ "",
    grepl ("Ribes", AccSpeciesName) ~ "",
    grepl ("Tanacetum", AccSpeciesName) ~ "",
    grepl ("Trifolium", AccSpeciesName) ~ "",
    grepl ("Vicia", AccSpeciesName) ~ "",
    grepl ("Comarum", AccSpeciesName) ~ "",
    grepl ("Hippuris", AccSpeciesName) ~ "",
    grepl ("Menyanthes", AccSpeciesName) ~ "",
    grepl ("Potamogeton", AccSpeciesName) ~ "",
    grepl ("Prunus", AccSpeciesName) ~ "",
    grepl ("Sparganium", AccSpeciesName) ~ "",
    grepl ("Utricularia", AccSpeciesName) ~ "",
    grepl ("Hedysarum", AccSpeciesName) ~ "",
    grepl ("Lupinus", AccSpeciesName) ~ "",
    grepl ("Oxytropis", AccSpeciesName) ~ "",
    grepl ("Dasiphora", AccSpeciesName) ~ "",
    grepl ("Trichophorum", AccSpeciesName) ~ "",
    grepl ("Nardus", AccSpeciesName) ~ "",
    grepl ("Aconitum", AccSpeciesName) ~ "",
    grepl ("Anemone", AccSpeciesName) ~ "",
    grepl ("Artemisia", AccSpeciesName) ~ "",
    grepl ("Elymus", AccSpeciesName) ~ "",
    grepl ("Gentiaglauca", AccSpeciesName) ~ "",
    grepl ("Geum", AccSpeciesName) ~ "",
    grepl ("Zea", AccSpeciesName) ~ "",
    grepl ("Veratrum", AccSpeciesName) ~ "",
    grepl ("Maianthemum", AccSpeciesName) ~ "",
    grepl ("Melica", AccSpeciesName) ~ "",
    grepl ("Galium", AccSpeciesName) ~ "",
    grepl ("Hypericum", AccSpeciesName) ~ "",
    grepl ("Oxalis", AccSpeciesName) ~ "",
    grepl ("Sanguisorba", AccSpeciesName) ~ "",
    grepl ("Hedysarum", AccSpeciesName) ~ "",
    grepl ("Paeonia", AccSpeciesName) ~ "",
    grepl ("Paris", AccSpeciesName) ~ "",
    grepl ("Daactylorhiza", AccSpeciesName) ~ "",
    grepl ("Tussilago", AccSpeciesName) ~ "",
    grepl ("Primula", AccSpeciesName) ~ "",
    grepl ("Lathyrus", AccSpeciesName) ~ "",
    grepl ("Phalaris", AccSpeciesName) ~ "",
    grepl ("Urtica", AccSpeciesName) ~ "",
    grepl ("Tanacetum", AccSpeciesName) ~ "",
    grepl ("Allium", AccSpeciesName) ~ "",
    grepl ("Senecio", AccSpeciesName) ~ "",
    grepl ("Parasenecio", AccSpeciesName) ~ "",
    grepl ("Cirsium", AccSpeciesName) ~ "",
    grepl ("Dianthus", AccSpeciesName) ~ "",
    grepl ("Moneses", AccSpeciesName) ~ "",
    grepl ("Cypripedium", AccSpeciesName) ~ "",
    grepl ("Melilotus", AccSpeciesName) ~ "",
    grepl ("Cirsium", AccSpeciesName) ~ "",
    grepl ("Plantago", AccSpeciesName) ~ "",
    grepl ("Chenopodium", AccSpeciesName) ~ "",
    grepl ("Polygonum", AccSpeciesName) ~ "",
    grepl ("Galeopsis", AccSpeciesName) ~ "",
    grepl ("Matricaria", AccSpeciesName) ~ "",
    grepl ("Myosotis", AccSpeciesName) ~ "",
    grepl ("Erysimum", AccSpeciesName) ~ "",
    grepl ("Urtica", AccSpeciesName) ~ "",
    grepl ("Dactylis", AccSpeciesName) ~ "",
    grepl ("Tripleurospermum", AccSpeciesName) ~ "",
    grepl ("Leucanthemum", AccSpeciesName) ~ "",
    grepl ("Dianthus", AccSpeciesName) ~ "",
    grepl ("Ajuga", AccSpeciesName) ~ "",
    grepl ("Fragaria", AccSpeciesName) ~ "",
    grepl ("Pulmonaria", AccSpeciesName) ~ "",
    grepl ("Arctium", AccSpeciesName) ~ "",
    grepl ("Scorzoneroides", AccSpeciesName) ~ "",
    grepl ("Linaria", AccSpeciesName) ~ "",
    grepl ("Gagea", AccSpeciesName) ~ "",
    grepl ("Rhinanthus", AccSpeciesName) ~ "",
    grepl ("Dactylorhiza", AccSpeciesName) ~ "",
    grepl ("Lagotis", AccSpeciesName) ~ "",
    grepl ("Ligusticum", AccSpeciesName) ~ "",
    grepl ("Tephroseris", AccSpeciesName) ~ "",
    grepl ("Neottia", AccSpeciesName) ~ "",
    grepl ("Butomus", AccSpeciesName) ~ "",
    grepl ("Pimpinella", AccSpeciesName) ~ "",
    grepl ("Epipactic", AccSpeciesName) ~ "",
    grepl ("Rorippa", AccSpeciesName) ~ "",
    grepl ("VACCINIUM", AccSpeciesName) ~ "",
    grepl ("ARCTOSTAPHYLOS", AccSpeciesName) ~ "",
    grepl ("CAPSELLA", AccSpeciesName) ~ "",
    grepl ("ALISMA", AccSpeciesName) ~ "",
    grepl ("DRYOPTERIS", AccSpeciesName) ~ "",
    grepl ("Epipactis", AccSpeciesName) ~ "",
  ))

try.n.test <- try.n %>% 
  filter(is.na (plant_type))
unique(try.n.test$AccSpeciesName)
