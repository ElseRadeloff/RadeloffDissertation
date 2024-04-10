# Else Radeloff 
# 24 January 2023

# Extracting WorldClim snow data at 10 m resolution 

setwd("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation")

# Loading packages ---- 

library (terra)
library (tidyverse)

# creating a SpatRaster 
snow1 <- rast("data/worldclim/wc2.1_10m_prec_2020-01.tif")

# Import WorldClim Mean precip data at 10 minute resolution
# into SpatRaster layers 

precip1 <- rast ("data/worldclim/wc2.1_10m_prec_2020-01.tif")
precip2 <- rast ("data/worldclim/wc2.1_10m_prec_2020-02.tif")
precip3 <- rast ("data/worldclim/wc2.1_10m_prec_2020-03.tif")
precip4 <- rast ("data/worldclim/wc2.1_10m_prec_2020-04.tif")
precip5 <- rast ("data/worldclim/wc2.1_10m_prec_2020-05.tif")
precip6 <- rast ("data/worldclim/wc2.1_10m_prec_2020-06.tif")
precip7 <- rast ("data/worldclim/wc2.1_10m_prec_2020-07.tif")
precip8 <- rast ("data/worldclim/wc2.1_10m_prec_2020-08.tif")
precip9 <- rast ("data/worldclim/wc2.1_10m_prec_2020-09.tif")
precip10 <- rast ("data/worldclim/wc2.1_10m_prec_2020-10.tif")
precip11 <- rast ("data/worldclim/wc2.1_10m_prec_2020-11.tif")
precip12 <- rast ("data/worldclim/wc2.1_10m_prec_2020-12.tif")

# can import code directly with the package geodata
# ex: temp <- worldclim_global(var = "tavg", res = 10, path = tempdir())

# get info about class, dimensions, resolution, extent, coord references, etc
precip1

# plot a map! 
plot (precip1)

# need to stack raster layers 

precip <- c(precip1, precip2, precip3, precip4, precip5, precip6, precip7, precip8, precip9, precip10, precip11, precip12)
plot (precip)
# puts the 12 months side by side instead of in one map 

# added the months together 
# I think this is total yearly rainfall but it goes really high 
precip_add <- precip1 + precip2 + precip3 + precip4 + precip5 + precip6 + precip7 + precip8 + precip9 + precip10 + precip11 + precip12
plot(precip_add)



# add points onto the map from ttt data ----

#Define projections
WGSCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# load tundra trait team data
load("C:/Users/else5/OneDrive - University of Edinburgh/4th year/Dissertation/data/ttt_for_else (1).RData")

# need to first make ttt.n layer by selecting points w n data and adding functional group info but that's in another code (combine)
# make a new vector layer from coordinates in tundra trait data 
ttt_points <- vect (ttt.n, geom=c("Lon", "Lat"), crs=WGSCRS)

# plot with points on top yay !!!!!!
plot (precip_add)
points (ttt_points)

# extract precip data for those points----

arctic_precip <- extract(precip_add, ttt_points, bind = TRUE)

head (arctic_precip)

(boxplot <- ggplot(data = as.data.frame(arctic_precip), aes (x = wc2.1_10m_prec_2020-01.tif, y = LeafN)) +
    geom_boxplot)
boxplot
# not working because it doesn't like the '.tif' in the x so 
  # probs should rename that anyway 
