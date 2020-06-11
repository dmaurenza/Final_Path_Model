"https://github.com/dmaurenza/Map_Cifor.git"

# Loading Packages ----
library(tidyverse)
library(raster)
library(sf)
library(maptools)

# Loading Data ----
data(wrld_simpl)
sites <- read_csv("/media/dmaurenza/42435441-533d-4576-99fb-0da16179c58f/CIFOR/Path-Analysis/Path_Analysis_Final/RData/datafull_range.csv")

# Converting to SF object ----
world <- st_as_sf(wrld_simpl)

# Croping map region-----
extention <- st_bbox(c(xmin = -180, xmax = 180, ymax = 35, ymin = -35), crs = crs(world))
plot(st_crop(world$geometry, extention))


