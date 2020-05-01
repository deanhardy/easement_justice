rm(list=ls())

library(tidyverse)
library(sf)
library(ggvoronoi)
library(lwgeom)
library(tidycensus)
options(tigris_use_cache = TRUE)

utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

##############################################################
## data import
##############################################################
cons <- st_read(file.path(datadir, 'cl_bufs.geojson')) %>%
  rowid_to_column() %>%
  filter(buf_m == 320) %>%
  st_transform(crs = utm) %>%
  st_centroid()

## import census data
bg <- st_read(file.path(datadir, "bg_demg.geojson"), stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  mutate(statefp = ifelse(STATEFP == 37, 45,
                          ifelse(STATEFP == 12, 13, 
                                 ifelse(STATEFP == 1, 13, STATEFP))))%>%
  filter(statefp != 'NA') %>%
  st_transform(crs = utm)

lc <- st_read(file.path(datadir, 'lc_tier1/lc_tier1.shp'), stringsAsFactors = FALSE) %>%
  st_transform(crs = utm)

box <- st_bbox(cons)

c2 <- st_union(cons)

vo <- st_voronoi(c2)
