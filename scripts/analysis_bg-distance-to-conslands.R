################################################################################################
## PURPOSE: This script analyzes block group distance to conservation lands
## BY: Dean Hardy
################################################################################################

rm(list=ls())

library(tidyverse)
library(sf)
library(tigris)
library(tmap)

## define variables
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

pub_buf <- NULL
pvt_buf <- NULL

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

##############################################################
## data import
##############################################################
cl <- st_read(file.path(datadir, 'cl.geojson')) %>%
  st_transform(alb)

## import census data
bg <- st_read(file.path(datadir, "bg_demg.geojson"), stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  mutate(statefp = ifelse(STATEFP == 37, 45,
                          ifelse(STATEFP == 12, 13, 
                                 ifelse(STATEFP == 1, 13, STATEFP))))%>%
  filter(statefp != 'NA') %>%
  st_transform(crs = alb)

## import lowcountry boundary file
lc_tier1 <- st_read(file.path(datadir, "lc_tier1/lc_tier1.shp")) %>%
  st_transform(crs = alb) %>%
  mutate(area = st_area(geometry)*3.86102e-7)

bg2 <- st_intersection(bg, lc_tier1)

## some ideas for how to do this
## calc minimum distance between multiple polygons
## https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r

# create an index of the nearest feature
index <- st_nearest_feature(x = bg2, y = cl)

# slice based on the index
cl2 <- cl %>% slice(index)

# calculate distance between polygons
poly_dist <- st_distance(x = bg2, y= cl2, by_element = TRUE)

# add the distance calculations to the BG polygons
bg2$distance <- poly_dist

bg_dist <- st_distance(bg2, cl, by_element = FALSE, tolerance = 1000)
#
# bgd <- bg_dist[1:10] %>%
#   rowwise() %>%
#   summarise(mean = mean(.))
