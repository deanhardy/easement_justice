################################################################################################
## PURPOSE: This script analyzes block group distance to conservation lands
## GOAL: If you're a resident living in a particular analysis unit (BG in this case), 
## what's your access to conservation reserves
## 
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


####################################################
## calc minimum distance between multiple polygons
## https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
####################################################

# create an index of the nearest feature
index <- st_nearest_feature(x = bg2, y = cl)

# slice based on the index
cl2 <- cl %>% slice(index)

# calculate distance between polygons
poly_dist <- st_distance(x = bg2, y= cl2, by_element = TRUE)

# add the distance calculations to the BG polygons
bg2$distance <- poly_dist

####################################################
## exploring how to calculate distance between multiple to multiple polygons
####################################################
## creates matrix of pairwise distances
# bg_dist <- st_distance(bg2, cl, by_element = FALSE, tolerance = 1000) ## not understanding tolerance

## converts matrix to data frame and adds row and column names for referencing OG BGs and CLs
# bgd <- as.data.frame(bg_dist)
# rownames(bgd) <- bg2$GEOID
# colnames(bgd) <- cl$id

## exploring other means of setting distance delimiter
# temp <- st_within(bg2, cl)
# temp <- st_is_within_distance(bg2, cl = bg2, 1000)

## example illustration of selecting all reserves within specified distance of block group
## this is what I want! but with distances calculated to each selected reserve as well
i = 506
d = 1000
ggplot() +
  geom_sf(data = st_buffer(bg2[i,], d)) + 
  geom_sf(data = bg2[i,], fill = 'blue') +
  geom_sf(data = st_intersection(cl, st_buffer(bg2[i,], d)), fill = 'red')

## working out how to do the above using apply, or for loop
## after doing that, I need to add calculate distance to each selected reserve
## possible solution: https://stackoverflow.com/questions/48505551/use-apply-with-a-simple-features-sf-function/48525396#48525396
# temp <- apply(bg2[100:110], 1, st_buffer(bg2, 10000))

## generates basic descriptive stats for one BG
cli <- st_intersection(cl, st_buffer(bg2[i,], d)) %>%
  st_distance(bg2[i,], ., by_element = FALSE) %>%
  drop_units() %>%
  as.data.frame() %>%
  summarise(n = ncol(.), mean = sum(.)/ncol(.), max = max(.), min = min(.)) %>%
  mutate(geoid = bg2[i,]$GEOID, dist = d) %>%
  select(geoid, dist, n, mean, max, min)

## for loop to run through all BGs and generate descriptive stats
cl.dist <- NULL
i = NULL
d = 1000
for (i in 1:nrow(bg2)) {
cli <- st_intersection(cl, st_buffer(bg2[i,], d)) %>%
  st_distance(bg2[i,], ., by_element = FALSE) %>%
  drop_units() %>%
  as.data.frame() %>%
  summarise(n = ncol(.), mean = sum(.)/ncol(.), max = max(.), min = min(.)) %>%
  mutate(geoid = bg2[i,]$GEOID, dist = d) %>%
  select(geoid, dist, n, mean, max, min)

cl.dist = rbind(cl.dist, cli)
}
