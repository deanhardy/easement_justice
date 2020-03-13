#####################################
## script analyses demographics of an self-defined region based on previously downloaded census data
## Author: Dean Hardy
#####################################

rm(list=ls())

library(tidyverse)
library(sf)
library(lwgeom)
library(tidycensus)
options(tigris_use_cache = TRUE)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

YR <- 2016
ST <- c('GA', 'SC')
gm <- NULL # used in for loop for calculating gmedian

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## import census data and filter to AOI states
bg <- st_read(file.path(datadir, "bg_data.geojson"), stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  mutate(statefp = ifelse(STATEFP == 37, 45,
                          ifelse(STATEFP == 12, 13, 
                                 ifelse(STATEFP == 1, 13, STATEFP))))%>%
  filter(statefp != 'NA' & STATEFP %in% c(13, 45)) %>%
  st_transform(crs = alb)

## read in AOI
AOI <- st_read(file.path(datadir, 'lc_tier1.shp'), stringsAsFactors = FALSE) %>%
  st_transform(crs = alb)

################################################
## working to add create int of AOI and states
################################################
library(tigris)

st <- states() %>% st_as_sf() %>%
  filter(STATEFP == 45 | STATEFP == 13) %>%
  st_transform(alb) %>%
  select(STATEFP, geometry)

intAOI <- st_intersection(st, AOI) 


###################################################
## apply proportional area adjustment to variables
## to assess count within AOI
###################################################

## define intersection between block groups and AOI
int <- as_tibble(st_intersection(intAOI, bg))

## proportional area adjustment/allocation method
percBGinAOI <- int %>%
  mutate(sqkm_bginAOI = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginAOI = (sqkm_bginAOI/sqkm_bg), sqkm_land = ALAND/ 1e6)

## save percBGinAOI data to use in median HH income estimation (see below)
bg_for_emed <- percBGinAOI %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(statefp, perc_bginAOI)

## demographic analysis of cons area ben zones
bz_geog <- percBGinAOI %>%
  mutate(tot_pop = total * perc_bginAOI,
         white = white * perc_bginAOI, 
         black = black * perc_bginAOI,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginAOI,
         latinx = latinx * perc_bginAOI,
         hu = hu * perc_bginAOI,
         sqkm_land = sqkm_land * perc_bginAOI) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(statefp) %>% ## regroups to cons areas after demo analysis on intersections
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = round(sum(hu, na.rm = TRUE), 0), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_land = sum(sqkm_land)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/sqkm_land, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0)) %>%
  merge(AOI, by = 'statefp') %>%
  dplyr::select(rowid, conscat, bzone_m, buf_m, tot_pop, popden, sqkm_bz, pland, pwhite, white, pblack, black, pother, other, 
                platinx, latinx, propPOC, hu, mnhhinc, geometry) %>%
  st_as_sf()
