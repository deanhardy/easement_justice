## HEADER START ----------------------------------------------
##
## PURPOSE: this script analyzes demographics (downloaded elsewhere) for an area of interest (AOI) using
## proportional area adjustment method
##
## Author: Dean Hardy
##
## HEADER END ------------------------------------------------

rm(list=ls())

library(tidyverse)
library(sf)
library(lwgeom)
library(tidycensus)
options(tigris_use_cache = TRUE)


###### Define variables and import data #######
## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## read in AOI
AOI <- st_read(file.path(datadir, 'lc_tier1/lc_tier1.shp'), stringsAsFactors = FALSE) %>%
  st_transform(crs = alb)

## import census data and filter to AOI states
bg <- st_read(file.path(datadir, "bg_demg.geojson"), stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  mutate(statefp = ifelse(STATEFP == 37, 45,
                          ifelse(STATEFP == 12, 13, 
                                 ifelse(STATEFP == 1, 13, STATEFP))))%>%
  filter(statefp != 'NA' & STATEFP %in% c(13, 45)) %>%
  st_transform(crs = alb)

## median HH income data
gm <- read.csv(file.path(datadir, 'gm_data.csv'), stringsAsFactors = FALSE)

  
  # st_read(file.path(datadir, "gm_data.geojson"), stringsAsFactors = FALSE) %>%
  # mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  # mutate(statefp = ifelse(STATEFP == 37, 45,
  #                         ifelse(STATEFP == 12, 13, 
  #                                ifelse(STATEFP == 1, 13, STATEFP))))%>%
  # filter(statefp != 'NA' & STATEFP %in% c(13, 45)) %>%
  # st_transform(crs = alb)


###### Proportional area adjustment method ######
## import state geometry
library(tigris)
st <- states() %>% st_as_sf() %>%
  filter(STATEFP == 45 | STATEFP == 13) %>%
  st_transform(alb) %>%
  select(STATEFP, geometry)

## intersection between state geometry and AOI
intAOI <- st_intersection(st, AOI) 

## intersection between block groups and state-bounded AOI
int <- as_tibble(st_intersection(intAOI, bg))

## proportional area adjustment/allocation method
percBGinAOI <- int %>%
  mutate(sqkm_bginAOI = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginAOI = (sqkm_bginAOI/sqkm_bg), sqkm_land = ALAND/ 1e6)

## save percBGinAOI data to use in median HH income estimation (see below)
bg_for_emed <- percBGinAOI %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(statefp, GEOID, perc_bginAOI)


###### Demographic analysis of AOI ######
AOI_geog <- percBGinAOI %>%
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
  merge(AOI) %>%
  dplyr::select(statefp, tot_pop, popden, pwhite, white, pblack, black, pother, other, 
                platinx, latinx, propPOC, hu, mnhhinc, geometry) %>%
  st_as_sf()


###### Estimate median household incomes within AOI ######

## this section of code calculates an estimated median from grouped (aka binned) household income data 
## within an area that overlaps several block groups

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html
## B modified to account for when median group is the 0-9999 bin
GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}

bg2 <- gm %>%
  mutate(statefp = as.numeric(str_extract(GEOID, "^.{2}"))) %>%
  left_join(bg_for_emed, by = "statefp") %>%
  filter(perc_bginAOI != 'NA') %>%
  mutate(eHH = households * perc_bginAOI) %>%
  group_by(statefp, variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## import gmedian estimates for hh income
emed <- bg2 %>%
  rename(emedhhinc = gmedian) %>%
  mutate(emedhhinc = round(emedhhinc, 0))

## merge emedian hh income with other demographic data
df <- AOI_geog %>% 
  merge(emed, by = "statefp") %>%
  st_transform(4326) 

###### Export ONLY attribute data ######
df %>%
  st_set_geometry(NULL) %>%
  write.csv(file.path(datadir, 'lowcountry-by-state_data.csv'), row.names = FALSE)

