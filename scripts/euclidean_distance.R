rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)


##############################################################
## data import and prepping
##############################################################

## define acs year and region for census data import
yr <- 2010
# cnty <- c("")
# ST <- c("Georgia", "South Carolina")

## import NCED data for low country region in SC & GA
nced <- st_read("data/nced_lc.shp")

## define acs variables of interest
acs_vars <- c(white = "B03002_003E", black = "B03002_004E", 
               native_american = "B03002_005E", asian = "B03002_006E", 
               hawaiian = "B03002_007E", other = "B03002_008E", 
               multiracial = "B03002_009E", latinx = "B03002_012E")

## define decennial variables of interest
dec_vars <- c(white = "P0050003", black = "P0050004", 
              native_american = "P0050005", asian = "P0050006", 
              hawaiian = "P0050007", other = "P0050008", 
              multiracial = "P0050009", nonlatinx = "P0050002",
              total = "P0050001")
  
## import area of interest data
ga <- get_decennial(geography = "block group", 
              variables = dec_vars,
              state = "Georgia",
              year = yr,
              output = 'wide',
              geometry = TRUE) %>%
  mutate(latinx = total - nonlatinx) %>%
  select(-nonlatinx)

sc <- get_decennial(geography = "block group", 
                    variables = dec_vars,
                    state = "South Carolina",
                    year = yr,
                    output = 'wide',
                    geometry = TRUE) %>%
  mutate(latinx = total - nonlatinx) %>%
  select(-nonlatinx)

bg <- rbind(ga, sc) %>%
  mutate(prop_POC = 1 - (white/total))
# bg <- st_zm(bg) ## drop "Z" data

## export census data
st_write(bg, "data/bg_data.shp", driver = 'GEOJson')

tm_shape(bg) + 
  tm_fill('prop_POC', palette = "Purples")
