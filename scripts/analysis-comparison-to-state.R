################################################
## PURPOSE: this script compares block group demographics aggregated to a study site
## to state level demographics (census data downloaded elsewhere)
## BY: Dean Hardy
################################################

rm(list=ls())

library(tidyverse)
library(data.table)
library(formattable)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## import data
st <- read.csv(file.path(datadir, 'st_data.csv'))
bg <- read.csv(file.path(datadir, 'cabz_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000) %>%
  group_by(conscat) %>%
  summarise(mean())

df %>%
  as.data.table() %>%
  select(tot_pop:emedhhinc) %>%
  select(-pland, -pblack, -pother, -platinx) %>%
  rename(., 
         'Area (km^2)' = sqkm_aoi.x,
         'Total Population' = tot_pop, 
         'Population Density (km^2)' = popden, 
         'White (%)' = pwhite,
         'People of Color (%)' = propPOC,
         'Housing Units (#)' = hu,
         'Mean Household Income ($)' = mnhhinc,
         'Estimated Median Household Income ($)' = emedhhinc) %>%
  formattable()
