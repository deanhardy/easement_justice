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
st <- read.csv(file.path(datadir, 'st_data.csv')) %>%
  filter(GEOID == 13 | GEOID == 45) %>%
  mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total, 
         pother = (native_american+asian+hawaiian+other+multiracial)/total, 
         propPOC = (total-white)/total,
         cat = NAME) %>%
  rename(tot_pop = total, statefp = GEOID) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)
cabz <- read.csv(file.path(datadir, 'cabz_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, statefp) %>%
  summarise(tot_pop = mean(tot_pop), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

df <- rbind(st, cabz)

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
