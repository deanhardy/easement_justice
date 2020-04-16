## HEADER START ----------------------------------------------
##
## PURPOSE: data prep for sharing
##
## Author: Dean Hardy
##
## HEADER END ------------------------------------------------

rm(list=ls())

library(tidyverse)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

st <- read.csv(file.path(datadir, 'st_data.csv'))[-1] %>%
  filter(GEOID == 13 | GEOID == 45) %>%
  mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total, 
         pother = (native_american+asian+hawaiian+other+multiracial)/total, 
         propPOC = (total-white)/total,
         cat = 'State') %>%
  rename(tot_pop = total, statefp = GEOID) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)
  
cabz <- read.csv(file.path(datadir, 'cabz_data.csv'))
lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv'))

## import data
st <- read.csv(file.path(datadir, 'st_data.csv')) %>%
  filter(GEOID == 13 | GEOID == 45) %>%
  mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total, 
         pother = (native_american+asian+hawaiian+other+multiracial)/total, 
         propPOC = (total-white)/total,
         cat = 'State') %>%
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

lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  rename(medhhinc = emedhhinc) %>%
  mutate(cat = 'Lowcountry') %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)