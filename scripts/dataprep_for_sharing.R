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


cabz <- read.csv(file.path(datadir, 'cabz_data.csv'))
lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv'))

## import data
cabz <- read.csv(file.path(datadir, 'cabz_data.csv'))[-1] %>%
  rename(cat = conscat, sqkm = sqkm_bz, medhhinc = emedhhinc) %>%
  select(cat, statefp, bzone_m:pwhite, pblack, pother, platinx, propPOC, hu, medhhinc)

lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  mutate(cat = 'Lowcountry', bzone_m = NA, buf_m = NA, sqkm = NA, pland = NA) %>%
  rename(medhhinc = emedhhinc) %>%
  select(cat, statefp, bzone_m, buf_m, sqkm, pland, tot_pop:pwhite, pblack, pother, platinx, propPOC, hu, medhhinc)

df <- rbind(cabz, lc)

st <- read.csv(file.path(datadir, 'st_data.csv'))[-1] %>%
  filter(GEOID == 13 | GEOID == 45) %>%
  mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total,
         pother = (native_american+asian+hawaiian+other+multiracial)/total,
         propPOC = (total-white)/total,
         cat = 'State',
         bzone_m = NA, buf_m = NA, sqkm = NA, pland = NA, popden = NA) %>%
  rename(tot_pop = total, statefp = GEOID) %>%
  select(cat, statefp, bzone_m, buf_m, sqkm, pland, tot_pop, popden, pwhite, pblack, pother, platinx, propPOC, hu, medhhinc)

df2 <- rbind(df, st)

write.csv(df2, file.path(datadir, 'easement_ej_data.csv'))
