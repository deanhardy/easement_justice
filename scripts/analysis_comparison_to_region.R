## HEADER START ----------------------------------------------
##
## PURPOSE: this script compares block group demographics (data downloaded elsewhere) between AOIs.
## In this case, between SC and GA state, lowcountry of SC and GA, and conservation area beneficiary zones (CABZ)
##
## Author: Dean Hardy
##
## HEADER END ------------------------------------------------

rm(list=ls())

library(tidyverse)
library(data.table)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## import data
# st <- read.csv(file.path(datadir, 'st_data.csv')) %>%
#   filter(GEOID == 13 | GEOID == 45) %>%
#   mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total, 
#          pother = (native_american+asian+hawaiian+other+multiracial)/total, 
#          propPOC = (total-white)/total,
#          cat = 'State') %>%
#   rename(tot_pop = total, statefp = GEOID) %>%
#   select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)

## GA and SC reserve data by state
cl_demg <- read.csv(file.path(datadir, 'cl_buf_demg_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  mutate(urban = if_else(urban == 'yes', 1, 0)) %>%
  select(cat, statefp, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, statefp) %>%
  summarise(sqkm = mean (sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = sum(urban), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc), ) %>%
  as.data.frame()

## GA and SC reserve data by urban
cl_demg_urb <- read.csv(file.path(datadir, 'cl_buf_demg_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  mutate(urban_cat = urban, urban = if_else(urban == 'yes', 1, 0)) %>%
  select(cat, urban_cat, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, urban_cat) %>%
  summarise(sqkm = mean (sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc), ) %>%
  as.data.frame()

## combine GA and SC reserve data
cl_demg_entire <- read.csv(file.path(datadir, 'cl_buf_demg_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  mutate(urban = if_else(urban == 'yes', 1, 0)) %>%
  select(cat, statefp, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat) %>%
  summarise(statefp = 'Lowcountry', sqkm = mean(sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = sum(urban), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## GA and SC lowcountry data by state
lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  rename(medhhinc = emedhhinc, sqkm = sqkm_land) %>%
  mutate(cat = 'Lowcountry', urban = NA) %>%
  select(cat, statefp, sqkm, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc)

## combine GA and SC lowcountry data
lc_entire <- read.csv(file.path(datadir, 'lowcountry_data.csv')) %>%
  rename(medhhinc = emedhhinc, sqkm = sqkm_land) %>%
  mutate(cat = 'Lowcountry', statefp = 'Lowcountry', urban = NA) %>%
  select(cat, statefp, sqkm, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc)


df2 <- rbind(cl_demg, cl_demg_entire)

df3 <- rbind(df2, lc)

df4 <- rbind(df3, lc_entire)

df5 <- rbind(df4, cl_demg_urb)

df5 <- df4 %>%
  as.data.table() %>%
  select(cat:medhhinc) %>%
  mutate(region = if_else(statefp == 45, 'South Carolina', 
                         if_else(statefp == 13, 'Georgia', 'Lowcountry')),
         sqkm = round(sqkm, 0),
         tot_pop = round(tot_pop, 0),
         popden = round(popden, 0),
         pwhite = round(pwhite, 2),
         pblack = round(pblack, 2),
         platinx = round(platinx, 2),
         pother = round(pother, 2),
         propPOC = round(propPOC, 2),
         medhhinc = round(medhhinc, 0)) %>%
  select(-statefp)

write.csv(df5, file.path(datadir, 'regional-comparison.csv'))

