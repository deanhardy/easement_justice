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
  mutate(urban = if_else(urban == 'yes', 1, 0),
         statefp = as.character(statefp)) %>%
  select(cat, statefp, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, statefp) %>%
  summarise(se_sqkm = sd(sqkm_bz, na.rm = T)/sqrt(length(sqkm_bz[!is.na(sqkm_bz)])),
            se_tot_pop = sd(tot_pop, na.rm = T)/sqrt(length(tot_pop[!is.na(tot_pop)])),
            se_popden = sd(popden, na.rm = T)/sqrt(length(popden[!is.na(popden)])),
            se_pwhite = sd(pwhite, na.rm = T)/sqrt(length(pwhite[!is.na(pwhite)])),
            se_pblack = sd(pblack, na.rm = T)/sqrt(length(pblack[!is.na(pblack)])),
            se_pother = sd(pother, na.rm = T)/sqrt(length(pother[!is.na(pother)])),
            se_platinx = sd(platinx, na.rm = T)/sqrt(length(platinx[!is.na(platinx)])),
            se_propPOC = sd(propPOC, na.rm = T)/sqrt(length(propPOC[!is.na(propPOC)])),
            se_medhhinc = sd(medhhinc, na.rm = T)/sqrt(length(medhhinc[!is.na(medhhinc)])),
            sqkm = mean(sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), 
            pother = mean(pother), platinx = mean(platinx), propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  # summarise(sqkm = mean (sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
  #           propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## GA and SC reserve data by urban
cl_demg_urb <- read.csv(file.path(datadir, 'cl_buf_demg_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  mutate(statefp = if_else(urban == 'yes', 'Urban', 'Rural'), urban = if_else(urban == 'yes', 1, 0)) %>%
  select(cat, statefp, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, statefp) %>%
  summarise(se_sqkm = sd(sqkm_bz, na.rm = T)/sqrt(length(sqkm_bz[!is.na(sqkm_bz)])),
            se_tot_pop = sd(tot_pop, na.rm = T)/sqrt(length(tot_pop[!is.na(tot_pop)])),
            se_popden = sd(popden, na.rm = T)/sqrt(length(popden[!is.na(popden)])),
            se_pwhite = sd(pwhite, na.rm = T)/sqrt(length(pwhite[!is.na(pwhite)])),
            se_pblack = sd(pblack, na.rm = T)/sqrt(length(pblack[!is.na(pblack)])),
            se_pother = sd(pother, na.rm = T)/sqrt(length(pother[!is.na(pother)])),
            se_platinx = sd(platinx, na.rm = T)/sqrt(length(platinx[!is.na(platinx)])),
            se_propPOC = sd(propPOC, na.rm = T)/sqrt(length(propPOC[!is.na(propPOC)])),
            se_medhhinc = sd(medhhinc, na.rm = T)/sqrt(length(medhhinc[!is.na(medhhinc)])),
            sqkm = mean(sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), 
            pother = mean(pother), platinx = mean(platinx), propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  # summarise(sqkm = mean (sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
  #           propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## combine GA and SC reserve data
cl_demg_entire <- read.csv(file.path(datadir, 'cl_buf_demg_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  mutate(urban = if_else(urban == 'yes', 1, 0)) %>%
  select(cat, statefp, sqkm_bz, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat) %>%
  summarise(statefp = 'Lowcountry',
    se_sqkm = sd(sqkm_bz, na.rm = T)/sqrt(length(sqkm_bz[!is.na(sqkm_bz)])),
            se_tot_pop = sd(tot_pop, na.rm = T)/sqrt(length(tot_pop[!is.na(tot_pop)])),
            se_popden = sd(popden, na.rm = T)/sqrt(length(popden[!is.na(popden)])),
            se_pwhite = sd(pwhite, na.rm = T)/sqrt(length(pwhite[!is.na(pwhite)])),
            se_pblack = sd(pblack, na.rm = T)/sqrt(length(pblack[!is.na(pblack)])),
            se_pother = sd(pother, na.rm = T)/sqrt(length(pother[!is.na(pother)])),
            se_platinx = sd(platinx, na.rm = T)/sqrt(length(platinx[!is.na(platinx)])),
            se_propPOC = sd(propPOC, na.rm = T)/sqrt(length(propPOC[!is.na(propPOC)])),
            se_medhhinc = sd(medhhinc, na.rm = T)/sqrt(length(medhhinc[!is.na(medhhinc)])),
            sqkm = mean(sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = n(), pwhite = mean(pwhite), pblack = mean(pblack), 
            pother = mean(pother), platinx = mean(platinx), propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  # summarise(statefp = 'Lowcountry', sqkm = mean(sqkm_bz), tot_pop = mean(tot_pop), popden = mean(popden), urban = sum(urban), pwhite = mean(pwhite), 
  #           pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
  #           propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## GA and SC lowcountry data by state
lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  rename(medhhinc = emedhhinc, sqkm = sqkm_land) %>%
  mutate(cat = 'Lowcountry', urban = NA, statefp = as.character(statefp)) %>%
  select(cat, statefp, sqkm, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc)

## combine GA and SC lowcountry data
lc_entire <- read.csv(file.path(datadir, 'lowcountry_data.csv')) %>%
  rename(medhhinc = emedhhinc, sqkm = sqkm_land) %>%
  mutate(cat = 'Lowcountry', statefp = 'Lowcountry', urban = NA) %>%
  select(cat, statefp, sqkm, tot_pop, popden, urban, pwhite, pblack, pother, platinx, propPOC, medhhinc)

df2 <- bind_rows(cl_demg, cl_demg_entire, lc, lc_entire, cl_demg_urb) %>%
  mutate_if(is.numeric, round, 2)

df3 <- df2 %>%
  as.data.table() %>%
  select(cat:medhhinc) %>%
  mutate(region = if_else(statefp == 45, 'South Carolina', 
                         if_else(statefp == 13, 'Georgia', statefp)),
         sqkm = round(sqkm, 0),
         tot_pop = round(tot_pop, 0),
         popden = round(popden, 0),
         medhhinc = round(medhhinc, 0),
         se_sqkm = round(se_sqkm, 0),
         se_tot_pop = round(se_tot_pop, 0),
         se_popden = round(se_popden, 0),
         se_medhhinc = round(se_medhhinc, 0)) %>%
  select(-statefp)

df3[5:9] <- df3[5:9] * 100
df3[15:19] <- df3[15:19] * 100

write.csv(df3, file.path(datadir, 'regional-comparison.csv'))

