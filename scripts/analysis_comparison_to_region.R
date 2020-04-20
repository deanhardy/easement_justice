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
st <- read.csv(file.path(datadir, 'st_data.csv')) %>%
  filter(GEOID == 13 | GEOID == 45) %>%
  mutate(pwhite = white/total, pblack = black/total, platinx = latinx/total, 
         pother = (native_american+asian+hawaiian+other+multiracial)/total, 
         propPOC = (total-white)/total,
         cat = 'State') %>%
  rename(tot_pop = total, statefp = GEOID) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)

## GA and SC reserve data by state
cabz <- read.csv(file.path(datadir, 'cabz_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat, statefp) %>%
  summarise(tot_pop = mean(tot_pop), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## GA and SC lowcountry data by state
lc <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  rename(medhhinc = emedhhinc) %>%
  mutate(cat = 'Lowcountry') %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)

## combine GA and SC reserve data
cabz_entire <- read.csv(file.path(datadir, 'cabz_data.csv')) %>%
  filter(buf_m == 320 & bzone_m == 16000 & statefp != 'NA') %>% 
  rename(cat = conscat, medhhinc = emedhhinc) %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc) %>%
  group_by(cat) %>%
  summarise(tot_pop = mean(tot_pop), pwhite = mean(pwhite), pblack = mean(pblack), pother = mean(pother), platinx = mean(platinx), 
            propPOC = mean(propPOC), medhhinc = mean(medhhinc)) %>%
  as.data.frame()

## combine GA and SC lowcountry data
lc_entire <- read.csv(file.path(datadir, 'lowcountry-by-state_data.csv')) %>%
  rename(medhhinc = emedhhinc) %>%
  mutate(cat = 'Lowcountry') %>%
  select(cat, statefp, tot_pop, pwhite, pblack, pother, platinx, propPOC, medhhinc)

df <- rbind(st, cabz)

df2 <- rbind(df, lc)

df3 <- df2 %>%
  as.data.table() %>%
  select(cat:medhhinc) %>%
  mutate(state = if_else(statefp == 45, 'South Carolina', 'Georgia'),
         tot_pop = round(tot_pop, 0),
         pwhite = round(pwhite, 2),
         pblack = round(pblack, 2),
         platinx = round(platinx, 2),
         pother = round(pother, 2),
         propPOC = round(propPOC, 2),
         medhhinc = round(medhhinc, 0)) %>%
  select(-statefp)

view(df3)

write.csv(df3, file.path(datadir, 'state-comparison.csv'))

