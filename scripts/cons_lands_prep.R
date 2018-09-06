rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')
  
## import NCED data for coastal plain (lc tier 3) region in SC & GA
nced <- st_read(file.path(datadir, "nced.shp")) %>%
  st_transform(crs = utm) %>%
  filter(owntype == 'PVT') %>%
  mutate(source = 'nced')
  #        purpose %in% c('ENV', 'FOR', 'FARM', 'REC')) %>%
  # #  filter(gapcat %in% c('1','2')) %>%
  # mutate(type = "Easement") %>%
  # dplyr::select(type, state, sitename, esmthldr, gis_acres, gapcat, purpose, geometry) %>%
  # rename(management = esmthldr, 
  #        acres = gis_acres, 
  #        gap = gapcat)

## import PAD-US data
padus <- st_read(file.path(datadir, "padus.shp")) %>%
  st_transform(crs = utm) %>%
  filter(Own_Type != "PVT", Own_Type != "NGO") %>%
  mutate(source = 'padus')
  #        Category != "Easement") %>%
  # filter(GAP_Sts %in% c('1', '2')) %>%
  # mutate(purpose = 'NA') %>%
  # dplyr::select(d_Own_Type, State_Nm, Unit_Nm, d_Mang_Nam, GIS_Acres, GAP_Sts, purpose, geometry) %>%
  # rename(type = d_Own_Type, 
  #        state = State_Nm, 
  #        sitename = Unit_Nm, 
  #        management = d_Mang_Nam, 
  #        acres = GIS_Acres, 
  #        gap = GAP_Sts)

## import protected SC-TNC for SC coastal plain region (tier 3)
## DO NOT SHARE DATA
tnc <- st_read(file.path(datadir, "tnc.shp")) %>%
  st_transform(crs = utm) %>%
  filter(OwnType %in% c("Private", "NGO", "<NA>")) %>%
  mutate(source = 'tnc')

ggplot() + 
  geom_sf(data = tnc, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = TRUE) + 
  geom_sf(data = padus, aes(color = NULL, fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE) + 
  geom_sf(data = nced, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE)


