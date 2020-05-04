rm(list=ls())

library(sf)
library(tidyverse)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

#### Import Data ####
## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1/lc_tier1.shp")) %>%
  st_transform(4326)

## import beneficiary zones data
bzone <- st_read(file.path(datadir, 'cabz.geojson')) %>%
  filter(buf_m == 320 & bzone_m == 16000) %>%
  st_make_valid() %>%
  st_union()

## import cons buf zones w demg data
buf <- st_read(file.path(datadir, 'cl_buf_demg.geojson')) %>%
  mutate(region = as.character(conscat), medhhinc = emedhhinc) %>%
  filter(buf_m == 320) %>%
  mutate(zblack = (black - mean(.$black))/sd(.$black),
         zmedhhinc = (medhhinc - mean(.$medhhinc))/sd(.$medhhinc)) %>%
  select(region, zblack, zmedhhinc, pblack, medhhinc) %>%
  st_set_geometry(NULL)

## import block group data
bg <- st_read(file.path(datadir, 'bg_demg.geojson')) %>%
  filter(st_intersects(t1, ., sparse = F)) %>%
  mutate(popden = total/sqkm_bg, pblack = black/total, region = 'Lowcountry') %>%
  mutate(zblack = (black - mean(black))/sd(black),
         zmedhhinc = (medhhinc - mean(medhhinc, na.rm=T))/sd(medhhinc, na.rm=T)) %>%
  select(region, zblack, zmedhhinc, pblack, medhhinc) %>%
  st_set_geometry(NULL)

df <- rbind(buf, bg) %>%
  filter(region != 'Lowcountry')

ggplot(df, aes(pblack, medhhinc, color = region)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = 'lm') + 
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0)
