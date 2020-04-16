################################################################################################
## PURPOSE: This script unions and buffers conservation land data
## BY: Dean Hardy
## needs much work, just cut and paste from cons_lands_prep and top of cons_lands_buffer_by_type
################################################################################################

rm(list=ls())

library(tidyverse)
library(sf)

## define variables
DIST = 16000 ## distance (m) of buffer zones
PERC = c(0.005, 0.01, 0.05) ## percent of buffer zone for unioning reserves
pub_buf <- NULL
pvt_buf <- NULL

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/cons_lands')

#######################################################################
## eliminate overlapping polygons among three source data sets
## for regression analysis 
#######################################################################

## examine intersection of NCED & TNC data sets
ncedxtnc <- st_intersection(nced, tnc) %>%
  mutate(acres_nced_tnc = acres - acres.1,
         acres_in_tnc = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  mutate(prop_in_tnc = acres_in_tnc/acres)
hist(ncedxtnc$prop_in_tnc)

## select observations with high overlap & filter original data to delete overlapping observations
# t <- filter(ncedxtnc, prop_in_tnc >=0.9 & abs(acres_nced_tnc) < 1)
tnc_del <- filter(ncedxtnc, prop_in_tnc >= 0.8)
tnc2 <- tnc %>% filter(!(id %in% tnc_del$id.1))

## examine intersection of NCED and PADUS data sets
ncedxpadus <- st_intersection(nced, padus) %>%
  mutate(acres_nced_padus = acres - acres.1,
         acres_in_padus = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  mutate(prop_in_padus = acres_in_padus/acres)
hist(ncedxpadus$prop_in_padus)

## select observations with high overlap & filter original data to delete overlapping observations
padus_del <- filter(ncedxpadus, prop_in_padus >= 0.8)
padus2 <- padus %>% filter(!(id %in% padus_del$id.1))

## examine intersection of PADUS & TNC data sets
padusxtnc <- st_intersection(padus2, tnc2) %>%
  mutate(acres_padus_tnc = acres - acres.1,
         acres_in_tnc = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  mutate(prop_in_tnc = acres_in_tnc/acres)
hist(padusxtnc$prop_in_tnc)

## select observations with high overlap & filter original data to delete overlapping observations
tnc2_del <- filter(padusxtnc, prop_in_tnc >= 0.8)
tnc3 <- tnc2 %>% filter(!(id %in% tnc2_del$id.1))

## combine filtered data
dat2 <- rbind(nced, padus2, tnc3)

# dat2_prv <- dat2 %>%
#   filter(ecorg_tier ==1 & conscat == 'Private') %>%
#   st_snap(., dat2, 50)

## Tidying feature geomotries with sf
## https://www.r-spatial.org/r/2017/03/19/invalid.html

## repair geometry and check validity
valid = st_is_valid(dat2)
table(valid)["FALSE"]
dat2_v <- st_make_valid(dat2)

dat2_f <- dat2 %>%
  filter(valid == 'TRUE')

## filter to lowcountry and union by conscat
dat3 <- dat2_f %>% 
  filter(ecorg_tier ==1) %>%
  split(.$conscat) %>% 
  lapply(st_union) %>% 
  do.call(c, .) %>% # bind the list element to a single sfc
  st_cast("MULTIPOLYGON") # mapview doesn't like GEOMETRY -> cast to MULTIPOLYGON
mapview(dat3)

dat4 <- dat2_f %>%
  group_by(conscat) %>%
  summarize()

## euclidean distance buffering
buf <- dat4 %>%
  st_buffer(dist = 16000) %>%
  mutate(sqkm_buf = as.numeric(st_area(geometry) / 1e6))

## keep receiving error, see sf issue 860 here: https://github.com/r-spatial/sf/issues/860
dat3 <- dat2_f %>%
  filter(ecorg_tier == 1) %>%
  st_cast('POLYGON') %>%
  st_set_precision(1e2) %>%
  st_buffer(100) %>%
  st_cast() %>%
  st_intersection()

test <- st_intersection(tnc)

## examine overlap within data set
# dat3 <- st_intersection(dat2, dat2) %>%
#   mutate(acres_nced_dat2 = acres - acres.1,
#          acres_in_dat2 = as.numeric(st_area(geometry) * 0.00024710538)) %>%
#   mutate(prop_in_tnc = acres_in_dat2/acres)
# hist(dat2$prop_in_dat2)

## select observations with high overlap & filter original data to delete overlapping observations
# t <- filter(ncedxtnc, prop_in_tnc >=0.9 & abs(acres_nced_tnc) < 1)
# dat2_del <- filter(tncxtnc, prop_in_dat2 >= 0.9)
# tnc2 <- tnc %>% filter(!(id %in% tnc_del$id.1))

## export cons lands data
dat2 %>% st_transform(crs = 4326) %>%
  filter(ecorg_tier == 1) %>%
  st_write(file.path(datadir, 'cons_lands.shp'), driver = 'ESRI Shapefile', delete_dsn = TRUE)

## export cons lands data
dat2 %>% st_transform(crs = 4326) %>%
  st_write(file.path(datadir, 'cons_lands.geojson'), driver = 'geojson', delete_dsn = TRUE)

## filtering the data sets
# tnc2 <- filter(tnc, conscat == 'Private')
# nced2 <- filter(nced, conscat == 'Private')
# padus2 <- filter(padus, conscat == 'Public')
# dat2 <- rbind(nced2, padus2, tnc2)
# 
# dat3 <- dat2 %>% filter(ecorg_tier == 1)
# table(dat3$source, dat3$conscat)

# ggplot() + 
#   geom_sf(data = tnc, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = TRUE) + 
#   geom_sf(data = padus, aes(color = NULL, fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE) + 
#   geom_sf(data = nced, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE)