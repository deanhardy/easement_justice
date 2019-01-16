################################################
## PURPOSE: This script wrangles conservation land data from three sources 
##     (PADUS, NCED, & SC-TNC (proprietary)) into one dataframe
## BY: Dean Hardy
################################################

rm(list=ls())

library(tidyverse)
library(lwgeom)
library(sf)
library(fasterize)
library(raster)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/cons_lands')

## import protected SC-TNC for SC coastal plain region (tier 3)
## assuming NAs and unknowns are PRIVATE (need to revise later)
## DO NOT SHARE DATA
tnc <- st_read(file.path(datadir, "tnc.shp")) %>%
  st_transform(crs = utm) %>%
  mutate(id = 1:nrow(.), source = 'tnc', acres = as.numeric(st_area(geometry) * 0.00024710538), 
         purpose = PurposeCde, state = 'SC', gap = 'NA') %>%
  dplyr::select(id, OwnType, HoldType, EsmtHldr, SiteName, PubAccess, state,
                acres, gap, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(owntype = OwnType,
         mgmttype = HoldType,
         management = EsmtHldr,
         orig_id = ORIG_FID,
         sitename = SiteName,
         access = PubAccess)  %>%
  mutate(mgmttype = ifelse(mgmttype %in% c('County', 'Local Government'), 'LOC',
                           ifelse(mgmttype == 'Federal', 'FED',
                                  ifelse(mgmttype == 'NGO/Local Government', 'JNT', 
                                         ifelse(mgmttype == 'NGO', 'NGO', 'UNK'))))) %>%
  mutate(owntype = ifelse(owntype %in% c('County', 'Local Government', 'Municipality'), 'LOC',
                          ifelse(owntype %in% c('State', 'State Government'), 'STAT',
                                 ifelse(owntype == 'Federal', 'FED', 
                                        ifelse(owntype == 'NGO/Local Government', 'JNT',
                                               ifelse(owntype == 'Private', 'PVT',
                                                      ifelse(owntype == 'Regional Agency', 'DIST', 
                                                             ifelse(owntype == 'NGO', 'NGO', 'UNK')))))))) %>%
  mutate(access = ifelse(access %in% c('Closed', 'No', 'NO'),'XA', 
                          ifelse(access %in% c(NA, 363.138382858769), 'UK', 
                                 ifelse(access %in% c('Restricted', 'Limited', 'Limited Access'), 'RA',
                                        ifelse(access %in% c('Open', 'Public Access', 'Yes'), 'OA', access))))) %>%
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT'), 'Public',
                          ifelse(owntype %in% c(NA, 'NGO', 'PVT', 'UNK'), 'Private', NA)))

## convert to raster then back to polygon
# r <- raster(tnc, res = 10)
# tnc_r <- fasterize(tnc, r, field = 'id')
# tnc_p <- rasterToPolygons(tnc_r)

## import NCED data for coastal plain (lc tier 3) region in SC & GA
## assuming all unknowns are PUBLIC (need to manually edit later)
nced <- st_read(file.path(datadir, "nced.shp")) %>%
  st_transform(crs = utm) %>%
  mutate(id = 1:nrow(.), source = 'nced', acres = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  dplyr::select(id, owntype, eholdtype, esmthldr, sitename, pubaccess, state, 
                acres, gapcat, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(management = esmthldr, 
        gap = gapcat,
        access = pubaccess,
        orig_id = ORIG_FID,
        mgmttype = eholdtype)%>%
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT', 'UNK'), 'Public',
                          ifelse(owntype %in% c('NGO', 'PVT'), 'Private', NA)))

## import PAD-US data
## assuming unknowns are PUBLIC
padus <- st_read(file.path(datadir, "padus.shp")) %>%
  st_transform(crs = utm) %>%
  mutate(id = 1:nrow(.), source = 'padus', acres = as.numeric(st_area(geometry) * 0.00024710538), 
         purpose = 'NA') %>%
  dplyr::select(id, Own_Type, Mang_Type, Loc_Mang, Unit_Nm, Access, State_Nm,
                acres, GAP_Sts, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(owntype = Own_Type,
         state = State_Nm,
         sitename = Unit_Nm,
         management = Loc_Mang,
         mgmttype = Mang_Type,
         gap = GAP_Sts,
         access = Access,
         orig_id = ORIG_FID) %>%
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT', 'UNK'), 'Public',
                          ifelse(owntype %in% c('NGO', 'PVT'), 'Private', NA)))

# tnc_inter <- tnc %>%
#   st_make_valid() %>%
#   filter(ecorg_tier ==1) %>%
#   st_set_precision(1e2) %>%
#   st_buffer(100) %>%
#   st_intersection()

dat <- rbind(nced, padus, tnc) 

## exploring the data
table(dat$source, dat$conscat)


# st_write(dat, file.path(datadir, 'cons_lands.shp'), driver = 'ESRI Shapefile')
# st_write(dat, file.path(datadir, 'tnc_rc.shp'), driver = 'ESRI Shapefile')
# st_write(dat, file.path(datadir, 'nced_rc.shp'), driver = 'ESRI Shapefile')
# st_write(dat, file.path(datadir, 'padus_rc.shp'), driver = 'ESRI Shapefile')

options("scipen"=100, "digits"=4)
# options("scipen"=0, "digits"=7) ## default

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

  
    


