## --------------------------------------------
## PURPOSE: This script wrangles conservation reserve data from three sources 
##     (PADUS, NCED, & SC-TNC (proprietary)) into one dataframe
## Conservative Approach == Cleaning data by removing all public reserves from SC-TNC and NCED and all private reserves from PADUS 
##    as well as all non-GA reserves from NCED w/ hopes of eliminating replication
## Author: Dr. Dean Hardy
## --------------------------------------------

rm(list=ls())

library(tidyverse)
library(lwgeom)
library(sf)
library(tmap)
options("scipen"=100, "digits"=4)
# options("scipen"=0, "digits"=7) ## default

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

lc_tier1 <- st_read(file.path(datadir, "lc_tier1/lc_tier1.shp")) %>%
  st_transform(crs = alb) %>%
  mutate(area = st_area(geometry)*3.86102e-7) %>%
  st_transform(4326)

## import protected SC-TNC for SC coastal plain region (tier 3)
## assuming NAs and unknowns are PRIVATE (need to revise later)
## DO NOT SHARE DATA

## clip tnc to lowcountry and then access NCED data restrictions
# tnc_lc <- st_read(file.path(datadir, "tnc/tnc.shp"), stringsAsFactors = F) %>%
#   st_transform(4326) %>%
#   filter(ecorg_tier == 1 & state %in% c('GA', 'SC') & !is.na(OwnType))
# 
#   filter(st_contains_properly(lc_tier1, ., sparse = F)) %>%
#   group_by(ORIG_FID, NCED) %>%
#   st_set_geometry(NULL) %>%
#   dplyr::select(NCED) %>% 
#   summarise(count = n())

tnc <- st_read(file.path(datadir, "tnc/tnc.shp"), stringsAsFactors = F) %>%
  st_transform(crs = utm) %>%
  mutate(id = 1:nrow(.), source = 'tnc', acres = as.numeric(st_area(geometry) * 0.00024710538), 
         purpose = 'NA', state = 'SC', gap = PurposeCde) %>%
  dplyr::select(id, OwnType, HoldType, EsmtHldr, SiteName, PubAccess, state,
                acres, gap, purpose, ORIG_FID, ecorg_tier, source, NCED, geometry) %>%
  rename(owntype = OwnType,
         mgmttype = HoldType,
         management = EsmtHldr,
         orig_id = ORIG_FID,
         sitename = SiteName,
         access = PubAccess) %>%
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
                          ifelse(owntype %in% c(NA, 'NGO', 'PVT', 'UNK'), 'Private', NA))) %>%
  filter(conscat == 'Private')

<<<<<<< HEAD
tnc_lc <- tnc %>%
  st_make_valid() %>%
  filter(ecorg_tier == 1 & state %in% c('GA', 'SC') & !is.na(owntype))

tnc_x_nced <- tnc_lc %>% 
  st_set_geometry(NULL) %>%
  group_by(orig_id) %>% 
  dplyr::select(NCED) %>% 
  summarise(nced = first(NCED)) %>%
  group_by(nced) %>%
  summarise(count = n())

# tnc_access <- tnc %>% 
#   st_set_geometry(NULL) %>%
#   dplyr::select(PubAccess) %>% 
#   group_by(PubAccess) %>% 
#   summarise(count = n())

=======
tnc_access <- tnc %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(access) %>% 
  group_by(access) %>% 
  summarise(count = n())

>>>>>>> f007b9383f45b7b25abe12dfe5243b85d3027408
## convert to raster then back to polygon
# r <- raster(tnc, res = 10)
# tnc_r <- fasterize(tnc, r, field = 'id')
# tnc_p <- rasterToPolygons(tnc_r)

## import NCED data for coastal plain (lc tier 3) region in SC & GA
nced <- st_read(file.path(datadir, "nced/nced.shp")) 

nced %>% st_geometry(NULL) %>% write.csv(file.path(datadir, 'nced.csv'))

nced <- nced %>%
  st_transform(crs = utm) %>%
  mutate(id = 1:nrow(.), source = 'nced', acres = as.numeric(st_area(geometry) * 0.00024710538)) %>%
  dplyr::select(id, owntype, eholdtype, esmthldr, sitename, pubaccess, state, 
                acres, gapcat, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(management = esmthldr, 
         gap = gapcat,
         access = pubaccess,
         orig_id = ORIG_FID,
         mgmttype = eholdtype) %>%
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT'), 'Public',
                          ifelse(owntype %in% c('NGO', 'PVT'), 'Private', NA))) %>%
  filter(conscat == 'Private' & state == 'GA')

## import PAD-US data
padus <- st_read(file.path(datadir, "padus/padus.shp")) %>%
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
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT'), 'Public',
                          ifelse(owntype %in% c('NGO', 'PVT'), 'Private', NA))) %>%
  filter(conscat == 'Public')

## combine tidy source data and make geometry valid
dat <- rbind(nced, padus, tnc) %>%
  st_make_valid() %>%
  filter(ecorg_tier == 1 & state %in% c('GA', 'SC') & !is.na(owntype))

# qtm(dat, fill = 'owntype')

## explore data by mgmt and ownership
# table(dat$source, dat$mgmttype)
# table(dat$source, dat$owntype)

## reassembles single-part reserves in multi-part units (not spatially, but for descriptive stats)
dat2 <- dat %>%
  group_by(source, orig_id) %>%
  summarise(state = first(state), owntype = first(owntype), mgmttype = first(mgmttype),
            management = first(management), sitename = first(sitename),
            acres = sum(acres), gap = first(gap), access = first(access), purpose = first(purpose),
            ecorg_tier = first(ecorg_tier), conscat = first(conscat))

dat2_access <- dat2 %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(access) %>% 
  group_by(source, access) %>% 
  summarise(count = n())

dat2_gap <- dat2 %>% 
  st_set_geometry(NULL) %>%
  dplyr::select(gap) %>% 
  group_by(source, gap) %>% 
  summarise(count = n())

ggplot(filter(dat2, conscat=='Private')) + 
  geom_boxplot(aes(conscat, acres))

dat2 %>% ungroup() %>%filter(conscat == 'Public') %>%
  summarise(n())

## summary descriptive stats
df_sum <- dat2 %>%
  st_drop_geometry() %>%
  group_by(source, conscat, state) %>%
  dplyr::summarise(count = n(), acres = sum(round(acres,0)))
df_sum

write.csv(df_sum, file.path(datadir, 'cl-stats.csv'))

## exploring the data
# table(dat2$source, dat2$conscat)
# table(dat2$source, dat2$owntype)
# table(dat2$source, dat2$state)
## qtm(dat, fill = 'conscat')

## export just lowcountry data
dat %>% st_transform(4326) %>% st_write(file.path(datadir, 'cl.geojson'), delete_dsn = TRUE)

