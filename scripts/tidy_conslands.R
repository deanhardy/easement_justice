################################################
## PURPOSE: This script wrangles conservation land data from three sources 
##     (PADUS, NCED, & SC-TNC (proprietary)) into one dataframe
## BY: Dean Hardy
################################################

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

lc_tier1 <- st_read(file.path(datadir, "lc_tier1.shp")) %>%
  st_transform(crs = alb) %>%
  mutate(area = st_area(geometry)*3.86102e-7)

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
         mgmttype = eholdtype) %>%
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT'), 'Public',
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
  mutate(conscat = ifelse(owntype %in% c('DESG', 'DIST', 'FED', 'LOC', 'STAT', 'JNT'), 'Public',
                          ifelse(owntype %in% c('NGO', 'PVT'), 'Private', NA)))

## combine tidy source data and make geometry valid
dat <- rbind(nced, padus, tnc) %>%
  st_make_valid() %>%
  filter(ecorg_tier == 1 & state %in% c('GA', 'SC') & !is.na(owntype))

## explore data by mgmt and ownership
# table(dat$source, dat$conscat)
# table(dat$source, dat$owntype)

## reassembles single-part reserves in multi-part units (not spatially, but for descriptive stats)
dat2 <- dat %>%
  group_by(source, orig_id) %>%
  summarise(state = first(state), owntype = first(owntype), mgmttype = first(mgmttype),
            management = first(management), sitename = first(sitename),
            acres = sum(acres), gap = first(gap), purpose = first(purpose),
            ecorg_tier = first(ecorg_tier), conscat = first(conscat))

## summary descriptive stats
df_sum <- dat2 %>%
  st_drop_geometry() %>%
  group_by(source, conscat, state) %>%
  dplyr::summarise(count = n(), acres = sum(round(acres,0)))
df_sum

write.csv(df_sum, file.path(datadir, 'cons-lands-descriptive-stats.csv'))

## exploring the data
# table(dat2$source, dat2$conscat)
# table(dat2$source, dat2$owntype)
# table(dat2$source, dat2$state)
## qtm(dat, fill = 'conscat')

## export just lowcountry data
dat %>% st_write(file.path(datadir, 'cons_lands.shp'), driver = 'ESRI Shapefile', append = FALSE)
