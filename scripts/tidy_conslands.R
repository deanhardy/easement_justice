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

## combine tidy source data and make geometry valid
dat <- rbind(nced, padus, tnc) %>%
  st_make_valid()

## summary descriptive stats
df_sum <- dat %>%
  st_drop_geometry() %>%
  filter(ecorg_tier == 1 & state %in% c('GA', 'SC')) %>%
  mutate(management = as.character(management)) %>%
  group_by(source, conscat, state) %>%
  dplyr::summarise(count = n(), acres = sum(round(acres,0)))
df_sum

write.csv(df_sum, file.path(datadir, 'cons-lands-descriptive-stats.csv'))

## exploring the data
dat2<- dat %>% filter(ecorg_tier == 1) 
table(dat2$source, dat2$conscat)
1# qtm(dat, fill = 'conscat')

## export just lowcountry data
dat %>% filter(ecorg_tier == 1) %>%
  st_write(file.path(datadir, 'cons_lands.shp'), driver = 'ESRI Shapefile')
