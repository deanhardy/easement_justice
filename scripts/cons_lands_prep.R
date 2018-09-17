rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

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

dat <- rbind(nced, padus, tnc)

dat_bx <- dat %>%
  filter(acres > 5 & ecorg_tier == 1)

png('figs/conscat_acres_boxplot.png', res = 150, units = 'in',
    height = 5, width = 5)
boxplot(acres ~ conscat, data = dat_bx, outline = F,
        ylab = 'Acres', xlab = 'Conservation Category')
mtext('*no outliers', side = 1, line = 4, adj = 1)
dev.off()

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

  
    


