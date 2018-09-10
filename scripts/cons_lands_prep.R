rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1.shp")) %>%
  st_transform(utm)
t2 <- st_read(file.path(datadir, "lc_tier2.shp")) %>%
  st_transform(utm)
t3 <- st_read(file.path(datadir, "lc_tier3.shp")) %>%
  st_transform(utm)


## import protected SC-TNC for SC coastal plain region (tier 3)
## DO NOT SHARE DATA
tnc <- st_read(file.path(datadir, "tnc.shp")) %>%
  st_transform(crs = utm) %>%
  # filter(OwnType %in% c("Private", "NGO", "<NA>")) %>%
  mutate(source = 'tnc', sqkm = as.numeric(st_area(geometry) / 1e6), 
         purpose = 'NA', state = 'SC', gap = 'NA') %>%
  dplyr::select(OwnType, HoldType, EsmtHldr, SiteName, PubAccess, state,
                sqkm, gap, PurposeCde, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(owntype = OwnType,
         mgmttype = HoldType,
         management = EsmtHldr,
         orig_id = ORIG_FID,
         sitename = SiteName,
         purpose = PurposeCde,
         access = PubAccess)  %>%
  mutate(access = ifelse(access %in% c('Closed', 'No', 'NO'),'XA', 
                          ifelse(access %in% c(NA, 363.138382858769), 'UK', 
                                 ifelse(access %in% c('Restricted', 'Limited', 'Limited Access'), 'RA',
                                        ifelse(access %in% c('Open', 'Public Access', 'Yes'), 'OA', NA)))))

## import NCED data for coastal plain (lc tier 3) region in SC & GA
nced <- st_read(file.path(datadir, "nced.shp")) %>%
  st_transform(crs = utm) %>%
  # filter(owntype %in% c('PVT', 'NGO')) %>%
  mutate(source = 'nced', sqkm = as.numeric(st_area(geometry) / 1e6)) %>%
  dplyr::select(owntype, eholdtype, esmthldr, sitename, pubaccess, state, 
                sqkm, gapcat, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(management = esmthldr, 
        gap = gapcat,
        access = pubaccess,
        orig_id = ORIG_FID,
        mgmttype = eholdtype)

## import PAD-US data
padus <- st_read(file.path(datadir, "padus.shp")) %>%
  st_transform(crs = utm) %>%
  # filter(Own_Type != "PVT", Own_Type != "NGO") %>%
  mutate(source = 'padus', sqkm = as.numeric(st_area(geometry) / 1e6), 
         purpose = 'NA') %>%
  dplyr::select(Own_Type, Mang_Type, Loc_Mang, Unit_Nm, Access, State_Nm,
                sqkm, GAP_Sts, purpose, ORIG_FID, ecorg_tier, source, geometry) %>%
  rename(owntype = Own_Type,
         state = State_Nm,
         sitename = Unit_Nm,
         management = Loc_Mang,
         mgmttype = Mang_Type,
         gap = GAP_Sts,
         access = Access,
         orig_id = ORIG_FID)

dat <- rbind(nced, padus, tnc)

st_write(dat, file.path(datadir, 'cons_lands.shp'), driver = 'ESRI Shapefile')

fig <- tm_shape(t3) + tm_fill(col = 'grey95') +
  tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(t1) + tm_borders(col = 'grey40') +
  tm_shape(dat) + 
  tm_fill(col = 'source', alpha = 0.5, palette = c('red', 'green', 'yellow'))

tiff('figures/conslands_by_source.tiff', compression = 'lzw', units = 'in',
     height = 5, width = 7, res = 300)
fig
dev.off()
  
  
# ggplot() + 
#   geom_sf(data = tnc, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = TRUE) + 
#   geom_sf(data = padus, aes(color = NULL, fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE) + 
#   geom_sf(data = nced, aes(fill = source), lwd = 0, alpha = 0.3, inherit.aes = FALSE)

# tm_shape(padus) +
#   tm_fill(col = 'blue', alpha = 0.5)+
#   tm_shape(nced) + 
#   tm_fill(col = 'yellow', alpha = 0.5) +
#   tm_shape(tnc) + 
#   tm_fill(col = 'red', alpha = 0.5)


  
    


