rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
library(lwgeom)

## Albers Conic Equal Area projection
## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
# alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## NAD 83 UTM 17N
utm <- 2150



##############################################################
## data import and prepping
##############################################################

## import lowcountry region boundary
lc <- st_read("data/lowcountry.shp") %>%
  st_transform(utm)

## import NCED data for low country region in SC & GA
private <- st_read("data/nced_lc.shp") %>%
  filter(owntype == 'PVT', 
         purpose %in% c('ENV', 'FOR', 'FARM', 'REC')) %>%
#  filter(gapcat %in% c('1','2')) %>%
  mutate(type = "Easement") %>%
  st_transform(crs = utm) %>%
  dplyr::select(type, state, sitename, esmthldr, gis_acres, gapcat, purpose, geometry) %>%
  rename(management = esmthldr, 
         acres = gis_acres, 
         gap = gapcat)

## import PAD-US data for low country region in SC & GA
public <- st_read("data/padus_lc.shp") %>%
  filter(Own_Type %in% c("FED", "STAT"),
         Category != "Easement") %>%
  filter(GAP_Sts %in% c('1', '2')) %>%
  mutate(purpose = 'NA') %>%
  st_transform(crs = utm) %>%
  dplyr::select(d_Own_Type, State_Nm, Unit_Nm, d_Mang_Nam, GIS_Acres, GAP_Sts, purpose, geometry) %>%
  rename(type = d_Own_Type, 
         state = State_Nm, 
         sitename = Unit_Nm, 
         management = d_Mang_Nam, 
         acres = GIS_Acres, 
         gap = GAP_Sts)

# fed <- st_read("data/padus_lc.shp") %>%
#   filter(Own_Type == "FED",
#          Category != "Easement") %>%
#   filter(GAP_Sts %in% c('1', '2'))
# 
# state <- st_read("data/padus_lc.shp") %>%
#   filter(Own_Type == "STAT",
#          Category != "Easement") %>%
#   filter(GAP_Sts %in% c('1', '2'))

cons <- rbind(private, public) %>%
  rowid_to_column()

## import census data
bg <- st_read("data/bg_data.geojson") %>%
  st_transform(crs = utm)

# fig <- tm_shape(bg) +
#   tm_fill('prop_POC', palette = "Greys",
#           title = "People of Color") +
#   tm_shape(cons) +
#   tm_fill('type', legend.show = TRUE) +
#   tm_shape(lc) +
#   tm_borders(col = 'black') +
#   tm_layout(title = "Lowcountry Conservation Areas (n = 323)",
#             frame = FALSE,
#             outer.margins=c(0,0,0,0),
#             inner.margins=c(0,0,0,0), asp=0) +
#   tm_compass(type = "arrow", size = 5, position = c(0.61, 0.09)) +
#   tm_scale_bar(breaks = c(0,100), size = 1.2, position= c(0.57, 0.02)) +
#   tm_legend(position = c(0.8, 0.04),
#             bg.color = "white",
#             frame = TRUE,
#             legend.text.size = 1.2,
#             legend.title.size = 1.5)
# fig
# 
# tiff('figures/cons_race.tiff', res = 300, units = 'in',
#      height = 9, width = 10, compression = 'lzw')
# fig
# dev.off()



###################################################
## apply proportional area adjustment to variables
## to assess count within buffer zones
###################################################

## euclidean distance buffering
buf <- cons %>%
  st_buffer(dist = 16000) %>%
  mutate(sqkm_buf = as.numeric(st_area(geometry) / 1e6))

# st_centroid(buf) %>%
#   st_transform(4326) %>%
#   select(rowid) %>%
#   st_write('data/buf_cntrd.geojson', driver = 'geojson')
  
# buf %>% st_transform(4326) %>%
# st_write('data/ben_zones.geojson', driver = 'geojson')

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(buf, bg))

## proportional area adjustment/allocation method
bz_geog <- int %>%
  mutate(sqkm_bginbuf = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginbuf = (sqkm_bginbuf/sqkm_bg)) %>%
  mutate(tot_pop = total * perc_bginbuf,
         white = white * perc_bginbuf, 
         black = black * perc_bginbuf,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginbuf,
         latinx = latinx * perc_bginbuf,
         hu = hu * perc_bginbuf) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(rowid) %>%
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = sum(hu, na.rm = TRUE), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_buf = mean(sqkm_buf)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/sqkm_buf, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0)) %>%
  dplyr::select(rowid, tot_pop, popden, sqkm_buf, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc) %>%
  merge(cons, by = 'rowid') %>%
  st_as_sf()

df <- bz_geog %>% 
  st_transform(4326)

# st_write(df,'data/bz_data_erp.geojson', driver = 'geojson', delete_layer = TRUE)

df %>%
  st_set_geometry(NULL) %>%
  filter(state == c('GA', 'SC')) %>%
  write.csv('data/cons_data.csv', row.names = FALSE)
