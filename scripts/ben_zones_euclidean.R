rm(list=ls())

library(tidyverse)
library(sf)
library(tmap)
library(lwgeom)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

##############################################################
## data import
##############################################################

cons <- st_read(file.path(datadir, 'cons_lands.geojson')) %>%
  rowid_to_column() %>%
  st_transform(crs = alb)

## import census data
bg <- st_read(file.path(datadir, "bg_data.geojson")) %>%
  st_transform(crs = alb)

# density plot
ggplot(bg, aes(x = hawaiian)) +
  geom_density() +
  theme_bw()


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
percBGinBUF <- int %>%
  mutate(sqkm_bginbuf = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginbuf = (sqkm_bginbuf/sqkm_bg))

## write percBGinBUF data out to use in median HH income estimation
percBGinBUF %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(rowid, GEOID, perc_bginbuf) %>%
  saveRDS(file.path(datadir, 'percBGinBUF.rds'))

bz_geog <- percBGinBUF %>%
  mutate(tot_pop = total * perc_bginbuf,
         white = white * perc_bginbuf, 
         black = black * perc_bginbuf,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginbuf,
         #other = multiracial * perc_bginbuf,
         latinx = latinx * perc_bginbuf,
         hu = hu * perc_bginbuf,
         ALAND = ALAND * perc_bginbuf) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(rowid) %>%
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx), 
            hu = sum(hu, na.rm = TRUE), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_buf = mean(sqkm_buf), ALAND = sum(ALAND)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/ALAND, 2), propPOC = round(1 - pwhite, 2),
         mnhhinc = round(agghhinc/hu, 0), pland = round((ALAND * 0.000001)/sqkm_buf, 2)) %>%
  dplyr::select(rowid, tot_pop, popden, sqkm_buf, pland, pwhite, pblack, pother, platinx, propPOC, hu, mnhhinc) %>%
  merge(cons, by = 'rowid') %>%
  st_as_sf()

# density plot
ggplot(bz_geog, aes(x = pblack, group = conscat)) +
  geom_density(aes(color = conscat)) +
  theme_bw()

## import gmedian estimates for hh income
emed <- readRDS(file.path(datadir, 'gmedian.rds')) %>%
  rename(rowid = BUFID, emedhhinc = gmedian)

df <- bz_geog %>% 
  merge(emed, by = "rowid") %>%
  st_transform(4326)

st_write(df,file.path(datadir, 'bz_data.geojson'), driver = 'geojson', delete_layer = TRUE)

df %>%
  st_set_geometry(NULL) %>%
  filter(state %in% c('GA', 'SC')) %>%
  write.csv(file.path(datadir, 'cons_data.csv'), row.names = FALSE)





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
