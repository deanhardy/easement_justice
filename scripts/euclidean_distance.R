rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)
library(lwgeom)

## Albers Conic Equal Area projection
## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## define acs year and region for census data import
yr <- 2010
# cnty <- c("")
# ST <- c("Georgia", "South Carolina")

##############################################################
## data import and prepping
##############################################################

## import NCED data for low country region in SC & GA
nced <- st_read("data/nced_lc.shp") %>%
  filter(owntype == 'PVT') %>%
  #        purpose %in% c('ENV', 'FOR', 'FARM', 'REC')) %>%
  # filter(gapcat == '2') # %>%
  st_transform(crs = alb)

# ## re-run if census variables need to be changed
# ## define acs variables of interest
# acs_vars <- c(white = "B03002_003E", black = "B03002_004E",
#                native_american = "B03002_005E", asian = "B03002_006E",
#                hawaiian = "B03002_007E", other = "B03002_008E",
#                multiracial = "B03002_009E", latinx = "B03002_012E")
# 
# ## define decennial variables of interest
# dec_vars <- c(white = "P0050003", black = "P0050004",
#               native_american = "P0050005", asian = "P0050006",
#               hawaiian = "P0050007", other = "P0050008",
#               multiracial = "P0050009", nonlatinx = "P0050002",
#               total = "P0050001")
# 
# ## import area of interest data
# ga <- get_decennial(geography = "block group",
#               variables = dec_vars,
#               state = "Georgia",
#               year = yr,
#               output = 'wide',
#               geometry = TRUE) %>%
#   st_transform(crs = alb) %>%
#   mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
#   select(-nonlatinx)
# 
# sc <- get_decennial(geography = "block group",
#                     variables = dec_vars,
#                     state = "South Carolina",
#                     year = yr,
#                     output = 'wide',
#                     geometry = TRUE) %>%
#   st_transform(crs = alb) %>%
#   mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
#   select(-nonlatinx)
# 
# bg <- rbind(ga, sc) %>%
#   mutate(prop_POC = 1 - (white/total))
# 
# ## export census data
# bg %>% st_transform(crs = 4326) %>%
# st_write("data/bg_data.geojson", driver = 'GEOJson')


## import census data
bg <- st_read("data/bg_data.geojson") %>%
  st_transform(crs = alb)

fig <- tm_shape(bg) + 
  tm_fill('prop_POC', palette = "Greys",
          title = "People of Color") +
  tm_shape(nced) + 
  tm_fill('owntype', palette = "green", legend.show = FALSE) +
  tm_layout(title = "Lowcountry Private Conservation Easements (n = 1196)",
            frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0) + 
  tm_compass(type = "arrow", size = 5, position = c(0.61, 0.09)) +
  tm_scale_bar(breaks = c(0,100), size = 1.2, position= c(0.57, 0.02)) +
  tm_legend(position = c(0.8, 0.04),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 1.2,
            legend.title.size = 1.5)
fig

tiff('figures/race_pvt_cons.tiff', res = 300, units = 'in',
     height = 9, width = 10, compression = 'lzw')
fig
dev.off()



###################################################
## apply proportional area adjustment to variables
## to assess count within buffer zones
###################################################

## euclidean distance buffering
buf <- nced %>%
  st_buffer(dist = 16000) %>%
  mutate(SqKM_BUF = as.numeric(st_area(geometry) / 1e6))

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(buf, bg))

## proportional area adjustment method
ncedbuf_bg <- int %>%
  mutate(SqKMBGinBUF = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(PercBGinBUF = (SqKMBGinBUF/SqKM_BG)) %>%
  mutate(total = total * PercBGinBUF,
         white = white * PercBGinBUF, 
         black = black * PercBGinBUF,
         native_american = native_american * PercBGinBUF,
         asian = asian * PercBGinBUF,
         hawaiian = hawaiian * PercBGinBUF,
         other = other * PercBGinBUF,
         multiracial = multiracial * PercBGinBUF,
         latinx = latinx * PercBGinBUF) %>%
  group_by(unique_id) %>%
  select(unique_id, total, white, black, native_american, asian, hawaiian, 
         other, multiracial, latinx, SqKM_BUF) %>%
  summarise(total = sum(total), white = sum(white), black = sum(black), 
            native_american = sum(native_american),
            asian = sum(asian), hawaiian = sum(hawaiian), other = sum(other),
            multiracial = sum(multiracial), latinx = sum(latinx),
            SqKM_BUF = mean(SqKM_BUF))





