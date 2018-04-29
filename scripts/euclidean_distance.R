rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)


##############################################################
## data import and prepping
##############################################################

## define acs year and region for census data import
yr <- 2010
# cnty <- c("")
# ST <- c("Georgia", "South Carolina")

## import NCED data for low country region in SC & GA
nced <- st_read("data/nced_lc.shp") %>%
  # filter(owntype == 'PVT', 
  #        purpose %in% c('ENV', 'FOR', 'FARM', 'REC')) %>%
  # filter(gapcat == '2') %>%
  st_transform(4269)

## re-run if census variables need to be changed
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
#   mutate(latinx = total - nonlatinx) %>%
#   select(-nonlatinx)
# 
# sc <- get_decennial(geography = "block group", 
#                     variables = dec_vars,
#                     state = "South Carolina",
#                     year = yr,
#                     output = 'wide',
#                     geometry = TRUE) %>%
#   mutate(latinx = total - nonlatinx) %>%
#   select(-nonlatinx)
# 
# bg <- rbind(ga, sc) %>%
#   mutate(prop_POC = 1 - (white/total))
# 
# ## export census data
# st_write(bg, "data/bg_data.shp", driver = 'GEOJson')

## import census data
bg <- st_read("data/bg_data.shp")

fig <- tm_shape(bg) + 
  tm_fill('prop_POC', palette = "Greys",
          title = "People of Color") +
  tm_shape(nced) + 
  tm_fill('owntype', palette = "green", legend.show = FALSE) +
  tm_layout(title = "Lowcountry Private Conservation Easements (n = 1470)",
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


