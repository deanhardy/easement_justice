################################################
## PURPOSE: this script downloads spatial census data on race, income, housing
## from several states in the lowcountry region of the US Southeast
## BY: Dean Hardy
################################################
 
rm(list=ls())

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## define variables
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
bg <- NULL
st <- NULL
YR <- 2016
ST <- c('AL', 'GA', 'FL', 'SC', 'NC')
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")
 
# all_vars <- load_variables(2016, 'acs5', cache = TRUE)
# ## define decennial variables of interest
# dec_vars <- c(white = "P0050003", black = "P0050004",
#               native_american = "P0050005", asian = "P0050006",
#               hawaiian = "P0050007", other = "P0050008",
#               multiracial = "P0050009", nonlatinx = "P0050002",
#               total = "P0050001")

# ## grab counties to create character vector for data grab
# ## necessary for block data grab, but not for block groups
# library(tigris)
# ga_cnty <- counties('georgia', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) %>%
#   select('COUNTYFP')
# 
# sc_cnty <- counties('south carolina', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) #%>%
#   select('COUNTYFP')

## download census variables for selected states
for(i in 1:length(ST)) {
  OUT <- get_acs(geography = "state",
                 variables = var,
                 state = ST[[i]],
                 year = YR,
                 output = 'wide') %>%
    data.frame()  
  st <- rbind(OUT, st)
}

## download census variables of block groups for selected states
for(i in 1:length(ST)) {
  OUT <- get_acs(geography = "block group",
              variables = var,
              state = ST[[i]],
              year = YR,
              output = 'wide',
              geometry = TRUE,
              keep_geo_vars = TRUE) %>%
    data.frame()  
    bg <- rbind(OUT, bg)
}

## cleanup and export for use in comparison of state level numbers to CABZ numbers
st2 <- st %>%
  dplyr::select(GEOID, NAME, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, medhhinc, agghhinc, hu)
write.csv(st2, file.path(datadir, 'st_data.csv'))

bg2 <- bg %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc)

## export census data
bg2 %>% st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_data.geojson"), driver = 'geojson', delete_dsn = TRUE)

## export census data as centroids
bg2 %>% 
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_data_cntrd.geojson"), driver = 'geojson', delete_dsn = TRUE)
