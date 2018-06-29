## this script downloads spatial census data on race from several states
rm(list=ls())

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)

## define variables
## Albers Conic Equal Area projection
## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# nad83 <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
geometry = st_sfc(lapply(1:1, function(x) st_geometrycollection()))
bg <- st_sf(data.frame(geometry = geometry), crs = alb)
YR <- 2016
ST <- c('AL')
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E")
 
# ## define decennial variables of interest
# all_vars <- load_variables(2016, 'acs5', cache = TRUE)
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
  OUT <- get_acs(geography = "block group",
              variables = var,
              state = ST[[i]],
              county = 'Jefferson',
              year = YR,
              output = 'wide',
              geometry = TRUE,
              keep_geo_vars = TRUE) %>%
    st_transform(alb)
  bg <- bind_rows(OUT, bg)
}

bg2 <- bg %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         prop_POC = 1 - (white/total)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, medhhinc, agghhinc, hu, mnhhinc)

## export census data
bg2 %>% st_transform(crs = 4326) %>%
  st_write("data/bg_data.geojson", driver = 'geojson')
