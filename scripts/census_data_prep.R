rm(list=ls())

library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

## define acs year and region for census data import
YR <- 2016
# ST <- c("Georgia", "South Carolina")
# CNTY <- c('Richland', 'Bryan')

## re-run if census variables need to be changed
## define acs variables of interest
# all_vars <- load_variables(2016, 'acs5', cache = TRUE)
# vars <- c(white = "B03002_003E", black = "B03002_004E",
#           native_american = "B03002_005E", asian = "B03002_006E",
#           hawaiian = "B03002_007E", other = "B03002_008E",
#           multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
#           medhhinc = "B19049_001E")
# 
# ## define decennial variables of interest
# dec_vars <- c(white = "P0050003", black = "P0050004",
#               native_american = "P0050005", asian = "P0050006",
#               hawaiian = "P0050007", other = "P0050008",
#               multiracial = "P0050009", nonlatinx = "P0050002",
#               total = "P0050001")
# 
# ## grab counties to create character vector for data grab
# ## necessary for block data grab, but not for block groups
# ga_cnty <- counties('georgia', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) %>%
#   select('COUNTYFP')
# 
# sc_cnty <- counties('south carolina', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) #%>%
#   select('COUNTYFP')

### need to work on writing this as a for loop or using apply()
ga <- get_acs(geography = "block group",
              variables = c(white = "B03002_003E", black = "B03002_004E",
                            native_american = "B03002_005E", asian = "B03002_006E",
                            hawaiian = "B03002_007E", other = "B03002_008E",
                            multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
                            medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E"),
              state = 'Georgia',
              year = YR,
              output = 'wide',
              geometry = TRUE) %>%
  st_transform(crs = alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu) %>%
  dplyr::select(GEOID, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, medhhinc, agghhinc, hu, mnhhinc)

sc <- get_acs(geography = "block group",
              variables = c(white = "B03002_003E", black = "B03002_004E",
                            native_american = "B03002_005E", asian = "B03002_006E",
                            hawaiian = "B03002_007E", other = "B03002_008E",
                            multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
                            medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E"),
              state = 'South Carolina',
              year = YR,
              output = 'wide',
              geometry = TRUE) %>%
  st_transform(crs = alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu) %>%
  dplyr::select(GEOID, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, medhhinc, agghhinc, hu, mnhhinc)

bg <- rbind(ga, sc) %>%
  mutate(prop_POC = 1 - (white/total))

## grouped median
# http://tillt.net/grouped-median-function-for-r/


## export census data
bg %>% st_transform(crs = 4326) %>%
  st_write("data/bg_data.geojson", driver = 'geojson')