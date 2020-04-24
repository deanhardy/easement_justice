## HEADER START ----------------------------------------------
## PURPOSE: this script downloads spatial census data on race, income, housing
## from several states in the US Southeast
## 
## Author: Dean Hardy
## HEADER END ----------------------------------------------

rm(list=ls())

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(sf)


###### Define variables #######
#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
st <- NULL # used in loop for state data download
bg <- NULL # used in for loop for block group data download
gm <- NULL # used in for loop for calculating gmedian
YR <- 2018
ST <- c('GA', 'FL', 'SC', 'NC')
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", total = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E", mhv = "B25077_001E")
 
# all_vars <- load_variables(2018, 'acs5', cache = TRUE)
# ## define decennial variables of interest
# dec_vars <- c(white = "P0050003", black = "P0050004",
#               native_american = "P0050005", asian = "P0050006",
#               hawaiian = "P0050007", other = "P0050008",
#               multiracial = "P0050009", nonlatinx = "P0050002",
#               total = "P0050001")


###### State Geography ######
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

## cleanup and export for use in comparison of state level numbers to CABZ numbers
st2 <- st %>%
  dplyr::select(GEOID, NAME, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, medhhinc, agghhinc, hu, mhv)
write.csv(st2, file.path(datadir, 'st_data.csv'))


###### Block Geography (race, income, etc) ######
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


###### BG Geography (race, income, etc) ######
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

bg2 <- bg %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6, mnhhinc = agghhinc/hu,
         propPOC = 1 - (white/total)) %>%
  dplyr::select(STATEFP, GEOID, ALAND, AWATER, sqkm_bg, total, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, mnhhinc, mhv)

## export census data
bg2 %>% st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_demg.geojson"), driver = 'geojson', delete_dsn = TRUE)

## export census data as centroids
bg2 %>% 
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  st_write(file.path(datadir, "bg_demg_cntrd.geojson"), driver = 'geojson', delete_dsn = TRUE)


###### BG Geography (median hh income) ######
## download hh income distribution tables for block groups & label bins
for(i in 1:length(ST)) {
  OUT <- get_acs(geography = "block group",
                 table = 'B19001',
                 state = ST[[i]],
                 year = YR) %>%
    select(-NAME, -moe) %>%
    rename(households = estimate) %>%
    filter(variable != 'B19001_001') %>%
    mutate(bin_min = ifelse(variable == 'B19001_002', 0, 
                            ifelse(variable == 'B19001_003', 10000, 
                                   ifelse(variable == 'B19001_004', 15000,
                                          ifelse(variable == 'B19001_005', 20000,
                                                 ifelse(variable == 'B19001_006', 25000,
                                                        ifelse(variable == 'B19001_007', 30000,
                                                               ifelse(variable == 'B19001_008', 35000,
                                                                      ifelse(variable == 'B19001_009', 40000,
                                                                             ifelse(variable == 'B19001_010', 45000,
                                                                                    ifelse(variable == 'B19001_011', 50000,
                                                                                           ifelse(variable == 'B19001_012', 60000,
                                                                                                  ifelse(variable == 'B19001_013', 75000,
                                                                                                         ifelse(variable == 'B19001_014', 100000,
                                                                                                                ifelse(variable == 'B19001_015', 125000,
                                                                                                                       ifelse(variable == 'B19001_016', 150000,
                                                                                                                              ifelse(variable == 'B19001_017', 200000, variable)))))))))))))))),
           bin_max = ifelse(variable == 'B19001_002', 9999, 
                            ifelse(variable == 'B19001_003', 14999, 
                                   ifelse(variable == 'B19001_004', 19999,
                                          ifelse(variable == 'B19001_005', 24999,
                                                 ifelse(variable == 'B19001_006', 29999,
                                                        ifelse(variable == 'B19001_007', 34999,
                                                               ifelse(variable == 'B19001_008', 39999,
                                                                      ifelse(variable == 'B19001_009', 44999,
                                                                             ifelse(variable == 'B19001_010', 49999,
                                                                                    ifelse(variable == 'B19001_011', 59999,
                                                                                           ifelse(variable == 'B19001_012', 74999,
                                                                                                  ifelse(variable == 'B19001_013', 99999,
                                                                                                         ifelse(variable == 'B19001_014', 124999,
                                                                                                                ifelse(variable == 'B19001_015', 149999,
                                                                                                                       ifelse(variable == 'B19001_016', 199999,
                                                                                                                              ifelse(variable == 'B19001_017', NA, variable))))))))))))))))) %>%
    mutate(interval = paste(bin_min, bin_max, sep = "-"))
  
  gm <- rbind(gm, OUT)
}

## export HH income data for use in grouped estimated HH median
write.csv(gm, file.path(datadir, 'gm_data.csv'))



