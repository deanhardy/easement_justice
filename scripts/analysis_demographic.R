rm(list=ls())

library(tidyverse)
library(sf)
library(lwgeom)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
BZONE = 16000 ## beneficiary zone buffer distance
CLBUF = 160 ## cons lands buffer distance

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

##############################################################
## data import
##############################################################

cons <- st_read(file.path(datadir, 'conslands_er1_bufs.geojson')) %>%
  rowid_to_column() %>%
  st_transform(crs = alb)

## import census data
bg <- st_read(file.path(datadir, "bg_data.geojson")) %>%
  st_transform(crs = alb)

# density plot
ggplot(bg, aes(x = white)) +
  geom_density() +
  theme_bw()



###################################################
## apply proportional area adjustment to variables
## to assess count within buffer zones
###################################################

## euclidean distance buffering
bz <- cons %>%
  st_buffer(dist = BZONE) %>%
  mutate(sqkm_buf = as.numeric(st_area(geometry) / 1e6))

st_centroid(bz) %>%
  st_transform(4326) %>%
  select(rowid) %>%
  st_write(file.path(datadir, 'bz_cntrd.geojson'), driver = 'geojson', delete_dsn = TRUE)

bz %>% st_transform(4326) %>%
  st_write(file.path(datadir, 'ben_zones.geojson'), driver = 'geojson', delete_dsn = TRUE)

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(bz, bg))

## proportional area adjustment/allocation method
percBGinBUF <- int %>%
  mutate(sqkm_bginbuf = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginbuf = (sqkm_bginbuf/sqkm_bg))

## save percBGinBUF data to use in median HH income estimation (see below)
bg_for_emed <- percBGinBUF %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(rowid, GEOID, perc_bginbuf) %>%
  rename(BUFID = rowid)

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
ggplot(bz_geog, aes(x = pwhite, group = conscat)) +
  geom_density(aes(color = conscat)) +
  theme_bw()



#########################################################
## estimate median household incomes within buffer zones
#########################################################
## this section of code calculates an estimated median from grouped (aka binned) household income data 
## within an area that overlaps several block groups

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

## define variables 
YR <- 2016
ST <- c('GA', 'SC', 'AL', 'FL', 'NC')
gm <- NULL # used in for loop

## load libraries
library(tidycensus)
options(tigris_use_cache = TRUE)

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

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html
## B modified to account for when median group is the 0-9999 bin
GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}

bg2 <- gm %>%
  left_join(bg_for_emed, by = "GEOID") %>%
  filter(perc_bginbuf != 'NA') %>%
  mutate(eHH = households * perc_bginbuf) %>%
  group_by(BUFID, variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## import gmedian estimates for hh income
emed <- bg2 %>%
  rename(rowid = BUFID, emedhhinc = gmedian)

## merge emedian hh income with other demographic data
df <- bz_geog %>% 
  merge(emed, by = "rowid") %>%
  st_transform(4326) 
# filter(state %in% c('GA', 'SC'))

# density plot
ggplot(df, aes(x = emedhhinc, group = conscat)) +
  geom_density(aes(color = conscat)) +
  theme_bw()

##############################
## export data
##############################

st_write(df,file.path(datadir, 'bz_data.geojson'), driver = 'geojson', delete_dsn = TRUE)

df %>%
  st_set_geometry(NULL) %>%
  write.csv(file.path(datadir, 'cons_data.csv'), row.names = FALSE)


