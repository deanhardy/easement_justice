rm(list=ls())

library(tidyverse)
library(sf)
library(lwgeom)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
BENZ = c(8000, 16000, 24000) ## beneficiary zone distance
BUFP = c(0.005, 0.01, 0.02) ## cons lands buffer distance proportion of BENZ


#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

##############################################################
## data import
##############################################################
cons <- st_read(file.path(datadir, 'cl_bufs.geojson')) %>%
  rowid_to_column() %>%
  st_transform(crs = alb)

## import census data
bg <- st_read(file.path(datadir, "bg_demg.geojson"), stringsAsFactors = FALSE) %>%
  mutate(STATEFP = as.numeric(STATEFP), GEOID = as.numeric(GEOID)) %>%
  mutate(statefp = ifelse(STATEFP == 37, 45,
                          ifelse(STATEFP == 12, 13, 
                                 ifelse(STATEFP == 1, 13, STATEFP))))%>%
  filter(statefp != 'NA') %>%
  st_transform(crs = alb)

urb <- urban_areas(cb = TRUE, year = 2018) %>%
  st_as_sf() %>%
  st_transform(crs = alb) %>%
  filter(UATYP10 == 'U' & st_intersects(st_union(bg), ., sparse = F))

# tm_shape(urb) + tm_fill(fill = 'grey') + tm_shape(cons2) + tm_polygons(fill = 'urban')

## for each buffered conservation reserve create a beneficiary zone (BZONE) around it
# for demographic analysis
cabz <- NULL ## Conservation Area Beneficiary Zone
for(i in BENZ) {
  for(j in BUFP) {
   OUT <- cons %>%
     filter(cons$buf_m == i * j) %>%
     st_buffer(., dist = i) %>%
     data.frame() %>%
     mutate(bzone_m = i, sqkm_bz = as.numeric(st_area(geometry) / 1e6))
   cabz <- rbind(OUT, cabz)
  }
}

cabz <- cabz %>% st_as_sf() ## re-spatialize

cabz <- filter(cabz, !(cabz$bzone_m == 8000 & cabz$buf_m %in% c(80,160))) %>% 
  rename(statefp = STATEFP) ## filter redundancies 

table(cabz$bzone_m, cabz$buf_m) ## check results

## export centroids of cabz
st_centroid(cabz) %>%
  st_transform(4326) %>%
  select(rowid) %>%
  st_write(file.path(datadir, 'cabz_cntrd.geojson'), driver = 'geojson', delete_dsn = TRUE)

cabz %>% st_transform(4326) %>% 
st_write(file.path(datadir, 'cabz.geojson'), driver = 'geojson', delete_dsn = TRUE)


###################################################
## apply proportional area adjustment to variables
## to assess count within buffer zones
###################################################

## define intersection between cons area ben zones and block groups
int <- as_tibble(st_intersection(bg, cabz))

## proportional area adjustment/allocation method
percBGinBZ <- int %>%
  mutate(sqkm_bginbz = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(perc_bginbz = (sqkm_bginbz/sqkm_bg), sqkm_land = ALAND/ 1e6)

## save percBGinBZ data to use in median HH income estimation (see below)
bg_for_emed <- percBGinBZ %>%
  data.frame() %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(rowid, GEOID, perc_bginbz) %>%
  rename(BZID = rowid)

## demographic analysis of cons area ben zones
cabz_demo <- percBGinBZ %>%
  mutate(tot_pop = total * perc_bginbz,
         white = white * perc_bginbz, 
         black = black * perc_bginbz,
         other = (native_american+asian+hawaiian+other+multiracial) * perc_bginbz,
         #other = multiracial * perc_bginbz,
         latinx = latinx * perc_bginbz,
         hu = hu * perc_bginbz,
         tenure_b = tenure_b * perc_bginbz,
         tenure_tot = tenure_tot * perc_bginbz,
         sqkm_land = sqkm_land * perc_bginbz) %>%
  mutate(agghhinc = hu * mnhhinc) %>%
  group_by(rowid) %>% ## regroups to cons buffer (or beneficiary?) zones after demo analysis on intersections
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            other = sum(other), latinx = sum(latinx),
            tenure_b = sum(tenure_b), tenure_tot = sum(tenure_tot),
            hu = round(sum(hu, na.rm = TRUE), 0), agghhinc = sum(agghhinc, na.rm = TRUE),
            sqkm_bz = mean(sqkm_bz), sqkm_land = sum(sqkm_land), bzone_m = mean(bzone_m)) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/sqkm_land, 2), propPOC = round(1 - pwhite, 2),
         ptenure_b = round(tenure_b/tenure_tot, 2), mnhhinc = round(agghhinc/hu, 0), pland = round((sqkm_land)/sqkm_bz, 2)) %>%
  merge(cons, by = 'rowid') %>%
  dplyr::select(rowid, conscat, bzone_m, buf_m, tot_pop, popden, sqkm_bz, pland, pwhite, white, pblack, black, pother, other, 
                platinx, latinx, propPOC, ptenure_b, tenure_b, hu, mnhhinc, geometry) %>%
  st_as_sf()

cabz2 <- cabz %>% select(rowid, statefp) 
st_geometry(cabz2) <- NULL
cabz_demo2 <- merge(cabz_demo, cabz2)

# cabz_demo %>% st_as_sf() %>% filter(bzone_m == 16000 & buf_m == 320) %>% qtm()

###
###### Estimate median HH incomes within AOI #######
###

## this section of code calculates an estimated median from grouped (aka binned) household income data 
## within an area that overlaps several block groups

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

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

## import median HH income data
gm <- read.csv(file.path(datadir, 'gm_data.csv'), stringsAsFactors = FALSE) %>%
  mutate(GEOID = as.character(GEOID))
  
bg2 <- gm %>%
  left_join(bg_for_emed, by = "GEOID") %>%
  filter(perc_bginbz != 'NA') %>%
  mutate(eHH = households * perc_bginbz) %>%
  group_by(BZID, variable) %>%
  summarise(interval = interval[[1]], eHH = sum(eHH), households = sum(households)) %>%
  summarise(gmedian = GMedian(eHH, interval, sep = "-", trim = 'cut'))

## import gmedian estimates for hh income
emed <- bg2 %>%
  rename(rowid = BZID, emedhhinc = gmedian) %>%
  mutate(emedhhinc = round(emedhhinc, 0))

## merge emedian hh income with other demographic data
df <- cabz_demo2 %>% 
  merge(emed, by = "rowid") 

# density plot
ggplot(df, aes(x = emedhhinc, group = conscat)) +
  geom_density(aes(color = conscat)) +
  theme_bw()

## connect to urban areas
df <- df %>%
  mutate(urban = if_else(st_contains(st_union(urb), ., sparse = F) == TRUE, 'yes', 'no')) %>%
  st_transform(4326) 

##############################
## export data
##############################

## export cl-buf with demg data as polygons
df %>% st_write(file.path(datadir, 'cl_buf_demg.geojson'), driver = 'geojson', delete_dsn = TRUE)

## export ONLY attribute data
df %>%
  st_set_geometry(NULL) %>%
  write.csv(file.path(datadir, 'cl_buf_demg_data.csv'), row.names = FALSE)

