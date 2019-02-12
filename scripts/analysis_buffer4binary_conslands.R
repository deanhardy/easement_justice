################################################################################################
## PURPOSE: This script unions and buffers conservation land data
## BY: Dean Hardy
################################################################################################

rm(list=ls())

library(tidyverse)
library(sf)

## define variables
DIST = c(8000, 16000, 24000) ## distance (m) of buffer zones
PERC = c(0.005, 0.01, 0.02) ## percent of buffer zone for unioning reserves
pub_buf <- NULL
pvt_buf <- NULL

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/cons_lands')

## read in data
dat <- st_read(file.path(datadir, 'cons_lands.shp'))

## filter to lowcountry and public or private
pub <- dat %>%
  filter(ecorg_tier == 1 & conscat == 'Public') %>%
  st_union()
pvt <- dat %>%
  filter(ecorg_tier == 1 & conscat == 'Private') %>%
  st_union()

## private and public reserves in close proximity via buffering
## nested for loop
for(i in PERC) {
for(j in DIST) {
  OUT <- pub %>%
    st_buffer(., dist = i * j) %>%
    st_cast("POLYGON") %>%
    data.frame() %>%
    mutate(buf_m = i * j, conscat = 'Public')
  pub_buf <- rbind(OUT, pub_buf)
}
}

for(i in PERC) {
  for(j in DIST) {
    OUT <- pvt %>%
      st_buffer(., dist = i * j) %>%
      st_cast("POLYGON") %>%
      data.frame() %>%
      mutate(buf_m = i * j, conscat = 'Private')
    pvt_buf <- rbind(OUT, pvt_buf)
  }
}

# CL <- rbind(pub, pvt)
# cl_buf <- NULL
# ## working on nested for loop
# for(x in 1:length(CL)) {
# for(i in PERC) {
# for(j in DIST) {
#     OUT <- cl_buf %>%
#       st_buffer(., dist = i * j) %>%
#       st_cast("POLYGON") %>%
#       data.frame() %>%
#       mutate(buf_m = i * j, conscat = x)
#     cl_buf <- rbind(OUT, cl_buf)
# }
# }
# }

cl_buf <- rbind(pvt_buf, pub_buf)

table(cl_buf$buf_m, cl_buf$conscat)

st_write(cl_buf, file.path(datadir, 'conslands_er1_bufs.shp'), driver = 'ESRI Shapefile')
