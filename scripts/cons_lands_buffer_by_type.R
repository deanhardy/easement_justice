################################################################################################
## PURPOSE: This script unions and buffers conservation land data
## BY: Dean Hardy
################################################################################################

rm(list=ls())

library(tidyverse)
library(lwgeom)
library(sf)

#######################################################################
## union & buffer reserves in prep for public vs private analysis
#######################################################################

DIST = 16000 ## distance (m) of buffer zones
PERC = c(0.005, 0.01, 0.05) ## percent of buffer zone for unioning reserves
pub_buf <- NULL
pvt_buf <- NULL

## filter to lowcountry and public or private
pub <- dat %>%
  filter(ecorg_tier == 1 & conscat == 'Public') %>%
  st_union()
pvt <- dat %>%
  filter(ecorg_tier == 1 & conscat == 'Private') %>%
  st_union()

## private and public reserves in close proximity via buffering
for (i in PERC) {
  OUT <- pub %>%
    st_buffer(., dist = DIST * i) %>%
    st_cast("POLYGON") %>%
    data.frame() %>%
    mutate(buf_m = i * DIST, conscat = 'Public')
  pub_buf <- rbind(OUT, pub_buf)
}

for (i in PERC) {
  OUT <- pvt %>%
    st_buffer(., dist = DIST * i) %>%
    st_cast("POLYGON") %>%
    data.frame() %>%
    mutate(buf_m = i * DIST, conscat = 'Private')
  pvt_buf <- rbind(OUT, pvt_buf)
}

cl_buf <- rbind(pvt_buf, pub_buf)

table(cl_buf$buf_m, cl_buf$conscat)

st_write(cl_buf, file.path(datadir, 'conslands_er1_bufs.shp'), driver = 'ESRI Shapefile')