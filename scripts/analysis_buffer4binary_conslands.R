################################################################################################
## PURPOSE: This script unions and buffers conservation land data
## BY: Dean Hardy
################################################################################################

rm(list=ls())

library(tidyverse)
library(sf)

## define variables
#DIST = c(8000, 16000, 24000) ## distance (m) of beneficiary zones used in demographic analysis
#PERC = c(0.005, 0.01, 0.02) ## percent of buffer zone for unioning reserves
DIST = c(40,80,120,160,240,320,480) ## distance of buffer zones as produce of DIST * PERC

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

#### still working on this; some of the combos produce same result
## private and public reserves in close proximity via buffering
## nested for loop
for(i in DIST) {
   OUT <- pub %>%
     st_buffer(., dist = i) %>%
     st_cast("POLYGON") %>%
     data.frame() %>%
     mutate(buf_m = i, conscat = 'Public')
   pub_buf <- rbind(OUT, pub_buf)
}

for(i in DIST) {
    OUT <- pvt %>%
      st_buffer(., dist = i) %>%
      st_cast("POLYGON") %>%
      data.frame() %>%
      mutate(buf_m = i, conscat = 'Private')
    pvt_buf <- rbind(OUT, pvt_buf)
}

cl_buf <- rbind(pvt_buf, pub_buf)

table(cl_buf$buf_m, cl_buf$conscat)

cl_buf %>% st_as_sf() %>% st_transform(4326) %>%
  st_write(file.path(datadir, 'conslands_er1_bufs.geojson'), delete_dsn=TRUE)

#########################
## leaflet map
#########################
library(leaflet)
library(leaflet.extras)

pvt2 <- st_cast(pvt, 'POLYGON') %>% st_transform(4326)
pub2 <- st_cast(pub, 'POLYGON') %>% st_transform(4326)
buf <- cl_buf %>% st_as_sf() %>% st_transform(4326)

pal <- colorFactor(rainbow(7), buf$buf_m)

m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolygons(data = buf, 
              color = ~pal(buf$buf_m),
              fillOpacity = 0.5,
              weight = 1, 
              group = "Buffer Zones") %>%
  addPolygons(data = pub2,
              group = "Public Reserves",
              fillColor = 'green',
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolygons(data = pvt2,
              group = "Private Reserves",
              fillColor = 'red',
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map"), 
                   overlayGroups = c("Private Reserves", "Public Reserves", "Buffer Zones"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright",
            pal = pal,
            values = buf$buf_m,
            title = "Buffer Zone") %>%
  addScaleBar("bottomright")
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/cons_lands/buffer_zones.html",
           title = "Buffer Zones")
