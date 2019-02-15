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

#### still working on this; some of the combos produce same result
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

cl_buf <- rbind(pvt_buf, pub_buf)

table(cl_buf$buf_m, cl_buf$conscat)

st_write(cl_buf, file.path(datadir, 'conslands_er1_bufs.shp'), driver = 'ESRI Shapefile')

#########################
##
#########################

cl <- st_transform(dat, 4326)
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
  addPolygons(data = cl,
              group = "Conservation Areas",
              fillColor = cl$conscat,
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map"), 
                   overlayGroups = c("Conservation Areas", "Buffer Zones"),
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
