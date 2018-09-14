rm(list=ls())

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)

utm <- 2150 ## NAD83 17N

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1.shp")) %>%
  st_transform(utm)
t2 <- st_read(file.path(datadir, "lc_tier2.shp")) %>%
  st_transform(utm)
t3 <- st_read(file.path(datadir, "lc_tier3.shp")) %>%
  st_transform(utm)

## import cons lands data
dat <- st_read(file.path(datadir, 'cons_lands.geojson')) %>%
  st_transform(utm)

## download ancillary data for cartographic use
## https://www.census.gov/geo/maps-data/maps/2010ua.html
st <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME %in% c('Georgia', 'South Carolina', 'North Carolina', 'Alabama', 'Florida'))
urb <- urban_areas(cb = TRUE, year = 2016) %>%
  st_as_sf() %>%
  filter(NAME10 %in% c('Athens-Clarke County, GA', 'Savannah, GA', 'Brunswick, GA', 
                       'Charleston--North Charleston, SC', 'Myrtle Beach--Socastee, SC--NC', 'Hilton Head Island, SC')) %>%
  st_centroid() %>%
  mutate(name = ifelse(NAME10 == 'Athens-Clarke County, GA', 'Athens',
                       ifelse(NAME10 == 'Savannah, GA', 'Savannah',
                              ifelse(NAME10 == 'Brunswick, GA', 'Brunswick',
                                     ifelse(NAME10 == 'Charleston--North Charleston, SC', 'Charleston',
                                            ifelse(NAME10 == 'Myrtle Beach--Socastee, SC--NC', 'Myrtle Beach',
                                                   ifelse(NAME10 == 'Hilton Head Island, SC', 'Hilton Head', NA)))))))
  
clr <- c('#7570b3', '#1b9e77', '#d95f02')
  
## plot data cons lands by source
fig <- 
  tm_shape(dat) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(t3) + tm_polygons(border.col = 'grey65', col = 'grey95') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(dat) +
  tm_fill(col = 'source', alpha = 1, palette = clr,
         legend.show = FALSE) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.3, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.63,0.08)) +
  tm_scale_bar(breaks = c(0,40, 80), size = 1, position= c(0.6, 0.0)) +
  tm_add_legend(type = c('fill'), labels = c('NCED', 'PADUS', 'TNC'), 
                col = clr, 
                title = "Data Source") +
  tm_legend(position = c(0.75, 0.04),
            bg.color = 'white',
            frame = TRUE,
            legend.text.size = 1.2,
            legend.title.size = 1.4) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'black',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3.2/2)
# fig

png(file.path(datadir, 'figures/conslands_by_source.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()




#################################
## cons land by type
#################################
dat2 <- dat %>%
  filter(conscat %in% c('Private', 'Public'))

clr2 <- c('#7570b3', '#1b9e77')

## plot data cons lands by source
fig <- 
  tm_shape(dat) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(t3) + tm_polygons(border.col = 'grey65', col = 'grey95') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(dat) +
  tm_fill(col = 'conscat', alpha = 1, palette = clr2,
          legend.show = FALSE) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.3, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.63,0.08)) +
  tm_scale_bar(breaks = c(0,40, 80), size = 1, position= c(0.6, 0.0)) +
  tm_add_legend(type = c('fill'), labels = c('Private', 'Public'), 
                col = clr2, 
                title = "Category") +
  tm_legend(position = c(0.75, 0.04),
            bg.color = 'white',
            frame = TRUE,
            legend.text.size = 1.2,
            legend.title.size = 1.4) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'black',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3.2/2)
# fig

png(file.path(datadir, 'figures/conslands_by_type.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()
