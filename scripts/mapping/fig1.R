rm(list=ls())

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)

utm <- 2150 ## NAD83 17N
clr <- c('#fc8d59', '#ffffbf', '#91bfdb')
clr2 <- c('#7570b3', '#1b9e77', 'white')

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

#### Import Data ####
## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1/lc_tier1.shp")) %>%
  st_transform(utm) %>%
  mutate(acres = as.numeric(st_area(geometry) * 0.00024710538))

## import cons buf zones data
buf <- st_read(file.path(datadir, 'cl_bufs.geojson')) %>%
  st_transform(utm) %>%
  mutate(conscat = as.character(conscat), acres = st_area(geometry) * 0.000247105)
attributes(buf$acres) = NULL
buf <- buf %>%   filter(acres >= 5 & buf_m == 320)

## import cons lands data
cl <- st_read(file.path(datadir, 'cl.geojson')) %>%
  st_transform(utm)

## import block group data
bg <- st_read(file.path(datadir, 'bg_demg.geojson')) %>%
  st_transform(utm)

## download ancillary data for cartographic use
## https://www.census.gov/geo/maps-data/maps/2010ua.html
st <- states(cb = FALSE, resolution = '500k', year = NULL) %>%
  st_as_sf() %>%
  filter(NAME %in% c('Georgia', 'South Carolina', 'North Carolina', 'Alabama', 'Florida'))

urb <- urban_areas(cb = TRUE, year = 2018) %>%
  st_as_sf() %>%
  filter(NAME10 %in% c('Athens-Clarke County, GA', 'Savannah, GA', 'Brunswick, GA', 
                       'Charleston--North Charleston, SC', 'Myrtle Beach--Socastee, SC--NC', 'Hilton Head Island, SC')) %>%
  st_centroid() %>%
  mutate(name = ifelse(NAME10 == 'Athens-Clarke County, GA', 'Athens',
                       ifelse(NAME10 == 'Savannah, GA', 'Savannah',
                              ifelse(NAME10 == 'Brunswick, GA', 'Brunswick',
                                     ifelse(NAME10 == 'Charleston--North Charleston, SC', 'Charleston',
                                            ifelse(NAME10 == 'Myrtle Beach--Socastee, SC--NC', 'Myrtle Beach',
                                                   ifelse(NAME10 == 'Hilton Head Island, SC', 'Hilton Head', NA))))))) %>%
  filter(name != 'Athens')

table(buf$conscat)


#### CONSCAT Map ####
fig <- 
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  # tm_shape(buf) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(buf) +
  tm_fill(col = 'conscat', alpha = 1, palette = clr2,
          legend.show = FALSE) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_text(text = 'NAME') +
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.3, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.75,0.35)) +
  tm_scale_bar(breaks = c(0,50,100), text.size = 0.8, position= c(0.72, 0.26)) +
  tm_add_legend(type = c('fill'), 
                border.col = c('black','black','black'),
                labels = c('Private Easement', 'Public Reserve', 'Lowcountry'), 
                col = clr2, 
                title = "MAP KEY") +
  tm_legend(position = c(0.68, 0.04),
            bg.color = 'white',
            frame = TRUE,
            legend.text.size = 1,
            legend.title.size = 1.2) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'white',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3/2)
# fig

tiff(file.path(datadir, 'figures/conscat-map.tiff'), units = 'in',
    height = 4, width = 6, res = 300, compression = 'lzw')
fig
dev.off()


#### Conscat/DEMG Map ####

pvt <- buf %>% filter(conscat == 'Private')
mycols <- gray.colors(5, start = 0.3, end = 0.9, gamma = 2.2, rev = TRUE)

bgc <- bg %>%
  filter(st_intersects(t1, ., sparse = F)) %>%
  mutate(percPOC = round(propPOC *100),0)

fig2 <- 
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(st) + tm_fill(col = 'white') +
  # tm_shape(bgc) +
  # tm_polygons(col = 'percPOC', palette = mycols, style = 'fisher', lwd = 0.2, title = 'People of Color (%)') +
  tm_shape(buf) +
  tm_fill(col = 'conscat', alpha = 1, palette = clr2,
          legend.show = FALSE) +
  # tm_fill(col = 'green', alpha = 1, palette = '#7570b3',
  #           legend.show = FALSE) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_text(text = 'NAME') +
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.1, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.75,0.35)) +
  tm_scale_bar(breaks = c(0,50,100), text.size = 0.8, position= c(0.72, 0.26)) +
  tm_add_legend(type = c('fill'), 
                border.col = 'black',
                labels = c('Private Easement Zone', 'Public Reserve Zone', 'Lowcountry'), 
                col = clr2, 
                title = "MAP KEY") +
  tm_legend(position = c(0.63, 0.04),
            bg.color = 'white',
            frame = TRUE,
            legend.text.size = 1,
            legend.title.size = 1.2) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'white',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3/2)
#fig2

# tiff(file.path(datadir, 'figures/fig1.tiff'), units = 'in',
#      height = 4, width = 6, res = 150, compression = 'lzw')
# fig2
# dev.off()

png(file.path(datadir, 'figures/fig1.png'), units = 'in',
    height = 4, width = 6, res = 150)
fig2
dev.off()
