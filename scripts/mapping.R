rm(list=ls())

library(tidyverse)
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

## plot data cons lands by source
fig <- tm_shape(t3) + tm_fill(col = 'grey95') +
  tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(t1) + tm_borders(col = 'grey40') +
  tm_shape(dat) + 
  tm_fill(col = 'source', alpha = 1, palette = c('red', 'green', 'yellow'),
          legend.show = FALSE) +
  tm_compass(type = 'arrow', size = 3, position = c(0.6,0.15)) +
  tm_scale_bar(breaks = c(0,50, 100), size = 1, position= c(0.6, 0.0)) +
  tm_add_legend(type = c('fill'), labels = c('NCED', 'PADUS', 'TNC'), 
                col = c('red', 'green', 'yellow'), 
                title = "Data Source") +
  tm_legend(position = c(0.8, 0.1),
            bg.color = 'white',
            frame = TRUE,
            legend.text.size = 1,
            legend.title.size = 1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=3.2/2)
fig

png(file.path(datadir, 'figures/conslands_by_source.png'), units = 'in',
    height = 7.5, width = 13, res = 150)
fig
dev.off()