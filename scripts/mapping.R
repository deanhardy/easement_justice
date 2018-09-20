rm(list=ls())

library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(tmap)

utm <- 2150 ## NAD83 17N
clr <- c('#fc8d59', '#ffffbf', '#91bfdb')
clr2 <- c('#7570b3', '#1b9e77')

#define data directory
datadir <- file.path('C:/Users/dhardy/Dropbox/r_data/cons_lands')

## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1.shp")) %>%
  st_transform(utm) %>%
  mutate(acres = as.numeric(st_area(geometry) * 0.00024710538))
t2 <- st_read(file.path(datadir, "lc_tier2.shp")) %>%
  st_transform(utm)%>%
  mutate(acres = as.numeric(st_area(geometry) * 0.00024710538))
t3 <- st_read(file.path(datadir, "lc_tier3.shp")) %>%
  st_transform(utm)%>%
  mutate(acres = as.numeric(st_area(geometry) * 0.00024710538))

## import cons lands data
dat <- st_read(file.path(datadir, 'cons_lands.geojson')) %>%
  st_transform(utm) %>%
  filter(acres > 5 & conscat != 'UNK') %>%
  mutate(conscat = as.character(conscat))

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
  


## map of low country region
fig <- 
  tm_shape(dat) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(t3) + tm_fill(col = 'grey95') +
  tm_shape(t1) + tm_polygons(border.col = 'black', col = 'grey85', lwd = 2) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_shape(urb) + tm_dots(col = 'black', size = 0.3, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.63,0.08)) +
  tm_scale_bar(breaks = c(0,40, 80), size = 1, position= c(0.6, 0.0)) +
  # tm_add_legend(type = c('fill'), labels = c('NCED', 'PADUS', 'TNC'), 
  #               col = clr, 
  #               title = "Data Source") +
  # tm_legend(position = c(0.75, 0.04),
  #           bg.color = 'white',
  #           frame = TRUE,
  #           legend.text.size = 1.2,
  #           legend.title.size = 1.4) + 
  tm_layout(frame = TRUE, 
            bg.color = 'skyblue',
            outer.bg.color = 'black',
            outer.margins=c(0,0,0,0),
            # inner.margins=c(0,0,0,0), 
            asp=3.2/2)
# fig

png(file.path(datadir, 'figures/lowcountry_region.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()



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

## plot data cons lands by source
fig <- 
  tm_shape(dat) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(t3) + tm_polygons(border.col = 'grey65', col = 'grey95') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(dat2) +
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

png(file.path(datadir, 'figures/conslands_by_conscat.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()


#################################
## cons land by category for lowcountry only
#################################
dat2 <- dat %>%
  filter(ecorg_tier == 1)

table(dat2$conscat)

## plot data cons lands by category
fig <- 
  tm_shape(dat) + tm_borders(col = NULL) +
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(t3) + tm_polygons(border.col = 'grey65', col = 'grey95') +
  # tm_shape(t2) + tm_borders(col = 'grey65') +
  tm_shape(dat2) +
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
fig

png(file.path(datadir, 'figures/conslands_by_conscat_tier1.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()



## import buffers
buf <- st_read(file.path(datadir, 'buf_zones.geojson')) %>%
  st_transform(utm) %>%
  mutate(conscat = as.character(conscat)) %>%
  filter(ecorg_tier == 1 & acres > 5 & conscat == 'Private' & state == 'GA') %>%
  sample_n(., 50, replace = FALSE)

ga <- dat2 %>%
  filter(state == 'GA' & conscat == 'Private')
  
## plot buffers by category for tier 1 only
fig <- 
  tm_shape(buf) + tm_borders(col = NULL) + 
  tm_shape(st) + tm_fill(col = 'white') +
  tm_shape(t3) + tm_polygons(border.col = 'grey65', col = 'grey95') +
  tm_shape(ga) +
  tm_fill(col = '#7570b3', alpha = 1,
          legend.show = FALSE) +
  tm_shape(st) + tm_borders(col = 'black') + 
  tm_shape(t1) + tm_borders(col = 'black', lwd = 2) +
  tm_shape(buf) + tm_borders(col = 'black') +
  tm_shape(urb) + tm_dots(col = 'black', size = 0.3, shape = 15, legend.show = FALSE) + 
  tm_text(text = 'name', just = 'left', xmod = 0.3, ymod = -0.3, shadow = TRUE) + 
  tm_compass(type = 'arrow', size = 3, position = c(0.7,0.08)) +
  tm_scale_bar(breaks = c(0,40, 80), size = 1, position= c(0.6, 0.0)) +
  tm_add_legend(type = c('fill'), labels = 'Private', 
                col = '#7570b3', 
                title = "Category") +
  tm_legend(position = c(0.9, 0.04),
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

png(file.path(datadir, 'figures/buffers_by_conscat_tier1.png'), units = 'in',
    height = 7.5, width = 13.33, res = 150)
fig
dev.off()



####################
## making plots
####################
## boxplot of private vs public cons land sizes
png('figs/conscat_acres_boxplot.png', res = 150, units = 'in',
    height = 5, width = 5)
boxplot(acres ~ conscat, data = dat2, outline = F,
        ylab = 'Acres', xlab = '')
mtext('*no outliers', side = 1, line = 4, adj = 1)
dev.off()

## 
table(dat$ecorg_tier, dat$conscat)

## percent of coastal plain in conservation
100 * ((sum(dat3$acres_sum) * 10000) / t3$acres)

## percent of lowcountry in conservation
100 * (((dat3[[3,3]] + dat3[[4,3]]) * 10000) / t1$acres)

## percent of coastal plain in private conservation
100 * (((dat3[[1,3]] + dat3[[3,3]]) * 10000) / t3$acres)

## percent of lowcountry in private conservation
100 * (((dat3[[3,3]]) * 10000) / t1$acres)

##
dat3 <- dat %>%
  st_set_geometry(NULL) %>%
  mutate(ecorg_tier = ifelse(ecorg_tier == 1, 1, 3)) %>%
  mutate(label = ifelse(ecorg_tier == 1, 'Lowcountry', 'Coastal Plain')) %>%
  group_by(label, conscat) %>%
  summarise(acres_sum = sum(acres/10000))

filter(dat3, conscat == 'Private' & label == 'Lowcountry') %>%
  print(acres_sum/tier1$acres)

ggplot(dat3) +
  geom_col(aes(label, acres_sum, fill = conscat), position = 'dodge') + 
  scale_discrete_manual(name = 'Conservation Category', fill = clr2)


## import buffer zone demographic data
bzdat <- st_read(file.path(datadir, 'bz_data.geojson')) %>%
  st_transform(utm) %>%
  mutate(conscat = as.character(conscat)) %>%
  filter(ecorg_tier == 1 & acres > 5)

## boxplot of private vs public cons POC
png('figs/conscat_poc_boxplot.png', res = 150, units = 'in',
    height = 5, width = 5)
boxplot(propPOC ~ conscat, data = bzdat, outline = F,
        ylab = 'People of Color', xlab = '')
# mtext('*no outliers', side = 1, line = 4, adj = 1)
dev.off()

## boxplot of private vs public cons emedhhinc
png('figs/conscat_emedhhinc_boxplot.png', res = 150, units = 'in',
    height = 5, width = 5)
boxplot(emedhhinc ~ conscat, data = bzdat, outline = F,
        ylab = 'Estimated Median Household Income', xlab = '')
# mtext('*no outliers', side = 1, line = 4, adj = 1)
dev.off()
