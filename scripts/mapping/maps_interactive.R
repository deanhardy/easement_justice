rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

#### Import Data ####
## import "lowcountry" regions
t1 <- st_read(file.path(datadir, "lc_tier1/lc_tier1.shp")) %>%
  st_transform(4326)

## import cons lands data
cl <- st_read(file.path(datadir, 'cl.geojson'))

## import cons buf zones w demg data
buf <- st_read(file.path(datadir, 'cl_buf_demg.geojson')) %>%
  mutate(conscat = as.character(conscat)) %>%
  filter(buf_m == 320) 

## import beneficiary zones data
bz <- st_read(file.path(datadir, 'cabz.geojson')) %>%
  filter(buf_m == 320 & bzone_m == 16000) 

## import block group data
bg <- st_read(file.path(datadir, 'bg_demg.geojson'))%>%
  filter(st_intersects(t1, ., sparse = F))

# set color schemes
clpal <- colorFactor(c('darkorchid4', 'darkgreen'), cl$conscat)
bufpal <- colorFactor(c('darkorchid1', 'green'), buf$conscat)
bzpal <- colorFactor(c('darkorchid1', 'green'), bz$conscat)
# bgpal <- colorFactor(gray.colors(5, start = 0.3, end = 0.9, gamma = 1, rev = TRUE), bg$propPOC)
bgpal <- colorQuantile("Greens", bg$propPOC, n = 3)
bgincpal <- colorQuantile("Blues", bg$medhhinc, n = 3)

# make leaflet map
m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolylines(data = t1,
               color = 'black',
               weight = 2) %>%
  addPolygons(data = bg,
              group = "US Census (Race)",
              color = 'black',
              fillColor = ~bgpal(bg$propPOC),
              fillOpacity = 1,
              weight = 0.5) %>%
  addPolygons(data = bg,
              group = "US Census (Income)",
              color = 'black',
              fillColor = ~bgincpal(bg$medhhinc),
              fillOpacity = 1,
              weight = 0.5) %>%
  # addPolylines(data = bz,
  #              group = "Beneficiary Zones",
  #              color = ~bzpal(bz$conscat)) %>%
  addPolygons(data = buf,
              popup = paste("<strong>Conservation Buffer Zone</strong>", "<br>",
                            # "Site Name:", df$sitename, "<br>",
                            # "Management:", df$management, "<br>",
                            #"Conservation Area (Acres):", round(df$acres, 0), "<br>",
                            "Beneficiary Zone (KM^2):", round(buf$sqkm_bz, 0), "<br>",
                            "Beneficiary Zone Radius (KM):", buf$bzone_m/1000, "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "People of Color (%):", 100*buf$propPOC, "<br>",
                            "Black (%):", 100*buf$pblack, "<br>",
                            "Other race (%):", 100*buf$pother, "<br>",
                            "Latinx (%):", 100*buf$platinx, "<br>",
                            "White (%):", 100*buf$pwhite, "<br>",
                            "Estimated Median HH Income (US$):", round(buf$emedhhinc, 0), "<br>",
                            "Estimated Mean HH Income (US$):", buf$mnhhinc),
              group = "Conservation Buffer Zones",
              fillColor = ~bufpal(buf$conscat),
              fillOpacity = 1,
              weight = 1) %>%
  addPolygons(data = cl,
              popup = paste("<strong>Conservation Reserve Info</strong>", "<br>",
                            "Owner Type: ", cl$owntype, "<br>",
                            "Site Name: ", cl$sitename, "<br>",
                            "Management: ", cl$management, "<br>",
                            "Conservation Area (Acres): ", round(cl$acres, 0), "<br>",
                            "GAP Status: ", cl$gap, "<br>",
                            "Public Access: ", cl$access, "<br>",
                            "Purpose:", cl$purpose, "<br>",
                            "Data Source:", cl$source),
              group = "Conservation Reserves",
              fillColor = ~clpal(cl$conscat),
              color = 'black',
              fillOpacity = 1,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery", "US Census (Race)",  "US Census (Income)"), 
      overlayGroups = c("Conservation Reserves", "Conservation Buffer Zones", "Beneficiary Zones"),
      options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright",
            pal = clpal,
            values = cl$conscat,
            title = "Reserve Type",
            group = "Conservation Reserves") %>%
  addLegend("bottomright",
            pal = bufpal,
            values = buf$conscat,
            title = "Buffer Type",
            group = "Conservation Buffer Zones") %>%
  # addLegend("bottomleft",
  #           pal = bgpal,
  #           values = bg$propPOC,
  #           group = "US Census (Race)",
  #           title = "People of Color (%)") %>%
  hideGroup("Conservation Reserves") %>%
  addScaleBar("bottomleft")
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/easement-justice/lowcountry_conservation.html",
           title = "Analysis of Lowcountry Conservation")


