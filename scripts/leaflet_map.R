rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(sf)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/cons_lands')

df2 <- st_read(file.path(datadir, 'tncxtnc.geojson'))
leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolygons(data = df2)

df <- st_read(file.path(datadir, 'bz_data.geojson')) %>%
  filter(ecorg_tier == 1)
bf <- st_read(file.path(datadir, 'buf_zones.geojson')) %>%
  filter(ecorg_tier == 1)

pal <- colorFactor(rainbow(3), df$conscat)

m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolygons(data = df,
              popup = paste("Site Name:", df$sitename, "<br>",
                            "Management:", df$management, "<br>",
                            "Conservation Area (Acres):", round(df$acres, 0), "<br>",
                            "Beneficiary Zone (KM^2):", round(df$sqkm_buf, 0), "<br>",
                            "Land (%):", 100*df$pland, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "Black (%):", 100*df$pblack, "<br>",
                            "Other race (%):", 100*df$pother, "<br>",
                            "Latinx (%):", 100*df$platinx, "<br>",
                            "White (%):", 100*df$pwhite, "<br>",
                            "Estimated Median HH Income (US$):", round(df$emedhhinc, 0), "<br>",
                            "Estimated Mean HH Income (US$):", df$mnhhinc, "<br>",
                            "GAP Status:", df$gap, "<br>",
                            "Purpose:", df$purpose, "<br>",
                            "Data Source:", df$source),
              group = "Conservation Areas",
              fillColor = ~pal(df$conscat),
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolylines(data = bf, 
               color = ~pal(bf$conscat),
               weight = 1, 
               group = "Buffer Zones") %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
      overlayGroups = c("Conservation Areas", "Buffer Zones"),
      options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright",
            pal = pal,
            values = df$conscat,
            title = "Conservation Area") %>%
  addScaleBar("bottomright")
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/cons_lands/lowcountry_conservation.html",
           title = "Lowcountry Conservation Areas")


