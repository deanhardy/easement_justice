rm(list=ls())

library(leaflet)
library(sf)

df <- st_read("data/bz_data.geojson")
bf <- st_read('data/ben_zones.geojson')

pal <- colorFactor(rainbow(3), df$type)

m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addPolygons(data = df,
              popup = paste("Site Name:", df$sitename, "<br>",
                            "Management:", df$management, "<br>",
                            "Acres:", df$acres, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "Black (%):", 100*df$pblack, "<br>",
                            "Other race (%):", 100*df$pother, "<br>",
                            "Latinx (%):", 100*df$platinx, "<br>",
                            "White (%):", 100*df$pwhite, "<br>",
                            "Median Household Income (US$):", df$mnmdhhinc, "<br>",
                            "GAP Status:", df$gap),
              group = "Conservation Areas",
              fillColor = ~pal(df$type),
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolylines(data = bf, 
               color = ~pal(bf$type),
               weight = 1, 
               group = "Beneficiary Zones") %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
      overlayGroups = c("Conservation Areas", "Beneficiary Zones"),
      options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright",
            pal = pal,
            values = df$type,
            title = "Conservation Area") %>%
  addScaleBar("bottomright")
m

### exploring exporting as html file for offline exploration
library(htmlwidgets)
saveWidget(m, 
           file="C:/Users/dhardy/Dropbox/sesync/manuscripts/unpublished/easement_justice/R/easement_justice/docs/lowcountry_conservation.html",
           title = "Lowcountry Conservation Areas")


