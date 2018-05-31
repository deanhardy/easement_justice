# rm(list=ls())

library(leaflet)
library(sf)

df <- st_read("data/bz_data.geojson")

pal <- colorFactor(rainbow(3), df$type)

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  # addPolygons(data = df,
  #             label = ~esmthldr,
  #             group = "hover",
  #             fillColor = ~pal(propPOC),
  #             fillOpacity = 0.5,
  #             weight = 1) %>%
  addPolygons(data = df,
              popup = paste("Site Name:", df$sitename, "<br>",
                            "Management:", df$management, "<br>",
                            "Acres:", df$acres, "<br>",
                            "People of Color (%)", "<br>",
                            "within 10 miles:", 100*df$propPOC, "<br>",
                            "GAP Status:", df$gap),
              group = "click",
              fillColor = ~pal(df$type),
              fillOpacity = 0.5,
              weight = 1) %>%
  # addLayersControl(baseGroups = c("hover", "click")) %>%
  addScaleBar() %>%
  addLegend(pal = pal,
            values = df$type,
            title = "Conservation Area")
m

### exploring exporting as html file for offline exploration
library(htmlwidgets)
# saveWidget(m, file="C:/Users/dhardy/Dropbox/sesync/manuscripts/unpublished/easement_justice/R/easement_justice/docs/lowcountry_conservation.html")
