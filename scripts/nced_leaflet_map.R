# rm(list=ls())

library(leaflet)
library(sf)

# df <- st_read("data/nced_race.geojson")

pal <- colorNumeric("Purples", df$propPOC)

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
              popup = paste("Easement Holder:", df$esmthldr, "<br>",
                            "Site Name:", df$sitename, "<br>",
                            "Acres:", df$gis_acres, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "Purpose:", df$purpose, "<br>",
                            "GAP Category:", df$gapcat),
              group = "click",
              fillColor = ~pal(propPOC),
              fillOpacity = 0.5,
              weight = 1) %>%
  # addLayersControl(baseGroups = c("hover", "click")) %>%
  addScaleBar() %>%
  addLegend(pal = pal,
            values = df$propPOC,
            title = "People of Color (%)")

### exploring exporting as html file for offline exploration
library(htmlwidgets)
saveWidget(m, file="m.html")
