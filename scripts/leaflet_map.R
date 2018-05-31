# rm(list=ls())

library(leaflet)
library(sf)

# df <- st_read("data/bz_data.geojson")

# pal <- colorNumeric("Rainbow", df$type)

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
              popup = paste("Status:", df$type, "<br>",
                            "Site Name:", df$sitename, "<br>",
                            "Acres:", df$acres, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "GAP Category:", df$gap),
              group = "click",
              fillColor = df$type,
              fillOpacity = 0.5,
              weight = 1) %>%
  # addLayersControl(baseGroups = c("hover", "click")) %>%
  addScaleBar() %>%
  addLegend(pal = pal,
            values = df$propPOC,
            title = "People of Color (%)")

### exploring exporting as html file for offline exploration
library(htmlwidgets)
saveWidget(m, file="C:/Users/dhardy/Dropbox/sesync/manuscripts/unpublished/easement_justice/R/easement_justice/docs/lowcountry_conservation.html")
