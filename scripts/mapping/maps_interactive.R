rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(sf)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

# import data
df <- st_read(file.path(datadir, 'cl_buf_demg.geojson')) %>%
  filter(bzone_m == 16000 & buf_m == 320)
cl <- st_read(file.path(datadir, 'cl.geojson'))
bz <- st_read(file.path(datadir, 'cabz.geojson')) %>%
  filter(bzone_m == 16000 & buf_m == 320)

# set color schemes
dpal <- colorFactor(rainbow(3), df$conscat)
cpal <- colorFactor(rainbow(3), cl$conscat)
bpal <- colorFactor(rainbow(3), bz$conscat)

# make leaflet map
m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 33, zoom = 7) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolylines(data = bz,
               group = "Beneficiary Zones",
               color = ~bpal(bz$conscat))
  addPolygons(data = df,
              popup = paste("<strong>Conservation Buffer Zone</strong>", "<br>",
                            # "Site Name:", df$sitename, "<br>",
                            # "Management:", df$management, "<br>",
                            #"Conservation Area (Acres):", round(df$acres, 0), "<br>",
                            "Beneficiary Zone (KM^2):", round(df$sqkm_bz, 0), "<br>",
                            "Beneficiary Zone Radius (KM):", df$bzone_m/1000, "<br>",
                            "Land (%):", 100*df$pland, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "Black (%):", 100*df$pblack, "<br>",
                            "Other race (%):", 100*df$pother, "<br>",
                            "Latinx (%):", 100*df$platinx, "<br>",
                            "White (%):", 100*df$pwhite, "<br>",
                            "Estimated Median HH Income (US$):", round(df$emedhhinc, 0), "<br>",
                            "Estimated Mean HH Income (US$):", df$mnhhinc),
                            # "GAP Status:", df$gap, "<br>",
                            # "Purpose:", df$purpose, "<br>",
                            # "Data Source:", df$source),
              group = "Buffered Conservation Zones",
              fillColor = ~dpal(df$conscat),
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolygons(data = cl,
              popup = paste("<strong>Conservation Reserve Info</strong>", "<br>",
                            # "Site Name:", df$sitename, "<br>",
                            # "Management:", df$management, "<br>",
                            #"Conservation Area (Acres):", round(df$acres, 0), "<br>",
                            "Beneficiary Zone (KM^2):", round(df$sqkm_bz, 0), "<br>",
                            "Beneficiary Zone Radius (KM):", df$bzone_m/1000, "<br>",
                            "Land (%):", 100*df$pland, "<br>",
                            "People of Color (%):", 100*df$propPOC, "<br>",
                            "Black (%):", 100*df$pblack, "<br>",
                            "Other race (%):", 100*df$pother, "<br>",
                            "Latinx (%):", 100*df$platinx, "<br>",
                            "White (%):", 100*df$pwhite, "<br>",
                            "Estimated Median HH Income (US$):", round(df$emedhhinc, 0), "<br>",
                            "Estimated Mean HH Income (US$):", df$mnhhinc),
              group = "Conservation Reserves",
              fillColor = ~cpal(cl$conscat),
              fillOpacity = 1,
              weight = 1) %>%
  addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery"), 
      overlayGroups = c("Conservation Reserves", "Buffered Conservation Zones", "Beneficiary Zones"),
      options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright",
            pal = dpal,
            values = df$conscat,
            title = "Conservation Area") %>%
  addScaleBar("bottomright")
m

## exporting as html file for exploration
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/easement-justice/lowcountry_conservation.html",
           title = "EJ Analysis of Lowcountry Conservation")


