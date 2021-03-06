rm(list=ls())

library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)
library(htmlwidgets)

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

## union ben zones for clipping bg data (maybe)
bzone <- bz %>%
  st_make_valid() %>%
  st_union()

## import block group data
bg <- st_read(file.path(datadir, 'bg_demg.geojson'))%>%
  filter(st_intersects(t1, ., sparse = F)) %>%
  mutate(popden = total/sqkm_bg, pblack = black/total)

# set color schemes
clpal <- colorFactor(c('darkorchid4', 'darkgreen'), cl$conscat)
bufpal <- colorFactor(c('darkorchid1', 'green'), buf$conscat)
bzpal <- colorFactor(c('darkorchid1', 'green'), bz$conscat)
# bgpal <- colorFactor(gray.colors(5, start = 0.3, end = 0.9, gamma = 1, rev = TRUE), bg$propPOC)
bgpal <- colorQuantile("Greens", bg$pblack, n = 4)
bgdenpal <- colorQuantile("Reds", bg$popden, n = 4)
bgincpal <- colorQuantile("Blues", bg$medhhinc, n = 4)
bgmhvpal <- colorQuantile("Oranges", bg$mhv, n = 4)


# make leaflet map
m <- leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  # addTiles(attribution = '<a href="https://www.conservationeasement.us/"> | NCED</a>') %>%
  # addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  setView(lng = -81, lat = 32.4, zoom = 8) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addPolylines(data = t1,
               color = 'black',
               weight = 3) %>%
  addPolygons(data = bg,
              popup = paste("<strong>Block Group Info</strong>", "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "Total Population: ", round(buf$tot_pop, 0), "<br>",
                            "Population Density (per KM^2): ", round(bg$total/bg$sqkm_bg, 0), "<br>",
                            "People of Color (%):", round(100*buf$propPOC,0), "<br>",
                            "Black (%):", round(100*(bg$black/bg$total), 0), "<br>",
                            # "Other race (%):", 100*(bg$other/bg$total)), "<br>",
                            "Latinx (%):", round(100*(bg$latinx/bg$total), 0), "<br>",
                            "White (%):", round(100*(bg$white/bg$total), 0), "<br>",
                            "Median HH Income (US$):", round(bg$medhhinc, 0), "<br>",
                            "Mean HH Income (US$):", round(bg$mnhhinc, 0)),
              group = "US Census (Black folk)",
              color = 'black',
              fillColor = ~bgpal(bg$pblack),
              fillOpacity = 1,
              weight = 0.5) %>%
  addPolygons(data = bg,
              popup = paste("<strong>Block Group Info</strong>", "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "Total Population: ", round(buf$tot_pop, 0), "<br>",
                            "Population Density (per KM^2): ", round(bg$total/bg$sqkm_bg, 0), "<br>",
                            "People of Color (%):", round(100*buf$propPOC,0), "<br>",
                            "Black (%):", round(100*(bg$black/bg$total), 0), "<br>",
                            # "Other race (%):", 100*(bg$other/bg$total)), "<br>",
                            "Latinx (%):", round(100*(bg$latinx/bg$total), 0), "<br>",
                            "White (%):", round(100*(bg$white/bg$total), 0), "<br>",
                            "Median HH Income (US$):", round(bg$medhhinc, 0), "<br>",
                            "Mean HH Income (US$):", round(bg$mnhhinc, 0)),
              group = "US Census (Income)",
              color = 'black',
              fillColor = ~bgincpal(bg$medhhinc),
              fillOpacity = 1,
              weight = 0.5) %>%
  addPolygons(data = bg,
              popup = paste("<strong>Block Group Info</strong>", "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "Total Population: ", round(buf$tot_pop, 0), "<br>",
                            "Population Density (per KM^2): ", round(bg$total/bg$sqkm_bg, 0), "<br>",
                            "People of Color (%):", round(100*buf$propPOC,0), "<br>",
                            "Black (%):", round(100*(bg$black/bg$total), 0), "<br>",
                            # "Other race (%):", 100*(bg$other/bg$total)), "<br>",
                            "Latinx (%):", round(100*(bg$latinx/bg$total), 0), "<br>",
                            "White (%):", round(100*(bg$white/bg$total), 0), "<br>",
                            "Median HH Income (US$):", round(bg$medhhinc, 0), "<br>",
                            "Mean HH Income (US$):", round(bg$mnhhinc, 0)),
              group = "US Census (MHV)",
              color = 'black',
              fillColor = ~bgmhvpal(bg$mhv),
              fillOpacity = 1,
              weight = 0.5) %>%
  addPolygons(data = bg,
              popup = paste("<strong>Block Group Info</strong>", "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "Total Population: ", round(buf$tot_pop, 0), "<br>",
                            "Population Density (per KM^2): ", round(bg$total/bg$sqkm_bg, 0), "<br>",
                            "People of Color (%):", round(100*buf$propPOC,0), "<br>",
                            "Black (%):", round(100*(bg$black/bg$total), 0), "<br>",
                            # "Other race (%):", 100*(bg$other/bg$total)), "<br>",
                            "Latinx (%):", round(100*(bg$latinx/bg$total), 0), "<br>",
                            "White (%):", round(100*(bg$white/bg$total), 0), "<br>",
                            "Median HH Income (US$):", round(bg$medhhinc, 0), "<br>",
                            "Mean HH Income (US$):", round(bg$mnhhinc, 0)),
              group = "US Census (Density)",
              color = 'black',
              fillColor = ~bgdenpal(bg$popden),
              fillOpacity = 1,
              weight = 0.5) %>%
  addPolylines(data = bz,
               group = "Beneficiary Zones",
               color = ~bzpal(bz$conscat)) %>%
  addPolygons(data = buf,
              popup = paste("<strong>Conservation Buffer Zone</strong>", "<br>",
                            # "Site Name:", df$sitename, "<br>",
                            # "Management:", df$management, "<br>",
                            #"Conservation Area (Acres):", round(df$acres, 0), "<br>",
                            "Beneficiary Zone (KM^2):", round(buf$sqkm_bz, 0), "<br>",
                            "Beneficiary Zone Radius (KM):", buf$bzone_m/1000, "<br>",
                            "Land (%):", 100*buf$pland, "<br>",
                            "Total Population: ", round(buf$tot_pop,0), "<br>",
                            "Population Density (per KM^2): ", buf$popden, "<br>",
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
  addLayersControl(baseGroups = c("US Census (Black folk)", "US Census (Income)", "US Census (MHV)", "US Census (Density)"), 
      overlayGroups = c("Conservation Buffer Zones", "Conservation Reserves", "Beneficiary Zones"),
      options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright",
            pal = bgpal,
            values = bg$pblack,
            group = "US Census (Black folk)",
            title = "Black (%)") %>%
  addLegend("bottomright",
            pal = bgdenpal,
            values = bg$popden,
            group = "US Census (Density)",
            title = "Population Density") %>%
  addLegend("bottomleft",
            pal = bgincpal,
            values = bg$medhhinc,
            group = "US Census (Income)",
            title = "Median Household Income") %>%
  addLegend("bottomleft",
            pal = bgmhvpal,
            values = bg$mhv,
            group = "US Census (MHV)",
            title = "Median House Value") %>%
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
  hideGroup(c("Conservation Reserves", "Beneficiary Zones")) %>%
  addScaleBar("bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")
#m

## exporting as html file for exploration
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/easement-justice/lowcountry_conservation.html",
           title = "Analysis of Lowcountry Conservation")


