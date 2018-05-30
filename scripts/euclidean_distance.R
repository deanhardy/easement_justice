rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)
library(lwgeom)

## Albers Conic Equal Area projection
## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## NAD 83 UTM 17N
utm <- 2150

## define acs year and region for census data import
YR <- 2010
# ST <- c("Georgia", "South Carolina")
# CNTY <- c('Richland', 'Bryan')

##############################################################
## data import and prepping
##############################################################

## import lowcountry region boundary
lc <- st_read("data/lowcountry.shp") %>%
  st_transform(utm)

## import NCED data for low country region in SC & GA
private <- st_read("data/nced_lc.shp") %>%
  filter(owntype == 'PVT', 
         purpose %in% c('ENV', 'FOR', 'FARM', 'REC')) %>%
  filter(gapcat %in% c('1','2')) %>%
  mutate(type = "Easement") %>%
  st_transform(crs = utm) %>%
  select(type, state, gis_acres, gapcat, geometry) %>%
  rename(acres = gis_acres, gap = gapcat)

## import PAD-US data for low country region in SC & GA
public <- st_read("data/padus_lc.shp") %>%
  filter(Own_Type %in% c("FED", "STAT"),
         Category != "Easement") %>%
  filter(GAP_Sts %in% c('1', '2')) %>%
  st_transform(crs = utm) %>%
  select(d_Own_Type, State_Nm, GIS_Acres, GAP_Sts, geometry) %>%
  rename(type = d_Own_Type, state = State_Nm, acres = GIS_Acres, gap = GAP_Sts)

fed <- st_read("data/padus_lc.shp") %>%
  filter(Own_Type == "FED",
         Category != "Easement") %>%
  filter(GAP_Sts %in% c('1', '2'))

state <- st_read("data/padus_lc.shp") %>%
  filter(Own_Type == "STAT",
         Category != "Easement") %>%
  filter(GAP_Sts %in% c('1', '2'))

cons <- rbind(private, public) %>%
  rowid_to_column()

# ## re-run if census variables need to be changed
# ## define acs variables of interest
# acs_vars <- c(white = "B03002_003E", black = "B03002_004E",
#                native_american = "B03002_005E", asian = "B03002_006E",
#                hawaiian = "B03002_007E", other = "B03002_008E",
#                multiracial = "B03002_009E", latinx = "B03002_012E")
# 
# ## define decennial variables of interest
# dec_vars <- c(white = "P0050003", black = "P0050004",
#               native_american = "P0050005", asian = "P0050006",
#               hawaiian = "P0050007", other = "P0050008",
#               multiracial = "P0050009", nonlatinx = "P0050002",
#               total = "P0050001")
# 
# ## grab counties to create character vector for data grab
# ## necessary for block data grab, but not for block groups
# ga_cnty <- counties('georgia', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) %>%
#   select('COUNTYFP')
# 
# ga <- get_decennial(geography = "block",
#               variables = dec_vars,
#               state = 'Georgia',
#               county = ga_cnty$COUNTYFP,
#               year = YR,
#               output = 'wide',
#               geometry = TRUE) %>%
#   st_transform(crs = alb) %>%
#   mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
#   dplyr::select(-nonlatinx)
# 
# sc_cnty <- counties('south carolina', cb = TRUE) %>%
#   st_as_sf() %>%
#   st_set_geometry(NULL) #%>%
#   select('COUNTYFP')
# 
# sc <- get_decennial(geography = "block",
#                     variables = dec_vars,
#                     state = 'South Carolina',
#                     county = sc_cnty$COUNTYFP,
#                     year = YR,
#                     output = 'wide',
#                     geometry = TRUE) %>%
#   st_transform(crs = alb) %>%
#   mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
#   select(-nonlatinx)
# 
# bg <- rbind(ga, sc) %>%
#   mutate(prop_POC = 1 - (white/total))
# 
# ## export census data
# bg %>% st_transform(crs = 4326) %>%
# st_write("data/block_data.geojson", driver = 'GEOJson')


## import census data
bg <- st_read("data/bg_data.geojson") %>%
  st_transform(crs = utm)

fig <- tm_shape(bg) +
  tm_fill('prop_POC', palette = "Greys",
          title = "People of Color") +
  tm_shape(cons) +
  tm_fill('type', legend.show = TRUE) +
  tm_shape(lc) +
  tm_borders(col = 'black') +
  tm_layout(title = "Lowcountry Conservation Areas (n = 323)",
            frame = FALSE,
            outer.margins=c(0,0,0,0),
            inner.margins=c(0,0,0,0), asp=0) +
  tm_compass(type = "arrow", size = 5, position = c(0.61, 0.09)) +
  tm_scale_bar(breaks = c(0,100), size = 1.2, position= c(0.57, 0.02)) +
  tm_legend(position = c(0.8, 0.04),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 1.2,
            legend.title.size = 1.5)
fig

tiff('figures/cons_race.tiff', res = 300, units = 'in',
     height = 9, width = 10, compression = 'lzw')
fig
dev.off()



###################################################
## apply proportional area adjustment to variables
## to assess count within buffer zones
###################################################

## euclidean distance buffering
buf <- cons %>%
  st_buffer(dist = 16000) %>%
  mutate(SqKM_BUF = as.numeric(st_area(geometry) / 1e6))

## define intersection between buffer zones and block groups
int <- as.tibble(st_intersection(buf, bg))

## proportional area adjustment method
bz_bg <- int %>%
  mutate(SqKMBGinBUF = as.numeric(st_area(geometry) / 1e6)) %>%
  mutate(PercBGinBUF = (SqKMBGinBUF/SqKM_BG)) %>%
  mutate(tot_pop = total * PercBGinBUF,
         white = white * PercBGinBUF, 
         black = black * PercBGinBUF,
         native_american = native_american * PercBGinBUF,
         asian = asian * PercBGinBUF,
         hawaiian = hawaiian * PercBGinBUF,
         other = other * PercBGinBUF,
         multiracial = multiracial * PercBGinBUF,
         latinx = latinx * PercBGinBUF) %>%
  group_by(rowid) %>%
  select(rowid, tot_pop, white, black, native_american, asian, hawaiian, 
         other, multiracial, latinx, SqKM_BUF) %>%
  summarise(tot_pop = sum(tot_pop), white = sum(white), black = sum(black), 
            native_american = sum(native_american),
            asian = sum(asian), hawaiian = sum(hawaiian), other = sum(other),
            multiracial = sum(multiracial), latinx = sum(latinx),
            SqKM_BUF = mean(SqKM_BUF)) %>%
  mutate(propPOC = (tot_pop - white)/tot_pop) %>%
  merge(cons, by = 'rowid') %>%
  st_as_sf()

df <- bz_bg %>% 
  st_transform(4326)

st_write(bz_bg,'data/bz_race.geojson', driver = 'geojson')
         
## GET EQUATION AND R-SQUARED AS STRING
## SOURCE: http://goo.gl/K4yh

# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }

fig2 <- ggplot(filter(ncedbuf_bg, purpose == 'ENV' & gapcat == '2')) + 
  geom_point(aes(propPOC * 100, log(gis_acres)), color = 'black') +
  geom_point(aes((white/total) * 100, log(gis_acres)), color = 'white') +
  geom_smooth(aes(propPOC * 100, log(gis_acres)), method = 'lm', color = 'black') + 
  geom_smooth(aes((white/total) * 100, log(gis_acres)), method = 'lm', color = 'white')
  # geom_text(x = 25, y = 8000, label = lm_eqn(df), parse = TRUE)
fig2

tiff('figures/proportionPOC_by_easement_size.tif', compression = 'lzw', res = 300,
     height = 5, width = 5, units = 'in')
fig2
dev.off()

M <-lm(ncedbuf_bg$gis_acres ~ ncedbuf_bg$propPOC)
summary(M)


