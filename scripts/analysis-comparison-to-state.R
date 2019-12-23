################################################
## PURPOSE: this script compares block group demographics aggregated to a study site
## to state level demographics (census data downloaded elsewhere)
## BY: Dean Hardy
################################################

rm(list=ls())

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## import data
st <- read.csv(file.path(datadir, 'st_data.csv'))
bg <- read.csv(file.path(datadir, 'cabz_data.csv'))
