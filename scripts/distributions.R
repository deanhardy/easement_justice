library(readr)
library(ggplot2)
library(dplyr)
library(magrittr)

df <- read_csv("data/cons_data.csv")
df_sub <- df[,c("type", "pwhite", "pblack", 
                "pother", "platinx", "propPOC", "mnhhinc")]
names(df_sub)
head(df_sub)
# understanding if conservation land types 
# (federal, state, and private) are predictors of 
# race and household income (my response variables) 
# in the zones surrounding the conservation areas.

# I’d like to add controlling for state jurisdictions 
# (as a categorical random effect) and 
# distance to shoreline (as a continuous covariate). 

# response vars = mnhhinc; pwhite, pblack, pother, platinx, propPOC
# explanatory vars = type, state, distance to shoreline

# empirical cumulative distribution of hhinc by type
ggplot(df_sub, aes(x = mnhhinc, group = type)) +
  stat_ecdf(geom = "line", aes(color = type)) + 
  theme_bw()

# density plot
ggplot(df_sub, aes(x = mnhhinc, group = type)) +
  geom_density(aes(color = type)) +
  theme_bw()

# empirical cumulative distributions of demographics
df_sub %>% 
  tidyr::gather(demographic, proportion, pwhite:propPOC) %>%
  dplyr::filter(demographic != "pother") %>%
  ggplot(aes(x = proportion, group = type)) +
  stat_ecdf(geom = "line", aes(color = type)) +
  facet_wrap(~demographic, scales = "free") +
  theme_bw()

# kernel density plots of deomgraphic distributions
df_sub %>% 
  tidyr::gather(demographic, proportion, pwhite:propPOC) %>%
  dplyr::filter(demographic != "pother") %>%
  ggplot(aes(x = proportion, group = type)) +
  geom_density(aes(color = type)) +
  facet_wrap(~demographic, scales = "free") +
  theme_bw()


  
