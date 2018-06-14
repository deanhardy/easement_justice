rm(list=ls())

library(lme4)
library(emmeans)
library(multcompView)

## import data
df <- st_read('data/bz_data_erp.geojson') %>%
  st_set_geometry(NULL) %>%
  mutate(state = as.character(state)) %>%
  filter(str_detect(state, 'GA|SC'))

#########################################
## data exploration
#########################################
boxplot(propPOC~type, df, notch = TRUE)

df2 <-filter(df, type == 'Easement')
hist(df2$mnmdhhinc)

####################################
## statistical analysis
####################################

## linear regression model with no mixed effects
model <- lm(mnmdhhinc ~ type, data = df)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')

## linear mixed effects model with state jurisdiction as categorial random effect
model <- lmer(mnmdhhinc ~ type + (type | state), data = df)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')

## note that I'm finding significant differences with state as random effect, but not without it

## some useful resources
# http://rcompanion.org/handbook/G_06.html
# https://magesblog.com/post/2015-08-04-generalised-linear-models-in-r/
