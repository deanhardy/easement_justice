rm(list=ls())

library(lme4)
library(emmeans)
library(multcompView)
library(HH)
# library(tidyverse)
# library(sf)


# df <- st_read('data/bz_data_erp.geojson') %>%
#   st_set_geometry(NULL) %>%
#   mutate(state = as.character(state)) %>%
#   filter(str_detect(state, 'GA|SC'))
# 
# write.csv(df, 'data/cons_data.csv')

## import data
df <- read.csv('data/cons_data.csv')

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
model <- glm(mnmdhhinc ~ type + state, data = df)
summary(model)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')

## linear mixed effects model with state jurisdiction as categorial random effect
model <- lmer(mnmdhhinc ~ type + (1 | state), data = df)
summary(model)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')

## note that I'm finding significant differences with state as random effect, but not without it

## some useful resources
# http://rcompanion.org/handbook/G_06.html
# https://magesblog.com/post/2015-08-04-generalised-linear-models-in-r/
# https://blog.cloudera.com/blog/2015/12/common-probability-distributions-the-data-scientists-crib-sheet/

## ancova, but need to check against assumptions, as pretty sure normal dist is violated
model <- ancova(propPOC ~ sqkm_buf + type, data = df)
summary(model)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')


