rm(list=ls())

library(lme4)
library(emmeans)
library(multcompView)

## import data
df <- st_read('data/bz_data_erp.geojson') %>%
  st_set_geometry(NULL)

#########################################
## data exploration
#########################################
boxplot(propPOC~type, df, notch = TRUE)

df2 <-filter(df, type == 'Easement')
hist(log(df2$platinx))

####################################
## statistical analysis
####################################
# http://rcompanion.org/handbook/G_06.html
# https://magesblog.com/post/2015-08-04-generalised-linear-models-in-r/

model <- lmer(mnmdhhinc ~ type + (type | state), data = df)
lsm <- lsmeans(model, ~type)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')
