rm(list=ls())

library(lme4)
library(emmeans)
library(multcompView)
library(HH)
library(tidyverse)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/easement-justice')

## import data
df <- read.csv(file.path(datadir, 'cabz_data.csv'), stringsAsFactors = F)

#########################################
## data exploration
#########################################
VARS <- c('emedhhinc', 'pblack', 'propPOC', 'pwhite', 'popden')

lapply(VARS, function(z) {
  ggplot(df) + 
  geom_boxplot(aes_string('conscat', z)) + 
  facet_grid(bzone_m ~ buf_m) + 
  ggsave(file.path(datadir, paste('/figures/', 'boxplot_', z, '.png', sep='')))
})

lapply(VARS, function(z) {
  ggplot(df, aes_string(x = z, group = 'conscat')) + 
    geom_density(aes(color = conscat)) + 
    ggsave(file.path(datadir, paste('/figures/', 'density_', z, '.png', sep='')))
})

# density plot
ggplot(df, aes(x = emedhhinc, group = conscat)) +
  geom_density(aes(color = conscat)) +
  theme_bw()

####################################
## statistical analysis
####################################

## linear regression model with no mixed effects
model <- glm(emedhhinc ~ conscat, data = df)
summary(model)
lsm <- lsmeans(model, ~conscat)
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
model <- ancova(propPOC ~ sqkm_buf + conscat, data = df)
summary(model)
lsm <- lsmeans(model, ~conscat)
cld(lsm, details = TRUE, alpha = 0.05, Letters = letters, adjust = 'bonferroni')


