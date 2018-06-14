#########################################
## data exploration
#########################################
boxplot(propPOC~type, df, notch = TRUE)

df2 <- gather(df, "race", "perc", 5:8)

# ggplot(df2) +
#   geom_point(aes(type, perc))



####################################
## statistical analysis
####################################
# http://rcompanion.org/handbook/G_06.html
# https://magesblog.com/post/2015-08-04-generalised-linear-models-in-r/

library(lme4)
library(emmeans)
library(multcompView)

## convert to table
bz_data <- df %>% st_set_geometry(NULL) %>% data.frame()
# write.csv(bz_data, 'data/bz_data.csv', row.names = FALSE)

bz2 <- bz_data %>% filter(type == 'Easement')
mean(bz2$platinx)

hist(bz_data$pblack)

model <- lmer(pblack ~ type + (type | state), data = bz_data)
marginal <- lsmeans(model, ~type)
cld(marginal, details = TRUE, alpha = 0.1, Letters = letters, adjust = 'bonferroni')