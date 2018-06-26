rm(list=ls())

library(tidyverse)
library(tidycensus)
library(binsmooth)
# library(fdth) # http://tillt.net/grouped-median-function-for-r/

## download hh income distribution tables for block groups
df <- get_acs(geography = "block group",
              table = 'B19001',
              state = 'Georgia',
              county = 'Chatham',
              year = '2016') %>%
  select(-NAME, -moe) %>%
  rename(households = estimate) %>%
  filter(variable != 'B19001_001') %>%
  mutate(bin_min = as.numeric(ifelse(variable == 'B19001_002', 0, 
                                     ifelse(variable == 'B19001_003', 10000, 
                                            ifelse(variable == 'B19001_004', 15000,
                                                   ifelse(variable == 'B19001_005', 20000,
                                                          ifelse(variable == 'B19001_006', 25000,
                                                                 ifelse(variable == 'B19001_007', 30000,
                                                                        ifelse(variable == 'B19001_008', 35000,
                                                                               ifelse(variable == 'B19001_009', 40000,
                                                                                      ifelse(variable == 'B19001_010', 45000,
                                                                                             ifelse(variable == 'B19001_011', 50000,
                                                                                                    ifelse(variable == 'B19001_012', 60000,
                                                                                                           ifelse(variable == 'B19001_013', 75000,
                                                                                                                  ifelse(variable == 'B19001_014', 100000,
                                                                                                                         ifelse(variable == 'B19001_015', 125000,
                                                                                                                                ifelse(variable == 'B19001_016', 150000,
                                                                                                                                       ifelse(variable == 'B19001_017', 200000, variable))))))))))))))))),
         bin_max = as.numeric(ifelse(variable == 'B19001_002', 9999, 
                                     ifelse(variable == 'B19001_003', 14999, 
                                            ifelse(variable == 'B19001_004', 19999,
                                                   ifelse(variable == 'B19001_005', 24999,
                                                          ifelse(variable == 'B19001_006', 29999,
                                                                 ifelse(variable == 'B19001_007', 34999,
                                                                        ifelse(variable == 'B19001_008', 39999,
                                                                               ifelse(variable == 'B19001_009', 44999,
                                                                                      ifelse(variable == 'B19001_010', 49999,
                                                                                             ifelse(variable == 'B19001_011', 59999,
                                                                                                    ifelse(variable == 'B19001_012', 74999,
                                                                                                           ifelse(variable == 'B19001_013', 99999,
                                                                                                                  ifelse(variable == 'B19001_014', 124999,
                                                                                                                         ifelse(variable == 'B19001_015', 149999,
                                                                                                                                ifelse(variable == 'B19001_016', 199999,
                                                                                                                                       ifelse(variable == 'B19001_017', NA, variable))))))))))))))))))

## subset data for exploration of mean and median calc
df2 <- df[1:16,]

##
sb <- stepbins(df2$bin_max, df2$households)
splb <- splinebins(df2$bin_max, df2$households)
splb[["est_mean"]]

## plot spline of PDF and stepPDF
plot(splb$splinePDF, 0, 50000, n=500)
plot(sb$stepPDF, do.points=FALSE, col="gray", add=TRUE)
# notice that the curve preserves bin area

library(pracma)
integral(splb$splinePDF, 0, splb$E)
integral(function(x){1-splb$splineCDF(x)}, 0, splb$E)
# splb <- splinebins(df2$bin_max, df2$households, [insert mean] numIterations = 100)
# integral(function(x){1-splb$splineCDF(x)}, 0, splb$E) # closer to given mean

