## this script calculates the median from grouped (aka binned) household income data from the census

## good explanation for how this works mathematically and upon which the function below is based
## https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

rm(list=ls())
#options(warn = -1)

# define variables 
YR <- 2016
ST <- 'Georgia'

## load libraryies
library(tidycensus)
library(tidyverse)

## download hh income distribution tables for block groups & label bins
df <- get_acs(geography = "block group",
              table = 'B19001',
              state = ST,
              county = 'Chatham',
              year = YR) %>%
  select(-NAME, -moe) %>%
  rename(households = estimate) %>%
  filter(variable != 'B19001_001') %>%
  mutate(bin_min = ifelse(variable == 'B19001_002', 0, 
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
                                                                                                                                       ifelse(variable == 'B19001_017', 200000, variable)))))))))))))))),
         bin_max = ifelse(variable == 'B19001_002', 9999, 
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
                                                                                                                                       ifelse(variable == 'B19001_017', NA, variable))))))))))))))))) %>%
  mutate(interval = paste(bin_min, bin_max, sep = "-"),
         GEOID = as.factor(GEOID))

## define function following stackoverflow post
# https://stackoverflow.com/questions/18887382/how-to-calculate-the-median-on-grouped-dataset
## but revised per variables from 
# https://www.mathsisfun.com/data/frequency-grouped-mean-median-mode.html

GMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }
  
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of the group containing the median 
  w <- diff(intervals[, Midrow]) # width of median class
  G <- frequencies[Midrow]       # frequency of median class
  B <- ifelse(Midrow > 1, cf[Midrow - 1], as.vector(0))  # cumulative frequency of the groups before median group
  n_2 <- max(cf)/2               # total observations divided by 2
  
  unname(L + (n_2 - B)/G * w)
}

# df2 <- df[1:84,]
# 
# frequencies <- df$households[85:3280]
# intervals <- df$interval[1:84]
# sep <- "-"
# trim <- "cut"
# i2 <- if (!is.null(sep)) {
#   if (is.null(trim)) pattern <- ""
#   else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
#   else pattern <- trim
#   intervals <- sapply(strsplit(gsub(pattern, "", intervals), '-'), as.numeric)
# }
  
## subset data for exploration of mean and median calc
# test <- df[df$GEOID %in% unique(df$GEOID)[135:200],]

# ## from Jessica, but realized didn't need for loop and that function 
# ## would work in dplyr pipe after mods to function
# for(i in 1:length(unique(df$GEOID))){
#   xx <- filter(df, GEOID == unique(df$GEOID)[i])
#   outt <- GMedian(xx$households, xx$interval, sep = "-", trim = "cut")
#   print(i)
#   print(outt)
#   }

df2 <- df %>%
  select(GEOID, households, interval) %>%
  group_by(GEOID) %>%
  filter(sum(households) > 0) %>%
  summarise(gmedian = GMedian(households, interval, sep = "-", trim = "cut"))






############################################
## possible alternative ways to do this
############################################

# http://tillt.net/grouped-median-function-for-r/

## a much more involved way following von Hippel et al 2017
# https://cran.r-project.org/web/packages/binsmooth/index.html

library(binsmooth)
# library(binequality) # see von Hippel et al 2016

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


