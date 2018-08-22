##  Starting in on easements.
##
##
##  BLN, 02 jul 2018; cont'd 17 Aug 2018
########################################

ease <- read.csv(file='/home/nuse/helping_folks/Dean_easements/cons_data.csv', stringsAsFactors = F)

### empirical cumulative distributions
plot.ecdf(ecdf(ease$emedhhinc[ease$type == "Easement"]), col = "green")
plot.ecdf(ecdf(ease$emedhhinc[ease$type == "Federal"]), col = "red", add = T)
lines(ecdf(ease$emedhhinc[ease$type == "State"]), col = "purple")

her <- function (val, v, brk) {
  hist(v[ease$type == val], plot = F, breaks = brk)
}

bper <- function (v) {
  rng <- range(v)
  brk <- seq(rng[1], rng[2], length.out = 20)
  lst <- sapply(c("Federal", "State", "Easement"), FUN = her, v = v, brk = brk, simplify = F)
  ct.mat <- sapply(lst, FUN = getElement, name = 'counts')
  barplot(ct.mat, beside = T)
}


##  it appears that there's no discernible difference between the types, along 'emedhhinc'

##################################################################
17 Aug 2018--

##  Ok, take a look again at the fields so far:
t(ease[1,])


##  Ok, I think one hurdle is the imbalance in the dataset:
table(ease$type)
#Easement  Federal    State 
#     418       12      106 

## Of course, once Dean has information on the easement class, things might improve.  But still, 12 is a very small number compared to 106.

## One option here might be a bootstrapping / data partitioning approach.
## What if I just take a sample of 20 cases from each of the easement and state classes, and leave all 12 federal cases in, each time?

part <- function (df, col = "type", rare = "Federal", n.othr) {
  stopifnot(rare %in% df[[col]])
  u <- unique(df[[col]])
  othr <- u[!(u %in% rare)]
  ind.mat <- sapply(othr, FUN = function (val) { sample(which(df[[col]] == val), size=n.othr) })
  ind.rare <- which(df[[col]] == rare)  
  return(c(ind.rare, ind.mat))
}


ease$emedhhinc.sc <- scale(ease$emedhhinc)

system.time(
coef.mat <- t(sapply(1:1000, FUN = function(i) { coef(glm(I(emedhhinc-mean(ease$emedhhinc)) ~ type * acres, data = ease[part(ease,n.othr=100),])) }))
)

coef.mn <- colMeans(coef.mat)
coef.sd <- apply(coef.mat, MAR = 2, FUN = sd)

coef.sum <- data.frame(mean = coef.mn, sd = coef.sd)

###  all cases at once:
boxplot(I(emedhhinc-mean(ease$emedhhinc)) ~ type, data = ease)

g <- glm(emedhhinc.sc ~ type * acres * state, data = ease)

######  OK, Clint says imbalance in the 'type' categorical predictor should not be an issue...
######  So, just try a number of models:
## Response: emedhhinc.sc
preds <- c("acres", "type", "state", "popden", "pblack")

sapply(1:length(preds), FUN = function (i) combn(preds, m = i))

