##  Starting in on easements.
##
##
##  BLN, 02 jul 2018; cont'd 17 Aug 2018
########################################

source("~/cons_land_dir")

ease <- read.csv(file='./data/cons_data.csv', stringsAsFactors = F)

#### empirical cumulative distributions
#plot.ecdf(ecdf(ease$emedhhinc[ease$type == "Easement"]), col = "green")
#plot.ecdf(ecdf(ease$emedhhinc[ease$type == "Federal"]), col = "red", add = T)
#lines(ecdf(ease$emedhhinc[ease$type == "State"]), col = "purple")

#her <- function (val, v, brk) {
#  hist(v[ease$type == val], plot = F, breaks = brk)
#}

#bper <- function (v) {
#  rng <- range(v)
#  brk <- seq(rng[1], rng[2], length.out = 20)
#  lst <- sapply(c("Federal", "State", "Easement"), FUN = her, v = v, brk = brk, simplify = F)
#  ct.mat <- sapply(lst, FUN = getElement, name = 'counts')
#  barplot(ct.mat, beside = T)
#}


###  it appears that there's no discernible difference between the types, along 'emedhhinc'

###################################################################
#17 Aug 2018--

###  Ok, take a look again at the fields so far:
#t(ease[1,])


###  Ok, I think one hurdle is the imbalance in the dataset:
#table(ease$type)
#Easement  Federal    State 
#     418       12      106 

## Of course, once Dean has information on the easement class, things might improve.  But still, 12 is a very small number compared to 106.

## One option here might be a bootstrapping / data partitioning approach.
## What if I just take a sample of 20 cases from each of the easement and state classes, and leave all 12 federal cases in, each time?

#part <- function (df, col = "type", rare = "Federal", n.othr) {
#  stopifnot(rare %in% df[[col]])
#  u <- unique(df[[col]])
#  othr <- u[!(u %in% rare)]
#  ind.mat <- sapply(othr, FUN = function (val) { sample(which(df[[col]] == val), size=n.othr) })
#  ind.rare <- which(df[[col]] == rare)  
#  return(c(ind.rare, ind.mat))
#}


ease$emedhhinc.sc <- ease$emedhhinc/10000
ease$pub.priv <- as.factor(ifelse(ease$type == "Easement", "Private", "Public"))

#system.time(
#coef.mat <- t(sapply(1:1000, FUN = function(i) { coef(glm(I(emedhhinc-mean(ease$emedhhinc)) ~ type * acres, data = ease[part(ease,n.othr=100),])) }))
#)

#coef.mn <- colMeans(coef.mat)
#coef.sd <- apply(coef.mat, MAR = 2, FUN = sd)

#coef.sum <- data.frame(mean = coef.mn, sd = coef.sd)

####  all cases at once:
#boxplot(I(emedhhinc-mean(ease$emedhhinc)) ~ type, data = ease)

#g <- glm(emedhhinc.sc ~ type * acres * state, data = ease)


######  OK, Clint says imbalance in the 'type' categorical predictor should not be an issue...
######  So, just try a number of models:


##################################################################
##################################################################
##################################################################
##################################################################
##  07 sep 2018--
##  Talked with Levi and Dean today.
##  We looked at some models that took [emedhhinc]; and by the end of the conversation, 
##  1. Dean derided PCA, saud he 'hates it.'
##  2. Decided that [distance-to-easement] would be a good response variable to consider -- however, Dean can't get that together right at the moment. 
##  3. In the meantime, a second response that Dean favored, was "Public / Private" easement type.
##  4. The broad hypotheses to be included / compared are "race" and "class".
##  5. I said I'd include quadratic terms.
##  6. I posited that we might want to limit the number of terms -- I'll try five first.
##  


resp <- "pub.priv"

ease$new.popden <- with(ease, tot_pop/sqkm_buf)
ease$new.popden.sc <- scale(ease$new.popden)
ease$acres.sc <- scale(ease$acres)

preds <- c("acres.sc", "emedhhinc.sc", "state", "new.popden.sc", "pblack")
#intxn.mat <- matrix(preds[combn(x = length(preds), m = 2)], nrow = 2)
#intxns <- apply(intxn.mat, MAR = 2, FUN = paste, collapse = ":")

pred.df <- ease[,preds]
num.cols <- sapply(pred.df, is.numeric)
pred.num.nms <- preds[num.cols]
pred.cat.nms <- preds[!num.cols]

pred.num <- pred.df[,pred.num.nms]
pred.cat <- pred.df[,pred.cat.nms]

###  How many rows, in each cross-classified category?
# xtabs(~., data = pred.cat)
###  Only one categorical predictor, at the moment.

###  Look for correlations in predictors:
round(pred.cor <- cor(pred.num,), 2)
#              acres.sc emedhhinc new.popden.sc pblack
#acres.sc          1.00      0.02         -0.03  -0.08
#emedhhinc         0.02      1.00          0.43  -0.68
#new.popden.sc    -0.03      0.43          1.00  -0.23
#pblack           -0.08     -0.68         -0.23   1.00


cor.thresh <- 0.6

cor.tri <- lower.tri(pred.cor)

prob.rows <- row(pred.cor)[abs(cor.tri*pred.cor) > cor.thresh]
prob.cols <- col(pred.cor)[abs(cor.tri*pred.cor) > cor.thresh]
cor.problem <- cbind(rownames(pred.cor)[prob.rows], colnames(pred.cor)[prob.cols])


###  Means of numeric predictors, in cross-classified levels:
tab.lst <- lapply(pred.num, FUN = tapply, INDEX = pred.cat, mean)   ### 
###  Only one categorical predictor, at the moment.
lapply(tab.lst, FUN = round, 2)

###  Only one categorical predictor, at the moment.
#$acres.sc
#   GA    SC 
# 0.06 -0.05 

#$emedhhinc.sc
#  GA   SC 
#3.95 4.09 

#$new.popden.sc
#   GA    SC 
#-0.09  0.08 

#$pblack
#  GA   SC 
#0.36 0.44 

###  Do ANOVAs to see if state should be forbidden with any of the above: income looks suspect, in particular...

one.way <- function (cat, dat, num.preds) {
  sapply(num.preds, FUN = function (pr) {lm(paste(pr, cat, sep = "~"), data = dat)}, simplify = F)
  }
  
aov.lst <- sapply(pred.cat.nms, FUN = one.way, dat = pred.df, num.preds = pred.num.nms, simplify = F)   ### 

p.get <- function (mod) {
  summary(aov(mod))[[1]]$`Pr(>F)`[1]
}
###  hmm, rapply is failing me here, apparently because objects of class "lm" are lists!  How useless.
# rapply(aov.lst, f = p.get, classes = "lm")   ## does not work.

###  So:
p.mat <- function (lst) {
  round(sapply(lst, FUN = p.get),2)
}

lapply(aov.lst, FUN = p.mat)
#$state
#     acres.sc  emedhhinc.sc new.popden.sc        pblack 
#         0.19          0.17          0.06          0.00 

###  So, looks like we don't want state mixed with [popden] or [pblack].
forbid <- rbind(cor.problem,
                matrix(c("new.popden.sc", "state",
                         "pblack", "state"), ncol = 2, byrow = T))

add.combs <- sapply(1:length(preds), FUN = function (i) combn(preds, m = i))

v.combs <- function (v) {
  comb.ind <- combn(length(v), m = 2)
  apply(comb.ind, MAR = 2, FUN = function (inds) { paste(v[inds], collapse = ":") })
  }

filt.test <- function (test.v, pred.v) {
  test.v[1] %in% pred.v && test.v[2] %in% pred.v
}

mat.combs <- function (mat) {
  mat.filt <- apply(mat, MAR = 2, FUN = function (pred.v) { any(apply(forbid, MAR = 1, FUN = filt.test, pred.v = pred.v)) })
  
  new.mat <- mat[,!mat.filt, drop = F]
  if (dim(new.mat)[2] > 0) {
    if (dim(new.mat)[1] > 1) {
      intxns <- apply(new.mat, MAR = 2, FUN = v.combs)
      out <- rbind(new.mat, intxns)
      } else {
        out <- new.mat
      }
  } else {
    out <- NULL
  }
  return(out)
}

valid.combs <- (tmp <- lapply(add.combs, FUN = mat.combs))[!sapply(tmp, FUN = is.null)]
#valid.combs[length(valid.combs) + 1] <- 1

valid.rhs <- unlist(lapply(valid.combs, FUN = function (mat) { apply(mat, MAR = 2, FUN = paste, collapse = " + ") }))
valid.forms <- paste(resp, valid.rhs, sep = " ~ ")
 

fit.lst <- lapply(valid.forms, FUN = glm, data = ease, family = binomial)

aic.tab <- data.frame(mod = valid.rhs, mod.num = 1:length(fit.lst), AIC = sapply(fit.lst, FUN = AIC))

aic.tab$delta.AIC <- aic.tab$AIC - min(aic.tab$AIC)
aic.tab$AIC.wt <- {pre.wt <- exp((-aic.tab$delta.AIC)/2); pre.wt/sum(pre.wt)}

aic.tab <- aic.tab[order(aic.tab$AIC.wt, decreasing = T),]

############################  
############################  START cross-validation.
###  assess fit of top models by cv.glm(): 
#library(boot)

###  from the cv.glm() help page examples:
     # leave-one-out and 11-fold cross-validation prediction error for 
     # the nodal data set.  Since the response is a binary variable an
     # appropriate cost function is
     # cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

###  so this asks, "how much of the time was the model WRONG?"
cst.fun <- function(r, pi) mean(abs(r-pi) > 0.5)

###  generalize this:
require(boot)
cv.fun <- function (ind, K, forms, dat) {
  remake <- glm(forms[[ind]], data = dat, family = binomial)
  err <- cv.glm(data = dat, glmfit = remake, cost = cst.fun, K = K)
  return(err$delta[2])
}


aic.tab <- cbind(aic.tab, cv.5_fold = sapply(aic.tab$mod.num, FUN = cv.fun, K = 5, forms = valid.forms, dat = ease))

############################  
############################  END cross-validation.

aic.tab.frm <- format(as.data.frame(lapply(aic.tab, FUN = function (v) { if (is.numeric(v)) { round(v,2) } else {v} } ))[,-4], justify = "left")

###  what's being predicted, exactly?  I.e., what's the order of the factor levels?
levels(ease$pub.priv)
# [1] "Private" "Public" 
###  so the prediction is "probability that the conservation land is public".
summary(fit.lst[[6]])

#Call:
#FUN(formula = X[[i]], family = ..2, data = ..1)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.3254  -0.6346  -0.5134  -0.4057   2.1599  

#Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)   
#(Intercept)            -1.4809     0.6451  -2.295  0.02171 * 
#acres.sc               19.1545     6.3725   3.006  0.00265 **
#emedhhinc.sc            0.1428     0.1428   1.001  0.31701   
#acres.sc:emedhhinc.sc  -2.6541     1.3854  -1.916  0.05539 . 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 565.05  on 535  degrees of freedom
#Residual deviance: 467.91  on 532  degrees of freedom
#AIC: 475.91

#Number of Fisher Scoring iterations: 7











########################################## 
##########################################
########################################## 
########################################## 
####    ARCHIVE: 
##########################################

###  exploration:
#anova1 <- glm(emedhhinc ~ state*type, data = ease)
#anova2 <- glm(pblack ~ state*type, data = ease)

#reg1 <- glm(emedhhinc ~ pblack, data = ease)
#reg2 <- glm(emedhhinc ~ pblack + type, data = ease)
#reg3 <- glm(emedhhinc ~ pblack + type + state, data = ease)
#reg4 <- glm(emedhhinc ~ pblack + state, data = ease)
