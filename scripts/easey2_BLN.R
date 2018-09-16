##  Starting in on easements.
##
##
##  BLN, 02 jul 2018; cont'd 17 Aug 2018
########################################

source("~/cons_land_dir")

source( './scripts/easey_funs.R')

ease <- read.csv(file='./data/cons_data.csv', stringsAsFactors = F)

ease$emedhhinc.sc <- ease$emedhhinc/1e4
ease$new.popden <- with(ease, tot_pop/sqkm_buf)
#ease$new.popden.sc <- scale(ease$new.popden)
ease$area_km2 <- ease$acres*0.00404686

###  For now, filter to Low Country: ecoreg = 1. (2 includes Okefenokee; 3 is whole coastal plain)
###  based on talking w/ Dean and Levi, 14 Sep 2018.
###  and, 15 sep, Dean says filter rows whose area is <= 5 acres -- or maybe <= 1 ac, but I like the former, I think.
ease <- ease[ease$conscat != "UNK" & ease$ecorg_tier == 1 & ease$acres <= 5,]

stopifnot(ease$conscat %in% c("Public", "Private"))
ease$conscat.bin <- ifelse(ease$conscat == "Private", 0, 1)

ease$state <- factor(ease$state)


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


resp <- "conscat.bin"

preds <- c("area_km2", "emedhhinc.sc", "state", "new.popden", "pblack", "propPOC")
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
#             area_km2 emedhhinc.sc new.popden pblack propPOC
#area_km2         1.00        -0.05       0.00   0.08    0.09
#emedhhinc.sc    -0.05         1.00       0.04  -0.67   -0.63
#new.popden       0.00         0.04       1.00   0.03    0.13
#pblack           0.08        -0.67       0.03   1.00    0.95
#propPOC          0.09        -0.63       0.13   0.95    1.00


cor.thresh <- 0.6

cor.tri <- lower.tri(pred.cor)

prob.rows <- row(pred.cor)[abs(cor.tri*pred.cor) >= cor.thresh]
prob.cols <- col(pred.cor)[abs(cor.tri*pred.cor) >= cor.thresh]
cor.problem <- cbind(rownames(pred.cor)[prob.rows], colnames(pred.cor)[prob.cols])


###  Means of numeric predictors, in cross-classified levels:
tab.lst <- lapply(pred.num, FUN = tapply, INDEX = pred.cat, mean)   ### 
###  Only one categorical predictor, at the moment.
lapply(tab.lst, FUN = round, 2)

###  Only one categorical predictor, at the moment.
#$area_km2
#  GA   SC 
#0.01 0.01 

#$emedhhinc.sc
#  GA   SC 
#4.95 5.32 

#$new.popden
#    GA     SC 
#160.26 135.31 

#$pblack
#  GA   SC 
#0.34 0.27 

#$propPOC
#  GA   SC 
#0.46 0.35 

###  Do ANOVAs to see if state should be forbidden with any of the above: income looks suspect, in particular...

aov.lst <- sapply(pred.cat.nms, FUN = one.way, dat = pred.df, num.preds = pred.num.nms, simplify = F)   ### 

lapply(aov.lst, FUN = p.mat)
####  following are p-values for one-way anovas:
#$state
#    area_km2 emedhhinc.sc   new.popden       pblack      propPOC 
#        0.88         0.00         0.00         0.00         0.00 


###  So, looks like we don't want state mixed with [popden] or [pblack].
forbid <- rbind(cor.problem,
                cbind("state",c("emedhhinc.sc", "new.popden", "pblack", "propPOC")))


max.terms <- 5


main.combs <- sapply(1:length(preds), FUN = function (i) combn(preds, m = i))

valid.main.combs <- (tmp <- lapply(main.combs, FUN = filtr))[!sapply(tmp, FUN = is.null)]

valid.main.lst <- unlist(lapply(valid.main.combs, FUN = function (mat) { c(data.frame(mat, stringsAsFactors = F, fix.empty.names = F)) } ), recursive = F)
names(valid.main.lst) <- NULL

quad.lst <- lapply(valid.main.lst, FUN = v.quad)

intxn.lst <- lapply(valid.main.lst, FUN = v.intxn)


all.rhs <- c("1", 
             unlist(sapply(1:length(valid.main.lst), FUN = function (i) {
               main <- paste(valid.main.lst[[i]], collapse = " + ")
               quads <- if (is.null(quad.lst[[i]])) {
                          main } else { c(main, paste(main, quad.lst[[i]], sep = " + ")) }
               sapply(quads, FUN = function (mod) { if (is.null(intxn.lst[[i]])) {
                                                      mod } else { paste(mod, intxn.lst[[i]], sep = " + ") }} )
             })))

names(all.rhs) <- NULL  

valid.rhs <- all.rhs[ sapply(all.rhs, FUN = function (chr) { length(unlist(strsplit(x = chr, split = "+", fixed = T))) }) <= max.terms ] 
#valid.rhs <- unlist(lapply(valid.combs, FUN = function (mat) { apply(mat, MAR = 2, FUN = paste, collapse = " + ") }))
valid.forms <- paste(resp, valid.rhs, sep = " ~ ")
 
length(valid.forms)

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

###  so this asks, "how much of the time was the model RIGHT?"  And threshold is 0.5
cst.fun <- function(r, pi) mean(abs(r-pi) < 0.5)

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
#levels(ease$pub.priv)
# [1] "Private" "Public" 
###  so the prediction is "probability that the conservation land is public".


#####  OK, 15 sep 2018: this is having filtered all reserves <= 5 acres, and using the dataset sent today by Dean:


#Confidence set, and friends:
#                                                                                         mod mod.num     AIC AIC.wt cv.5_fold
#1  new.popden + propPOC + I(new.popden^2) + I(propPOC^2) + new.popden:propPOC                     42 2236.76   0.94      0.67
#2  new.popden + pblack + I(new.popden^2) + I(pblack^2) + new.popden:pblack                        38 2242.43   0.06      0.66

summary(fit.lst[[42]])
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.4606  -1.2294   0.6821   0.9871   1.1866  

#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.822e+00  4.828e-01   5.845 5.06e-09 ***
#new.popden         -6.156e-05  2.943e-03  -0.021  0.98331    
#propPOC            -1.173e+01  2.305e+00  -5.089 3.60e-07 ***
#I(new.popden^2)     2.504e-05  4.599e-06   5.444 5.21e-08 ***
#I(propPOC^2)        1.404e+01  2.841e+00   4.940 7.79e-07 ***
#new.popden:propPOC -1.382e-02  4.709e-03  -2.934  0.00334 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 2369.1  on 1841  degrees of freedom
#Residual deviance: 2224.8  on 1836  degrees of freedom
#AIC: 2236.8

#Number of Fisher Scoring iterations: 4


#source('./scripts/cplot.R')


#cplot(mod = fit.lst[[42]],
#      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_38_acresXpblack',
##      intxns = "all",
##      sub = NULL,
#      focal.preds = c("pblack", "new.popden"),
#      focal.nms = c("Proportion black", "Population density (people km^-2)"),
#      use.whole.range = T,
##      focal.quants = c(0.1, 0.99),
#      cond.preds = NULL,
#      cond.nms = "Population density (km^-2)",
#      cond.fac.nms = paste0(c("1", "5", "9"), "0th percentile"),
#      plot.title = "",
#      inch = 5,
#      png.res = 400)


source('./scripts/cplot_intrxn.R')

cplot.intxn(mod = fit.lst[[42]],
            intxns = "all",
#            intxn.nms = "Population density (people km^-2):Proportion people of color",
            sub = list(c("new.popden", "Population~density~(people~km^-2)"), c("propPOC", "Proportion~people~of~color")),
            plot.title = "",
            file.nm = '/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_42_propPOC_x_popden',
            inch = 6)


###################################################################
###################################################################
###################################################################




## allowing pblack and state:
#summary(fit.lst[[28]])

### disallowing pblack and state:
#summary(fit.lst[[54]])

### fit with unscaled preds:
#valid.forms[54]
##[1] "conscat.bin ~ acres.sc + new.popden.sc + pblack + I(acres.sc^2) + new.popden.sc:pblack"


#source('./scripts/cplot.R')
##source('./scripts/cplot_intrxn.R')

#cplot(mod = fit.lst[[54]],
#      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_54_acresXpblack',
##      intxns = "all",
##      sub = NULL,
#      focal.preds = c("pblack", "acres.sc"),
#      focal.nms = c("Proportion black", "Reserve size (10k acres)"),
#      use.whole.range = F,
#      focal.quants = c(0.1, 0.99),
#      cond.preds = "new.popden",
#      cond.nms = "Population density (km^-2)",
#      cond.fac.nms = paste0(c("1", "5", "9"), "0th percentile"),
#      plot.title = "",
#      inch = 5,
#      png.res = 400)
#      
#      

#cplot(mod = fit.lst[[54]],
#      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_54_pblackXpopden',
#      focal.preds = c("pblack", "new.popden"),
#      focal.nms = c("Proportion black", "Population density (km^-2)"),
#      use.whole.range = T,
##      focal.quants = c(0.1, 0.9),
#      cond.preds = "acres.sc",
#      cond.nms = "Reserve size (10k acres)",
#      cond.fac.nms = paste0(c("10", "50", "90", "99"), "th percentile"),
##      cond.fac.nms = NULL,
#      cond.quants = c(0.1, 0.5, 0.9, 0.99),
#      plot.title = "",
#      inch = 5,
#      png.res = 400)
#      
#      


######  This is when allowing pblack and state in same model:

#cplot(mod = fit.lst[[73]],
#      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_73_pblackXpopden',
#      focal.preds = c("pblack", "new.popden"),
#      focal.nms = c("Proportion black", "Population density (km^-2)"),
#      use.whole.range = T,
##      focal.quants = c(0.1, 0.9),
#      cond.preds = "state",
#      cond.nms = "State",
#      cond.fac.nms = c("GA", "SC"),
##      cond.quants = c(0.1, 0.5, 0.9, 0.99),
#      plot.title = "",
#      inch = 5,
#      png.res = 400)
#      
#      



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
