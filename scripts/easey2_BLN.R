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
ease$acres.sc <- ease$acres/1e4 #scale(ease$acres)

ease <- ease[ease$conscat != "UNK",]

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

preds <- c("acres.sc", "emedhhinc.sc", "state", "new.popden", "pblack")
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
#              acres.sc emedhhinc.sc new.popden.sc pblack
#acres.sc          1.00         0.00         -0.03  -0.01
#emedhhinc.sc      0.00         1.00          0.32  -0.60
#new.popden.sc    -0.03         0.32          1.00  -0.10
#pblack           -0.01        -0.60         -0.10   1.00


cor.thresh <- 0.595

cor.tri <- lower.tri(pred.cor)

prob.rows <- row(pred.cor)[abs(cor.tri*pred.cor) >= cor.thresh]
prob.cols <- col(pred.cor)[abs(cor.tri*pred.cor) >= cor.thresh]
cor.problem <- cbind(rownames(pred.cor)[prob.rows], colnames(pred.cor)[prob.cols])


###  Means of numeric predictors, in cross-classified levels:
tab.lst <- lapply(pred.num, FUN = tapply, INDEX = pred.cat, mean)   ### 
###  Only one categorical predictor, at the moment.
lapply(tab.lst, FUN = round, 2)

###  Only one categorical predictor, at the moment.
#$acres.sc
#   GA    SC 
# 0.03 -0.02 

#$emedhhinc.sc
#  GA   SC 
#4.23 4.45 

#$new.popden.sc
#   GA    SC 
# 0.04 -0.02 

#$pblack
#  GA   SC 
#0.36 0.35 


###  Do ANOVAs to see if state should be forbidden with any of the above: income looks suspect, in particular...

aov.lst <- sapply(pred.cat.nms, FUN = one.way, dat = pred.df, num.preds = pred.num.nms, simplify = F)   ### 

lapply(aov.lst, FUN = p.mat)
####  following are p-values for one-way anovas:
#$state
#     acres.sc  emedhhinc.sc new.popden.sc        pblack 
#         0.03          0.00          0.01          0.12 

###  So, looks like we don't want state mixed with [popden] or [pblack].
forbid <- rbind(cor.problem,
                matrix(c("acres.sc", "state",
                         "emedhhinc.sc", "state",
                         "new.popden.sc", "state"#,
##  As of  13 Sep, the favored model has state*pblack and I(pblack^2) -- but the predictive value according to 5-fold cv shows no advantage.  So, I think perhaps they're pretty correlated.
##                         "pblack", "state"
                         ), ncol = 2, byrow = T))


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

## allowing pblack and state:
#summary(fit.lst[[28]])

## disallowing pblack and state:
summary(fit.lst[[54]])

## fit with unscaled preds:
valid.forms[54]
#[1] "conscat.bin ~ acres.sc + new.popden.sc + pblack + I(acres.sc^2) + new.popden.sc:pblack"


source('./scripts/cplot.R')
#source('./scripts/cplot_intrxn.R')

cplot(mod = fit.lst[[54]],
      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_54_acresXpblack',
#      intxns = "all",
#      sub = NULL,
      focal.preds = c("pblack", "acres.sc"),
      focal.nms = c("Proportion black", "Reserve size (10k acres)"),
      use.whole.range = F,
      focal.quants = c(0.1, 0.99),
      cond.preds = "new.popden",
      cond.nms = "Population density (km^-2)",
      cond.fac.nms = paste0(c("1", "5", "9"), "0th percentile"),
      plot.title = "",
      inch = 5,
      png.res = 400)
      
      

cplot(mod = fit.lst[[54]],
      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_54_pblackXpopden',
      focal.preds = c("pblack", "new.popden"),
      focal.nms = c("Proportion black", "Population density (km^-2)"),
      use.whole.range = T,
#      focal.quants = c(0.1, 0.9),
      cond.preds = "acres.sc",
      cond.nms = "Reserve size (10k acres)",
      cond.fac.nms = paste0(c("10", "50", "90", "99"), "th percentile"),
#      cond.fac.nms = NULL,
      cond.quants = c(0.1, 0.5, 0.9, 0.99),
      plot.title = "",
      inch = 5,
      png.res = 400)
      
      


#####  This is when allowing pblack and state in same model:

cplot(mod = fit.lst[[73]],
      file.nm ='/home/nuse/helping_folks/Dean_easements/git/easement_justice/figs/mod_73_pblackXpopden',
      focal.preds = c("pblack", "new.popden"),
      focal.nms = c("Proportion black", "Population density (km^-2)"),
      use.whole.range = T,
#      focal.quants = c(0.1, 0.9),
      cond.preds = "state",
      cond.nms = "State",
      cond.fac.nms = c("GA", "SC"),
#      cond.quants = c(0.1, 0.5, 0.9, 0.99),
      plot.title = "",
      inch = 5,
      png.res = 400)
      
      



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
