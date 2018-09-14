##  Functions to support Conservation Lands analysis.
##
##
##
##  12 sep 2018
###########################################


###  Do ANOVAs to see if a categorical variable is related to a continuous one:

one.way <- function (cat, dat, num.preds) {
  sapply(num.preds, FUN = function (pr) {lm(paste(pr, cat, sep = "~"), data = dat)}, simplify = F)
  }

p.get <- function (mod) {
  summary(aov(mod))[[1]]$`Pr(>F)`[1]
}

p.mat <- function (lst) {
  round(sapply(lst, FUN = p.get),2)
}



#################################################################
#################################################################


###  The following functions set up the set of models to be fit.
###  These help accomplish the following:
###  1. Models containing forbidden pairs are removed.
###  2. Quadratic terms are added.
###  3. Interactions are added.
###  4. Models are filtered accroding to the number of terms (where the effect of a categorical variable are considered "one term").

###  Get the possible quadratic amendments: this will be applied to vectors representing additive models
v.quad <-  function (v) {
  v <- v[v %in% pred.num.nms]
  if (length(v) > 0) {
    unlist(sapply(1:length(v), FUN = function (i) { cmb <- combn(v, m = i)
                                                  apply(cmb, MAR = 2, FUN = function (v) { paste(paste0("I(", v, "^2)"), collapse = " + ") }) }))
  }
}


###  Get the possible interactions to be added.
v.intxn <- function (v) {
  if ( length(v) > 1 ) {
    comb.ind <- combn(length(v), m = 2)
    intxn.v <- apply(comb.ind, MAR = 2, FUN = function (inds) { paste(v[inds], collapse = ":") })
    intxn.combs <-   unlist(sapply(1:length(intxn.v), FUN = function (i) {cmb <- combn(intxn.v, m = i); apply(cmb, MAR = 2, FUN = function (v) { paste(v, collapse = " + ")}) }))
  } else {
    intxn.combs <- NULL
  }
  return(intxn.combs)
}


###  Test whether a forbidden pair exists in the model predictor set.
filt.test <- function (test.v, pred.v) {
  test.v[1] %in% pred.v && test.v[2] %in% pred.v
}


###  Test each column of a matrix for forbidden pairs.
filtr <- function (mat) {
  mat.filt <- apply(mat, MAR = 2, FUN = function (pred.v) { any(apply(forbid, MAR = 1, FUN = filt.test, pred.v = pred.v)) })
  new.mat <- mat[,!mat.filt, drop = F]
  if (dim(new.mat)[2] > 0) {
    out <- new.mat
  } else {
    out <- NULL
  }
  return(out)
}



