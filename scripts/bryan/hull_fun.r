##  Function definition, for hull.fun().
##
##
##
##  08 aug 2019, BLN
####################################################33



hull.fun <- function(b, vars, data = ease, cut = 0.95, col.v = c(pub = "blue", pri = "olivedrab1"), bg.col = "gray70", centroid = "median", plot = T) {
  require(geometry)
  require(rgl)
  sc <- data.frame(conscat = data$conscat[data$buf_m == b], scale(data[data$buf_m == b, vars]))
  mat.pub <- sc[sc$conscat == "Public", vars]
  mat.pri <- sc[sc$conscat == "Private", vars]
  
##  centroid....  the colMeans will be ~0, since each var is scaled.
  ctr.pub <- apply(mat.pub, MAR = 2, FUN = centroid)
  ctr.pri <- apply(mat.pri, MAR = 2, FUN = centroid)

##  distance from the centroid;
  d.pub <- {d <- t(mat.pub) - ctr.pub; sqrt(colSums(d*d))}
  d.pri <- {d <- t(mat.pri) - ctr.pri; sqrt(colSums(d*d))}

  pub.cut <- mat.pub[-which(d.pub > quantile(d.pub, prob = cut)),]
  pri.cut <- mat.pri[-which(d.pri > quantile(d.pri, prob = cut)),]
  
  mango <- list()
  mango$pub <- convhulln(pub.cut, options = "Fa")
  mango$pri <- convhulln(pri.cut, options = "Fa")
  mango$ovrlp <- intersectn(pub.cut, pri.cut, return.chs = F)[[1]]

  bleh <- .check3d()
  rgl.bg(color = c(bg.col, "white"))
  plot3d(x = mat.pub, xlab = vars[1], ylab = vars[2], zlab = vars[3], col = col.v["pub"])
  points3d(x = mat.pri, col = col.v["pri"])  
  rgl.triangles(with(mango$pri, p[c(t(hull)),]), color = col.v["pri"], alpha = 0.2, add = T)
  rgl.triangles(with(mango$pub, p[c(t(hull)),]), color = col.v["pub"], alpha = 0.2, add = T)

  vol <- sapply(mango, FUN = getElement, 'vol')
  ovrlp <- c(pub = vol[3]/vol[1], pri = vol[3]/vol[2])
  
  return(mget(c("vol", "ovrlp")))
}

