#' Calculate interaction detectors
#'
#' @param y A numeric vector
#' @param x A data.frame with more than two strata variables
#'
#' @importFrom utils combn
#'
#' @examples
#' data(StraRoad)
#' gd.riskmean(StraRoad$damage, StraRoad[,c(2:5)])
#'
#' @export
gd.interact <- function(y, x){
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    warning("x should be a dataframe with more than two variables")
  }
  result <- as.data.frame(t(combn(variable,2)))
  names(result) <- c("var1","var2")
  result$qv1 <- NA
  result$qv2 <- NA
  result$qv12 <- NA
  result$interaction <- NA
  for (u in 1:nrow(result)){
    x1 <- x[,which(variable==result$var1[u])]
    x2 <- x[,which(variable==result$var2[u])]
    x12 <- stra.2v(x1,x2)
    gd1 <- gd(y, x1)
    gd2 <- gd(y, x2)
    gd12 <- gd(y, x12)
    qv1 <- gd1$qv; qv2 <- gd2$qv; qv12 <- gd12$qv
    result$qv1[u] <- gd1$qv
    result$qv2[u] <- gd2$qv
    result$qv12[u] <- gd12$qv
    if (gd12$qv < min(gd1$qv, gd2$qv)) {
      result$interaction[u] <- c("Weaken, nonlinear")
    } else if (gd12$qv >= min(gd1$qv, gd2$qv) & gd12$qv <= max(gd1$qv, gd2$qv)) {
        result$interaction[u] <- c("Weaken, uni-")
    } else if (gd12$qv > max(gd1$qv, gd2$qv) & gd12$qv < gd1$qv + gd2$qv) {
          result$interaction[u] <- c("Enhance, bi-")
    } else if (gd12$qv == gd1$qv + gd2$qv) {
            result$interaction[u] <- c("Independent")
    } else {
      result$interaction[u] <- c("Enhance, nonlinear")
      }
  }
  return(result)
}
