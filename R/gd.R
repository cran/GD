#' Calculate geographical detectors
#'
#' @param y A numeric vector
#' @param x A vector or a data.frame
#' @importFrom stats sd pf
#'
#' @examples
#' gd(c(1.2,1.5,2.1,3.3,3.6, 4.3,1.2,1.5,1.9,2.8),
#'    c("d","d","d","c","c", "c","d","d","d","c"))
#' data(StraRoad)
#' gd(StraRoad$damage, StraRoad[,c(2:5)])
#'
#' @export
#'
gd <- function(y, x){
  ny <- length(y)
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  result <- as.data.frame(variable)
  result$qv <- NA
  result$sig <- NA
  for (i in 1:ncolx){
    if (typeof(x)=="list"){
      xi <- x[,i]
    } else {
      xi <- x
    }

    # non-central F test
    xx <- levels(factor(xi))
    nx <- length(xx) # number of strata
    na <- c()
    for (u in 1:nx){
      na[u] <- length(y[which(xi==xx[u])]) * sd(y[which(xi==xx[u])])^2
    }
    qv <- 1 - sum(na, na.rm = T)/(length(y)*sd(y)^2)
    Fv <- (ny - nx)/(nx - 1)*qv/(1 - qv)
    m1 <- c(); m2 <- c()
    for (u in 1:nx){
      m1[u] <- mean(y[which(xi==xx[u])])^2
      m2[u] <- sqrt(length(y[which(xi==xx[u])])) * mean(y[which(xi==xx[u])])
    }
    lambda <- (sum(m1) - sum(m2)^2/ny)/sd(y)^2
    p0 <- pf(Fv, df1 = (nx - 1), df2 = (ny - nx), ncp = lambda)
    sig <- 2*(1-p0)
    result$qv[i] <- qv
    result$sig[i] <- sig
  }
  return(result)
}


