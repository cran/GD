#' Calculate ecological detectors
#'
#' @param y A numeric vector
#' @param x A vector or a data.frame
#'
#' @importFrom stats pf
#' @importFrom utils combn
#'
#' @examples
#' data(StraRoad)
#' gd.eco(StraRoad$damage, StraRoad[,c(2:5)])
#'
#' @export
gd.eco <- function(y,x){
  if (typeof(x)!="list"){
    warning("x should contain multiple variables for ecological detector")
  }
  ncolx <- ncol(x)
  variable <- colnames(x)

  fv <- as.data.frame(t(combn(variable,2)))
  names(fv) <- c("var1","var2")

  fv$f <- NA; fv$sig <- NA; fv$eco <- NA
  for (i in 1:nrow(fv)){
    x1 <- x[,which(variable==as.character(fv$var1[i]))]
    x2 <- x[,which(variable==as.character(fv$var2[i]))]
    # F test
    c1 <- aggregate(y, list(x1), length)
    s1 <- aggregate(y, list(x1), sd)
    c2 <- aggregate(y, list(x2), length)
    s2 <- aggregate(y, list(x2), sd)
    fv$f[i] <- length(x1)*(length(x2)-1)*sum(c1$x*s1$x^2)/(length(x2)*(length(x1)-1)*sum(c2$x*s2$x^2))
    p0 <- pf(fv$f[i], df1 = (length(x1) - 1), df2 = (length(x2) - 1))
    fv$sig[i] <- 2*(1-p0)
    if (fv$sig[i] < 0.05){
      fv$eco[i] <- c("Y")
    } else {
      fv$eco[i] <- c("N")
    }
  }
  return(fv)
}
