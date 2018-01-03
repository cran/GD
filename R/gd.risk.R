#' Calculate risk detectors
#'
#' @param y A numeric vector
#' @param x A vector or a data.frame
#'
#' @importFrom stats t.test
#' @importFrom utils combn
#'
#' @examples
#' gd.risk(c(1.2,1.5,2.1,3.3,3.6, 4.3,1.2,1.5,1.9,2.8),
#'         c("d","d","d","c","c", "c","d","d","d","c"))
#' data(StraRoad)
#' gd.risk(StraRoad$damage, StraRoad[,c(2:5)])
#'
#' @export
gd.risk <- function(y,x){
  ny <- length(y)
  # space for results
  result <- list()
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  for (i in 1:ncolx){
    if (typeof(x)=="list"){
      xi <- x[,i]
    } else {
      xi <- x
    }
    # t test by pairs
    itv <- levels(factor(xi))
    citv <- length(itv)
    tv <- as.data.frame(t(combn(itv,2)))
    names(tv) <- c("itv1","itv2")
    tv$t <- NA; tv$df <- NA; tv$sig <- NA; tv$risk <- NA
    for (j in 1:nrow(tv)){
      y1 <- y[which(xi==as.character(tv$itv1[j]))]
      y2 <- y[which(xi==as.character(tv$itv2[j]))]
      tt <- t.test(y1,y2)
      tv[j,3] <- tt$statistic
      tv[j,4] <- tt$parameter
      tv[j,5] <- tt$p.value
      if (tt$p.value < 0.05) {
        tv[j,6] <- c("Y")
      } else {
        tv[j,6] <- c("N")
      }
    }
    result[[i]] <- tv
  }
  if (typeof(x)=="list"){
    names(result) <- c(variable)
  } else {
    names(result) <- c("var")
  }
  return(result)
}


