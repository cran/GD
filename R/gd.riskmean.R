#' Calculate risk means within intervalss
#'
#' @param y A numeric vector
#' @param x A vector or a data.frame
#'
#' @examples
#' gd.riskmean(c(1.2,1.5,2.1,3.3,3.6, 4.3,1.2,1.5,1.9,2.8),
#'             c("d","d","d","c","c", "c","d","d","d","c"))
#' data(StraRoad)
#' gd.riskmean(StraRoad$damage, StraRoad[,c(2:5)])
#'
#' @export
#'
gd.riskmean <- function(y,x){
  ny <- length(y)
  # space for results
  result <- list()
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    colnames.x <- colnames(x)
    for (i in 1:ncolx){
      xi <- x[,i]
      result1 <- aggregate(y, list(xi), mean) # mean by group
      names(result1) <- c("Group","meanrisk")
      result[[i]] <- result1
    }
    names(result) <- c(colnames.x)
  } else {
    ncolx <- 1
    result1 <- aggregate(y, list(x), mean) # mean by group
    names(result1) <- c("Group","meanrisk")
    result[[1]] <- result1
  }
  return(result)
}
