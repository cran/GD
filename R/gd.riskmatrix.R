#' Convert risk detector results to risk matrix
#'
#' @param vec A data.frame of risk detector result of a strata variable
#'
#' @examples
#' data(StraRoad)
#' gdrisk <- gd.risk(StraRoad$damage, StraRoad[,c(2:5)])
#' gd.riskmatrix(gdrisk$population)
#'
#' @export

gd.riskmatrix <- function(vec){
  vec2loma <- function (x, diag = FALSE) {
    n <- length(x)
    d <- floor((sqrt(1 + n*8) + 1)/2)
    matx <- diag(d)
    matx[lower.tri(matx)] <- x
    matx[upper.tri(matx)] <- NA
    diag(matx) <- NA
    return(matx)
  }
  riskmatrix <- vec2loma(vec$risk, diag=FALSE)
  riskmatrix <- as.data.frame(riskmatrix)
  itvname <- c(as.character(vec$itv1),as.character(vec$itv2))
  interval <- itvname[!duplicated(itvname)]
  names(riskmatrix) <- interval
  riskmatrix <- cbind(interval, riskmatrix)
  return(riskmatrix)
}
