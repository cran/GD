#' Convert ecological detector results to ecological matrix
#'
#' @param vec A data.frame of ecological detector result of a strata variable
#'
#' @examples
#' data(StraRoad)
#' gdeco <- gd.eco(StraRoad$damage, StraRoad[,c(2:5)])
#' gd.ecomatrix(gdeco)
#'
#' @export

gd.ecomatrix <- function(vec){
  vec2loma <- function (x, diag = FALSE) {
    n <- length(x)
    d <- floor((sqrt(1 + n*8) + 1)/2)
    matx <- diag(d)
    matx[lower.tri(matx)] <- x
    matx[upper.tri(matx)] <- NA
    diag(matx) <- NA
    return(matx)
  }
  ecomatrix <- vec2loma(vec$eco, diag=FALSE)
  ecomatrix <- as.data.frame(ecomatrix)
  varname <- c(as.character(vec$var1),as.character(vec$var2))
  variable <- varname[!duplicated(varname)]
  names(ecomatrix) <- variable
  ecomatrix <- cbind(variable, ecomatrix)
  return(ecomatrix)
}
