#' Convert continuous data to strata variables
#'
#' @usage stra(continuous_var, itv)
#'
#' @param continuous_var A vector of a continuous variable
#' @param itv A vector of intervals
#'
#' @examples
#' # stra(rnorm(100,0,2), c(-4.9,-1.3,-0.5,0.3,0.9,2.0,4.9))
#' # stra(c(1:10,15:20,5:30,10:25), c(1,7,13,18,23,30))
#' ds1 <- disc(ndvi_40$Tempchange, 4)
#' stv1 <- stra(ndvi_40$Tempchange, ds1$itv)
#'
#' @export
stra <- function(continuous_var, itv){
  n <- length(itv) - 1
  svar <- continuous_var
  for (u in 1:n){
    if (u==1) {
      svar[which(continuous_var <= itv[2])] <- u
    } else if (u==n) {
      svar[which(continuous_var > itv[n])] <- u
    } else {
      svar[which(continuous_var > itv[u] & continuous_var <= itv[u+1])] <- u
    }
  }
  return(svar)
}
