#' Convert discretized data to strata variables
#'
#' @param cont.var A vector
#' @param itv A vector
#'
#' @examples
#' stra(rnorm(100,0,2), c(-4.9,-1.3,-0.5,0.3,0.9,2.0,4.9))
#' stra(c(1:10,15:20,5:30,10:25), c(1,7,13,18,23,30))
#' data(Roaddamage)
#' stra(Roaddamage$population, c(1.0,6.9,27.6,168.8,226.5,370.1,2489.8))
#'
#' @export
stra <- function(cont.var, itv){
  n <- length(itv)
  disc.var <- cont.var
  for (u in 1:n){
    disc.var[which(cont.var>=(itv[u]-0.0000001) & cont.var<=(itv[u+1]+0.0000001))] <- u
  }
  return(disc.var)
}






















