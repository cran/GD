#' Generate two category or discretized variables determined strata variables
#'
#' @param var1 A vector
#' @param var2 A vector
#'
#' @examples
#' stra.2v(c("a","a","b","b","b"), c("d","c","d","c","c"))
#' stra.2v(c(1,1,1,2,2, 3,1,2,2,3, 2,3,2,1,3), c(7,8,7,9,8, 7,8,9,8,9, 9,9,8,8,9))
#' data(StraRoad)
#' stra.2v(StraRoad$soiltype, StraRoad$population)
#'
#' @export

stra.2v <- function(var1, var2){
  lv1 <- levels(factor(var1))
  lv2 <- levels(factor(var2))
  s2.var <- c()
  for (u in 1:length(lv1)){
    for (v in 1:length(lv2)){
      s2.var[which(var1==lv1[u] & var2==lv2[v])] <- paste0(lv1[u],"_",lv2[v])
    }
  }
  return(s2.var)
}






















