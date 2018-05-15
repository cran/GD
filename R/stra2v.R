#' Generates two categorical or discretized variables determined strata.
#'
#' @usage stra2v(categorical_var1, categorical_var2)
#'
#' @param categorical_var1 A vector of a categorical variable
#' @param categorical_var2 A vector of a categorical variable
#'
#' @examples 
#' stv2 <- stra2v(ndvi_40$Climatezone, ndvi_40$Mining)
#'
#' @export
#'
stra2v <- function(categorical_var1, categorical_var2){
  lv1 <- levels(factor(categorical_var1))
  lv2 <- levels(factor(categorical_var2))
  s2var <- c()
  for (u in 1:length(lv1)){
    for (v in 1:length(lv2)){
      s2var[which(categorical_var1==lv1[u] & categorical_var2==lv2[v])] <-
        paste0(lv1[u],"_",lv2[v])
    }
  }
  return(s2var)
}



