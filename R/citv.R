#' Count data within intervals for categorical or discretized variables
#'
#' @usage citv(strata_var)
#'
#' @param strata_var A vector of a strata variable
#'
#' @importFrom stats aggregate na.omit
#'
#' @examples
#' data(ndvi_40)
#' ds1 <- disc(ndvi_40$Tempchange, 4)
#' stv1 <- stra(ndvi_40$Tempchange, ds1$itv)
#' citv(stv1)
#'
#' @export
citv <- function(strata_var){
  if (any(is.na(strata_var))) {
    warning("strata_vartor has missing values, omitted in finding classes")
    strata_var <- c(na.omit(strata_var))
  }
  itv <- levels(factor(as.character(strata_var)))
  nitv <- length(itv)
  citv <- as.data.frame(itv)
  count <- aggregate(strata_var, list(strata_var), length)
  citv$count <- count$x
  return(citv)
}


