#' Count data within intervals
#'
#' @param vec A vector
#'
#' @importFrom stats aggregate na.omit
#'
#' @examples
#' citv(c("a","a","b","b","b"))
#' data(StraRoad)
#' citv(StraRoad$population)
#' @export
citv <- function(vec){
  if (any(is.na(vec))) {
    warning("vector has missing values, omitted in finding classes")
    vec <- c(na.omit(vec))
  }
  itv <- levels(factor(as.character(vec)))
  nitv <- length(itv)
  citv <- as.data.frame(itv)
  count <- aggregate(vec, list(vec), length)
  citv$count <- count$x
  return(citv)
}

