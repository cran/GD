#' Convert continuous data to discretized data
#'
#' @usage disc(var, n, method = "quantile", ManualItv)
#' \\method{print}{disc}(result)
#' \\method{plot}{disc}(result)
#'
#' @aliases disc print.disc plot.disc
#'
#' @param var A numeric vector of continuous variable
#' @param n The numeber of intervals
#' @param method A character of discretization method
#' @param ManualItv A numeric vector of manual intervals
#' @param result A list of \code{disc} result
#'
#' @importFrom stats na.omit quantile
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw labs geom_vline
#' @importFrom BAMMtools getJenksBreaks
#'
#' @examples
#' ## method is default (quantile); number of intervals is 4
#' ds1 <- disc(ndvi_40$Tempchange, 4)
#' # ds1
#' ## method is equal; number of intervals is 4
#' ds2 <- disc(ndvi_40$Tempchange, 4, method = "equal")
#' ## method is manual; number of intervals is 4
#' manualitv1 <- c(-0.5, 0, 1, 2, 4)
#' ds3 <- disc(ndvi_40$Tempchange, 4, method = "manual", ManualItv = manualitv1)
#'
#' @export

disc <- function(var, n, method = "quantile", ManualItv){
  if (!is.numeric(var))
    stop("var is not numeric")
  if (any(is.na(var))) {
    warning("var has missing values, omitted in finding classes")
    var <- c(na.omit(var))
  }
  if (method == "equal") {
    itv <- seq(min(var), max(var), length.out = (n + 1))
  } else if (method == "natural") {
    itv <- getJenksBreaks(var, (n + 1))
  } else if (method == "quantile") {
    itv <- quantile(var, probs = seq(0, 1, length = n + 1))
  } else if (method == "geometric") {
    exponent <- function(a, pow) (abs(a)^pow)*sign(a)
    if (min(var) == 0){
      var <- var + 1
      k <- exponent(max(var)/min(var), 1/n)
      itv <- min(var)*(k^c(seq(n+1)-1))
      itv <- itv - 1
    } else {
      k <- exponent(max(var)/min(var), 1/n)
      itv <- min(var)*(k^c(seq(n+1)-1))
    }
  } else if (method == "sd") {
    seqa <- seq(2000, 2, -2)
    itv <- c()
    m <- n
    while (length(itv) < n + 1){
      m <- m + 1
      seqb <- (m-1) - seqa
      seqb <- seqb[which(seqb >= 0)]
      if (m <= 7) {
        itvb1 <- mean(var) - seqb/2 * sd(var)
        itvb2 <- mean(var) + seqb/2 * sd(var)
      } else {
        itvb1 <- mean(var) - seqb/4 * sd(var)
        itvb2 <- mean(var) + seqb/4 * sd(var)
      }
      itvb1 <- rev(itvb1)
      itvb1 <- itvb1[which(itvb1 > min(var))]
      itvb2 <- itvb2[which(itvb2 < max(var))]
      itvb <- c(itvb1, itvb2)
      itvb <- itvb[!duplicated(itvb)]
      itv <- c(min(var),itvb,max(var))
    }
  } else if (method == "manual") {
    if (!is.null(ManualItv)){
      itv <- ManualItv
    } else {
      warning("Input manual interval vector")
    }
  }
  c.itv <- c()
  for (u in 1:n){
    if (u == 1){
      c.itv[u] <- length(which(var>=itv[u] & var<=itv[u+1]))
    } else {
      c.itv[u] <- length(which(var>itv[u] & var<=itv[u+1]))
    }
  }
  disc.list <- list("var"=var, "itv"=itv, "c.itv"=c.itv)
  ## define class
  class(disc.list) <- "disc"
  disc.list
}

print.disc <- function(result){
  cat("Intervals:\n", result$itv)
  cat("\n")
  cat("Numbers of data within intervals:\n", result$c.itv)
  invisible(result)
}

plot.disc <- function(result){
  var <- result$var
  var <- as.data.frame(var)
  plotdisc <- ggplot(data=var, aes(var)) +
    geom_histogram(breaks=seq(min(var), max(var), by = ((max(var) - min(var))/30))) +
    theme_bw() +
    labs(x = "variable", y = "Frequency") +
    geom_vline(xintercept=result$itv, color = "red")
  return(plotdisc)
}

