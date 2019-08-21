#' Generates discretization parameters for continuous data.
#'
#' @description Function for discretizing continuous data and obtaining the
#' different outputs, including discretization intervals, numbers of values
#' within intervals, and visualization of discretization.
#'
#' @usage disc(var, n, method = "quantile", ManualItv)
#' \method{print}{disc}(x, ...)
#' \method{plot}{disc}(x, ...)
#'
#' @aliases disc print.disc plot.disc
#'
#' @param var A numeric vector of continuous variable
#' @param n The numeber of intervals
#' @param method A character of discretization method
#' @param ManualItv A numeric vector of manual intervals
#' @param x A list of \code{disc} result
#' @param ... Ignore
#'
#' @importFrom stats na.omit quantile sd runif
#' @importFrom graphics hist abline
#'
#' @examples
#' ## method is default (quantile); number of intervals is 4
#' ds1 <- disc(ndvi_40$Tempchange, 4)
#' ds1
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

  MethodEqual <- function(var, n){
    seq(min(var), max(var), length.out = (n + 1))
  }

  MethodNatural <- function(var, n){ # debug: increase speed
    ssd <- function(x) sum((x - mean(x))^2)
    minvar <- min(var); maxvar <- max(var)
    # set optional itv values to increase speed
    options <- unique(sort(var))
    size <- 1000
    if (length(options) > size) {
      options2 <- seq(minvar, maxvar, length = size)
      count.options2 <- round(sqrt(table(cut(var, options2, include.lowest = TRUE))))
      options <- unlist(sapply(1:(size - 1), function(x)
        seq(options2[x], options2[x + 1], length = count.options2[x])))
      options <- unique(options, maxvar)
    }
    # set variable data as op.var: mean var within options
    op.cut <- as.numeric(cut(var, options, include.lowest = TRUE))
    op.var <- sapply(split(var, op.cut), mean)
    # locations
    locations <- sapply(1:1000, function(x) sort(sample(2:(length(options) - 1), n - 1, replace = F)))
    locations <- unique(locations, MARGIN = 2)
    lncol <- ncol(locations); lnrow <- nrow(locations)
    # select best itv
    options2 <- matrix(options[locations], lnrow, lncol)
    itv0 <- rbind(rep(minvar, lncol), options2, rep(maxvar, lncol))
    itv0.cut <- apply(itv0, 2, function(x) as.numeric(cut(op.var, x, include.lowest = TRUE)))
    itv0.x <- apply(itv0.cut, 2, function(x) sum(sapply(split(op.var, x), ssd)))
    min.tssd <- quantile(itv0.x, 0.1, na.rm = TRUE)
    k <- which(itv0.x <= min.tssd)
    itv <- as.matrix(itv0[, k]) # debug: ensure itv is a matrix
    itv <- rowMeans(itv)
    itv[1] <- minvar
    return(itv)
  }

  MethodQuantile <- function(var, n){
    quantile(var, probs = seq(0, 1, length = n + 1))
  }

  MethodGeometric <- function(var, n){
    FunX <- function(x, n){
      x1 <- x + 1
      b <- (max(x1)/min(x1))^(1/(n - 1))
      x0 <- min(x1)/b # debug: x1 = b * x0; xn = b^n * x0
      x2 <- b^seq(n) * x0 - 1
      return(x2)
    }
    if (min(var) >= 0){
      itv <- FunX(var, n)
    } else {
      var.range <- abs(c(min(var), max(var)))
      kmax <- which(var.range == max(var.range))
      kmin <- c(1,2)[-kmax]
      n2 <- floor((n + 1)/(1 + var.range[kmin]/var.range[kmax]))
      n1 <- n + 1 - n2
      itv1 <- FunX(c(0, var.range[kmin]), n1 + 1)[-1]
      itv2 <- FunX(c(0, var.range[kmax]), n2 + 1)[-1]
      if (kmin == 1){
        itv <- c(-rev(itv1), itv2)
      } else {
        itv <- c(-rev(itv2), itv1)
      }
    }
    return(itv)
  }

  MethodSd <- function(var, n){
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
    return(itv)
  }

  if (method == "equal") {
    itv <- MethodEqual(var, n)
  } else if (method == "natural") {
    itv <- MethodNatural(var, n)
  } else if (method == "quantile") {
    itv <- MethodQuantile(var, n)
  } else if (method == "geometric") {
    itv <- MethodGeometric(var, n)
  } else if (method == "sd") {
    itv <- MethodSd(var, n)
  } else if (method == "manual") {
    if (!is.null(ManualItv)){
      itv <- ManualItv
    } else {
      warning("Input manual interval vector")
    }
  }

  # debug: delete citv function
  disc.list <- list("var" = var, "itv" = itv)
  ## define class
  class(disc.list) <- "disc"
  disc.list
}

print.disc <- function(x, ...){
  cat("Intervals:\n", x$itv)
  invisible(x)
}

plot.disc <- function(x, ...){
  var <- x$var
  # debug: use basic plot functions for histogram
  hist(var, 30, col = "gray", border = "gray", main = NULL, xlab = "Variable", las = 1)
  abline(v = x$itv, col = "red")
  box()
}

