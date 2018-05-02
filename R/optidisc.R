#' Optimal discretization for continuous variables and visualization
#'
#' @usage optidisc(y = y, x = x, discmethod = discmethod, discitv = discitv)
#' \\method{print}{optidisc}(result)
#' \\method{plot}{optidisc}(result)
#'
#' @aliases optidisc print.optidisc plot.optidisc
#'
#' @param y A numeric vector of response variable
#' @param x A numeric vector of explanatory variable
#' @param discmethod A character vector of discretization methods
#' @param discitv A numeric vector of numbers of intervals
#' @param result A list of \code{optidisc} result
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw
#' scale_x_continuous scale_y_continuous scale_color_discrete
#' @importFrom reshape2 melt
#'
#' @examples
#' ## set optional discretization methods and numbers of intervals
#' # optional methods: equal, natural, quantile, geometric, sd and manual
#' discmethod <- c("equal","natural","quantile","geometric","sd")
#' discitv <- c(3:7)
#' ## optimal discretization
#' odc1 <- optidisc(ndvi_40$NDVIchange, ndvi_40$Tempchange, discmethod, discitv)
#' # odc1
#' # plot(odc1)
#' # plot(odc1$discretization)
#'
#' @export

optidisc <- function(y = y, x = x, discmethod = discmethod, discitv = discitv){

  n.itv <- length(discitv)
  n.method <- length(discmethod)

  OD <- matrix(NA, n.itv, n.method)
  OD <- as.data.frame(OD)
  names(OD) <- discmethod
  OD <- cbind(discitv,OD)

  for (u in 1:n.itv){ # no.itv
    for (v in 1:n.method){ # discmethod
      dc1 <- disc(x, discitv[u], method = discmethod[v])
      if (min(dc1$c.itv) < 2) {
        # remove number of data within itv < 2 due to sd() in q
        OD[u,v+1] <- NA
      } else {
        stra.var1 <- stra(x, dc1$itv)
        gd1 <- gd(y,stra.var1)
        OD[u,v+1] <- gd1$Factor$qv
      }
    }
  }
  odmatrix <- OD[,-1]
  k <- which(odmatrix==max(odmatrix, na.rm = TRUE))
  nik <- discitv[(k[1]-1)%%n.itv+1]
  dmk <- discmethod[(k[1]-1)%/%n.itv+1]

  dc1 <- disc(x, nik, method = dmk)

  optidisc.list <- list("method"=dmk, "n.itv"=nik, "itv"=dc1$itv,
              "c.itv"=dc1$c.itv, "OD"=OD, "discretization"=dc1)

  ## define class
  class(optidisc.list) <- "optidisc"
  optidisc.list
}

print.optidisc <- function(result){
  cat("\n")
  cat("Optimal discretization method: ", result$method)
  cat("\n")
  cat("Optimal number of intervals: ", result$n.itv)
  cat("\n")
  cat("Intervals:\n", result$itv)
  cat("\n")
  cat("Numbers of data within intervals:\n", result$c.itv)
  cat("\n\n")
  invisible(result)
}

plot.optidisc <- function(result){
  discitv <- NA; value <- NA; variable <- NA
  # plot optimal discretization results
  reshapedata <- melt(result$OD, id="discitv")
  rsd <- reshapedata[!is.na(reshapedata$value),]
  if (nrow(rsd) < nrow(reshapedata)){
    cat("\r","One or several discretization results are removed,
        due to less than two data in a certain interval.")
  }
  plot1 <- ggplot(data=rsd,
                  aes(x=discitv, y=value, colour=variable)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name ="Number of intervals") +
    scale_y_continuous(name ="Q value") +
    scale_color_discrete(name = "Method") +
    theme_bw()
  plot1
}
