#' Optimal discretization for continuous variables and visualization.
#'
#' @usage optidisc(continuous_variable, response_variable,
#'         discmethod = discmethod, discitv = discitv)
#' \method{print}{optidisc}(x, ...)
#' \method{plot}{optidisc}(x, ...)
#'
#' @aliases optidisc print.optidisc plot.optidisc
#'
#' @param continuous_variable A vector or a data.frame of continuous explanatory variables
#' @param response_variable A vector of response variable
#' @param discmethod A character vector of discretization methods
#' @param discitv A numeric vector of numbers of intervals
#' @param x A list of \code{optidisc} result
#' @param ... Ignore
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw
#' scale_x_continuous scale_y_continuous scale_color_discrete
#' @importFrom reshape2 melt
#' @importFrom graphics par plot
#'
#' @examples 
#' ## set optional discretization methods and numbers of intervals
#' # optional methods: equal, natural, quantile, geometric, sd and manual
#' discmethod <- c("equal","natural","quantile","geometric","sd")
#' discitv <- c(3:7)
#' ## optimal discretization
#' odc1 <- optidisc(ndvi_40$Tempchange, ndvi_40$NDVIchange, discmethod, discitv)
#' odc1
#' plot(odc1)
#'
#' @export

optidisc <- function(continuous_variable, response_variable,
                     discmethod = discmethod, discitv = discitv){

  ### function of optimal discretization for an explanatory variables
  optimaldiscretization <- function(y, x, discmethod, discitv){
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
          gddata <- as.data.frame(cbind(y,stra.var1))
          gd1 <- gd(y ~ stra.var1, data = gddata)
          OD[u,v+1] <- gd1$Factor$qv
        }
      }
    }
    odmatrix <- OD[,-1]
    k <- which(odmatrix==max(odmatrix, na.rm = TRUE))
    nik <- discitv[(k[1]-1)%%n.itv+1]
    dmk <- discmethod[(k[1]-1)%/%n.itv+1]

    dc1 <- disc(x, nik, method = dmk)

    optimaldiscretization.list <- list("method"=dmk, "n.itv"=nik, "itv"=dc1$itv,
                          "c.itv"=dc1$c.itv, "OD"=OD, "discretization"=dc1)
  }

  ### optimal discretization for one or multiple continuous variables
  if (typeof(continuous_variable)=="list"){
    ncolx <- ncol(continuous_variable)
    variable <- colnames(continuous_variable)
    list1 <- list()
    for (i in 1:ncolx){
      list1[[i]] <- optimaldiscretization(response_variable, continuous_variable[,i],
                                          discmethod, discitv)
    }
    names(list1) <- variable
    optidisc.list <- list1
  } else {
    list2 <- optimaldiscretization(response_variable, continuous_variable,
                                   discmethod, discitv)
    optidisc.list <- list("var" = list2)
  }

  ## define class
  class(optidisc.list) <- "optidisc"
  optidisc.list
}

print.optidisc <- function(x, ...){
  namesx <- names(x)
  for (i in 1:length(namesx)){
    cat("optimal discretization result of",namesx[i])
    cat("\n")
    cat("method             : ", x[[i]]$method)
    cat("\n")
    cat("number of intervals: ", x[[i]]$n.itv)
    cat("\n")
    cat("intervals:\n", x[[i]]$itv)
    cat("\n")
    cat("numbers of data within intervals:\n", x[[i]]$c.itv)
    cat("\n\n")
  }
  invisible(x)
}

plot.optidisc <- function(x, ...){
  discitv <- NA; value <- NA; variable <- NA
  # plot optimal discretization results
  lr <- length(x)
  names.result <- names(x)
  plotoptidisc <- list()

  for (i in 1:lr){
    reshapedata <- melt(x[[i]]$OD, id="discitv")
    rsd <- reshapedata[!is.na(reshapedata$value),]
    if (nrow(rsd) < nrow(reshapedata)){
      cat("One or several discretization results are removed, due to less than two data in a certain interval.\n\n")
    }
    plotoptidisc[[i]] <- ggplot(data=rsd,
                    aes(x=discitv, y=value, colour=variable)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(name ="Number of intervals") +
      scale_y_continuous(name ="Q value") +
      scale_color_discrete(name = "Method") +
      theme_bw()
  }
  ### optimal discretization process
  cat("Optimal discretization process ...\n\n")
  if (lr==1) {
    print(plotoptidisc[[1]])
  } else {
    if (lr > 1 & lr <= 4) {
      cols <- 2
    } else if (lr > 4 & lr <= 9) {
      cols <- 3
    } else {
      cols <- 4
    }
    layout <- t(matrix(seq(1, cols*ceiling(lr/cols)),
                       ncol = ceiling(lr/cols), nrow = cols))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:lr) {
      index <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plotoptidisc[[i]], vp = viewport(layout.pos.row = index$row,
                                             layout.pos.col = index$col))
    }
  }
  ### optimal discretization results
  cat("Optimal discretization result ...\n\n")
  if (lr==1) {
    plot(x[[1]]$discretization)
  } else {
    if (lr > 1 & lr <= 4) {
      cols <- 2
    } else if (lr > 4 & lr <= 9) {
      cols <- 3
    } else {
      cols <- 4
    }
    layout <- t(matrix(seq(1, cols*ceiling(lr/cols)),
                       ncol = ceiling(lr/cols), nrow = cols))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:lr) {
      index <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plot.disc(x[[i]]$discretization), vp = viewport(layout.pos.row = index$row,
                                 layout.pos.col = index$col))
    }
  }
}

