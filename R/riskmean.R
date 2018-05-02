#' Calculate risk means within intervals and visualization
#'
#' @usage riskmean(y, x)
#' \\method{print}{riskmean}(result)
#' \\method{plot}{riskmean}(result)
#'
#' @aliases riskmean print.riskmean plot.riskmean
#'
#' @param y A numeric vector of response variable
#' @param x A vector or a data.frame of explanatory variables
#' @param result A data.frame of risk mean values
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip xlab ylab theme_bw scale_y_discrete
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples
#' rm1 <- riskmean(ndvi_40$NDVIchange, ndvi_40[,2:3])
#' # rm1
#' # plot(rm1)
#'
#' @export
#'
riskmean <- function(y,x){
  ny <- length(y)
  # space for results
  result <- list()
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    colnames.x <- colnames(x)
    for (i in 1:ncolx){
      xi <- x[,i]
      result1 <- aggregate(y, list(xi), mean) # mean by group
      names(result1) <- c("Group","meanrisk")
      result[[i]] <- result1
    }
    names(result) <- c(colnames.x)
  } else {
    ncolx <- 1
    result1 <- aggregate(y, list(x), mean) # mean by group
    names(result1) <- c("Group","meanrisk")
    result <- list("variable"=result1)
  }

  ## define class
  class(result) <- "riskmean"
  result
}

print.riskmean <- function(result){
  lr <- length(result)
  names.result <- names(result)
    for (i in 1:lr){
      resulti <- result[[i]]
      cat(names.result[i])
      cat("\n")
      print(resulti)
      cat("\n")
    }
  invisible(result)
}

plot.riskmean <- function(result){
  Group <- NA; meanrisk <- NA
  lr <- length(result)
  names.result <- names(result)
  plotriskmean <- list()
  for (i in 1:lr){
    vec <- result[[i]]
    plotriskmean[[i]] <- ggplot(vec, aes(Group, meanrisk)) +
      geom_bar(stat = "identity") +
      geom_bar(data = subset(vec, meanrisk==min(meanrisk)), aes(Group, meanrisk),
               fill = "royalblue", stat = "identity") +
      geom_bar(data = subset(vec, meanrisk==max(meanrisk)), aes(Group, meanrisk),
               fill = "orangered", stat = "identity") +
      coord_flip() +
      xlab(names.result[i]) +
      ylab("Mean value") +
      scale_x_discrete(limits = rev(sort(vec$Group))) +
      theme_bw()
  }
  if (lr==1) {
    print(plotriskmean[[1]])
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
      print(plotriskmean[[i]], vp = viewport(layout.pos.row = index$row,
                                      layout.pos.col = index$col))
    }
  }
}

