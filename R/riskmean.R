#' Geographical detectors: risk means in risk detector.
#'
#' @description Function for calculating risk means within intervals and visualization.
#'
#' @usage riskmean(formula, data = NULL)
#' \method{print}{riskmean}(x, ...)
#' \method{plot}{riskmean}(x, ...)
#'
#' @aliases riskmean print.riskmean plot.riskmean
#'
#' @param formula a formula of response and explanatory variables
#' @param data a data.frame includes response and explanatory variables
#' @param x a list of risk mean values
#' @param ... ignore
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip xlab ylab theme_bw scale_y_discrete
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples 
#' rm1 <- riskmean(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#' rm1
#' plot(rm1)
#' \donttest{
#' data <- ndvi_40[,1:3]
#' rm2 <- riskmean(NDVIchange ~ ., data = data)
#' rm2
#' }
#'
#' @export
#'
riskmean <- function(formula, data = NULL){
  formula <- as.formula(formula)
  response <- data[,colnames(data) == as.character(formula[[2]])]
  if (formula[[3]]=="."){
    explanatory <- data[,-which(colnames(data) == as.character(formula[[2]]))]
  } else {
    explanatory <- data[,match(all.vars(formula)[-1], colnames(data))]
  }

  ny <- length(response)
  # space for results
  result <- list()

  if (typeof(explanatory)=="list"){
    ncolx <- ncol(explanatory)
    colnames.x <- colnames(explanatory)
    for (i in 1:ncolx){
      xi <- explanatory[,i]
      result1 <- aggregate(response, list(xi), mean) # mean by group
      names(result1) <- c("Group","meanrisk")
      result[[i]] <- result1
    }
    names(result) <- c(colnames.x)
  } else {
    ncolx <- 1
    result1 <- aggregate(response, list(explanatory), mean) # mean by group
    names(result1) <- c("Group","meanrisk")
    result <- list("variable"=result1)
  }

  ## define class
  class(result) <- "riskmean"
  result
}

print.riskmean <- function(x, ...){
  lr <- length(x)
  names.result <- names(x)
    for (i in 1:lr){
      resulti <- x[[i]]
      cat(names.result[i])
      cat("\n")
      print(resulti)
      cat("\n")
    }
  invisible(x)
}

plot.riskmean <- function(x, ...){
  Group <- NA; meanrisk <- NA
  lr <- length(x)
  names.result <- names(x)
  plotriskmean <- list()
  for (i in 1:lr){
    vec <- x[[i]]
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

