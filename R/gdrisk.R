#' Geographical detectors: risk detector.
#'
#' @description Function for risk detector calculation, risk matrix and visualization.
#'
#' @usage gdrisk(formula, data = NULL)
#' \method{print}{gdrisk}(x, ...)
#' \method{plot}{gdrisk}(x, ...)
#'
#' @aliases gdrisk print.gdrisk plot.gdrisk
#'
#' @param formula A formula of response and explanatory variables
#' @param data A data.frame includes response and explanatory variables
#' @param x A list of risk detector results
#' @param ... Ignore
#'
#' @importFrom stats t.test
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual geom_text
#' theme_bw xlab ylab theme scale_y_discrete
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples 
#' gr1 <- gdrisk(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#' gr1
#' plot(gr1)
#' \donttest{
#' data <- ndvi_40[,1:3]
#' gr2 <- gdrisk(NDVIchange ~ ., data = data)
#' gr2
#' }
#'
#' @export
gdrisk <- function(formula, data = NULL){
  formula <- as.formula(formula)
  response <- data[,colnames(data) == as.character(formula[[2]])]
  if (formula[[3]]=="."){
    explanatory <- data[,-which(colnames(data) == as.character(formula[[2]]))]
  } else {
    explanatory <- data[,match(all.vars(formula)[-1], colnames(data))]
  }

  if (typeof(explanatory)=="list"){
    ncolx <- ncol(explanatory)
    variable <- colnames(explanatory)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  ny <- length(response)
  # space for results
  result <- list()

  for (i in 1:ncolx){
    if (typeof(explanatory)=="list"){
      xi <- explanatory[,i]
    } else {
      xi <- explanatory
    }
    # t test by pairs
    itv <- levels(factor(xi))
    citv <- length(itv)
    tv <- as.data.frame(t(combn(itv,2)))
    names(tv) <- c("itv1","itv2")
    tv$t <- NA; tv$df <- NA; tv$sig <- NA; tv$risk <- NA
    for (j in 1:nrow(tv)){
      y1 <- response[which(xi==as.character(tv$itv1[j]))]
      y2 <- response[which(xi==as.character(tv$itv2[j]))]
      if (length(y1) - length(y2) >= 0){
        lengthy <- length(y1)
      } else {
        lengthy <- length(y2)
      }

      tt <- tryCatch({
        t.test(y1,y2)
      }, error = function(y1,y2){
        data.frame("statistic"=0,
                   "parameter"=c(lengthy-1),
                   "p.value"=1)
      })

      tv[j,3] <- tt$statistic
      tv[j,4] <- tt$parameter
      tv[j,5] <- tt$p.value

      if (tt$p.value < 0.05) {
        tv[j,6] <- c("Y")
      } else {
        tv[j,6] <- c("N")
      }
    }
    result[[i]] <- tv
  }
  if (typeof(explanatory)=="list"){
    names(result) <- c(variable)
  } else {
    names(result) <- c("var")
  }

  ## define class
  class(result) <- "gdrisk"
  result
}

print.gdrisk <- function(x, ...){
  lr <- length(x)
  names.result <- names(x)
  plotriskmatrix <- list()
  for (i in 1:lr){
    vec <- x[[i]]
    riskmatrix <- v2m(vec$risk, diag=FALSE)
    itvname <- c(as.character(vec$itv1),as.character(vec$itv2))
    interval <- itvname[!duplicated(itvname)]

    ## export matrix data.frame
    riskmatrix <- as.data.frame(riskmatrix)
    names(riskmatrix) <- interval
    riskmatrix <- cbind(interval, riskmatrix)

    cat(names.result[i])
    cat("\n")
    print(riskmatrix)
    cat("\n")
  }
  invisible(x)
}

plot.gdrisk <- function(x, ...){
  lr <- length(x)
  names.result <- names(x)
  plotriskmatrix <- list()
  for (i in 1:lr){
    vec <- x[[i]]

    riskmatrix <- v2m(vec$risk, diag=FALSE)
    itvname <- c(as.character(vec$itv1),as.character(vec$itv2))
    interval <- itvname[!duplicated(itvname)]

    ## plot matrix
    Ri <- c(riskmatrix)
    ri1 <- rep(interval,each=length(interval))
    ri2 <- rep(interval, length(interval))
    Rm <- as.data.frame(cbind(ri1,ri2,Ri))

    plotriskmatrix[[i]] <- ggplot(Rm, aes(x = ri1, y = ri2, fill = Ri)) +
      geom_tile(colour = "white") +
      scale_fill_manual(values = c("Y"="#FFA500","N"="#7FDBFF")) +
      geom_text(aes(x = ri1, y = ri2,
                    label = ifelse(is.na(Ri), "", paste(Ri))),
                colour = "black", size = 4) +
      theme_bw() +
      xlab(names.result[i]) +
      ylab(names.result[i]) +
      theme(legend.position="none") +
      scale_y_discrete(limits = rev(unique(sort(Rm$ri1))))
  }
  if (lr==1) {
    print(plotriskmatrix[[1]])
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
      print(plotriskmatrix[[i]], vp = viewport(layout.pos.row = index$row,
                                             layout.pos.col = index$col))
    }
  }
}

