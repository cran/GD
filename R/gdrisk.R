#' Calculate risk detectors and risk matrix and visualization
#'
#' @usage gdrisk(y, x)
#' \\method{print}{gdrisk}(result)
#' \\method{plot}{gdrisk}(result)
#'
#' @aliases gdrisk print.gdrisk plot.gdrisk
#'
#' @param y A numeric vector of response variable
#' @param x A vector or a data.frame of explanatory variables
#' @param result A list of risk detector results
#'
#' @importFrom stats t.test
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual geom_text
#' theme_bw xlab ylab theme scale_y_discrete
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples
#' gr1 <- gdrisk(ndvi_40$NDVIchange, ndvi_40[,2:3])
#' # gr1
#' # plot(gr1)
#'
#' @export
gdrisk <- function(y,x){
  ####################
  ## calculate gdrisk
  ####################
  ny <- length(y)
  # space for results
  result <- list()
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  for (i in 1:ncolx){
    if (typeof(x)=="list"){
      xi <- x[,i]
    } else {
      xi <- x
    }
    # t test by pairs
    itv <- levels(factor(xi))
    citv <- length(itv)
    tv <- as.data.frame(t(combn(itv,2)))
    names(tv) <- c("itv1","itv2")
    tv$t <- NA; tv$df <- NA; tv$sig <- NA; tv$risk <- NA
    for (j in 1:nrow(tv)){
      y1 <- y[which(xi==as.character(tv$itv1[j]))]
      y2 <- y[which(xi==as.character(tv$itv2[j]))]
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
  if (typeof(x)=="list"){
    names(result) <- c(variable)
  } else {
    names(result) <- c("var")
  }

  ## define class
  class(result) <- "gdrisk"
  result
}

print.gdrisk <- function(result){
  lr <- length(result)
  names.result <- names(result)
  plotriskmatrix <- list()
  for (i in 1:lr){
    vec <- result[[i]]
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
  invisible(result)
}

plot.gdrisk <- function(result){
  lr <- length(result)
  names.result <- names(result)
  plotriskmatrix <- list()
  for (i in 1:lr){
    vec <- result[[i]]

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

