#' Optimal discretization and geographical detectors for multiple variables and visulization
#'
#' @usage gdm(y, xcategorical=NULL, xcontinuous=NULL, discmethod, discitv)
#' \\method{print}{gdm}(result)
#' \\method{plot}{gdm}(result)
#'
#' @aliases gdm print.gdm plot.gdm
#'
#' @param y A numeric vector of response variable
#' @param xcategorical A vector or a data.frame of categorical variables
#' @param xcontinuous A vector or a data.frame of continuous variables
#' @param discmethod A character vector of discretization methods
#' @param discitv A numeric vector of numbers of intervals
#' @param result A list of \code{gdm} result
#'
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples
#' ###############
#' ## NDVI: ndvi_40
#' ###############
#' ## define elements orders of categorical variables
#' cz <- c("Bwk","Bsk","Dwa","Dwb","Dwc") ## climate zone
#' mp <- c("very low","low","medium","high","very high") ## mining production
#' ndvi_40$Climatezone <- as.numeric(1:5)[match(ndvi_40$Climatezone, cz)]
#' ndvi_40$Mining <- as.numeric(1:5)[match(ndvi_40$Mining, mp)]
#' ## set optional parameters of optimal discretization
#' ## optional methods: equal, natural, quantile, geometric, sd and manual
#' discmethod <- c("equal","natural","quantile")
#' discitv <- c(4:6)
#' ## "gdm" function
#' ndvigdm <- gdm(y = ndvi_40[,1], xcategorical = ndvi_40[,2:3],
#'                xcontinuous = ndvi_40[,4:7],
#'                discmethod = discmethod, discitv = discitv)
#' # ndvigdm
#' # plot(ndvigdm)
#' #############
#' ## H1N1: h1n1_100
#' #############
#' ### set optional parameters of optimal discretization
#' # discmethod <- c("equal","natural","quantile")
#' # discitv <- c(4:6)
#' ### "gdm" function
#' # h1n1gdm <- gdm(y = h1n1_100[,1], xcategorical = h1n1_100[,11],
#' #               xcontinuous = h1n1_100[,c(2:10)],
#' #               discmethod = discmethod, discitv = discitv)
#' # h1n1gdm
#' # plot(h1n1gdm)
#'
#' @export
#'
#'
#'

gdm <- function(y, xcategorical=NULL, xcontinuous=NULL, discmethod, discitv){
  ### result of optimal discretization
  odclist <- list()
  if (is.null(xcategorical) & is.null(xcontinuous)){
    warning("At least an explanatory variable is required.\n")
  } else if (!is.null(xcategorical) & is.null(xcontinuous)){
    ### categorical variables
    xcg <- xcategorical
    x <- as.data.frame(xcg)
  } else if (is.null(xcategorical) & !is.null(xcontinuous)){
    ### continuous variables
    xct <- xcontinuous
    x <- as.data.frame(xct)
    nx <- ncol(x)
    namesx <- names(x)
    for (i in 1:nx){
      odc1 <- optidisc(y, x[[i]], discmethod, discitv)
      stra.x <- stra(x[[i]], odc1$itv)
      odclist[[i]] <- odc1
      names(odclist)[i] <- namesx[i]
      x[,i] <- stra.x
    }
  } else {
    ### both variables
    ### categorical variables
    xcg <- xcategorical
    xcg <- as.data.frame(xcg)
    ### continuous variables
    xct <- xcontinuous
    xct <- as.data.frame(xct)
    nxct <- ncol(xct)
    namesxct <- names(xct)
    for (i in 1:nxct){
      odc1 <- optidisc(y, xct[[i]], discmethod, discitv)
      stra.x <- stra(xct[[i]], odc1$itv)
      odclist[[i]] <- odc1
      names(odclist)[i] <- namesxct[i]
      xct[,i] <- stra.x
    }
    ### all variables
    x <- cbind(xcg, xct)
  }
  ### geographical detectors
  if (ncol(x)==1){
    ### factor detectors
    gd1 <- gd(y,x)
    ### risk detectors
    gdrm1 <- riskmean(y, x)
    gdr1 <- gdrisk(y, x)
    ### interaction and ecological detectors
    cat("Factor and risk detectors are computed.
        At least two explanatory variables are required for computing
        interaction and ecological detectors.\n")
    gdi1 <- c()
    gde1 <- c()
  } else {
    ### factor detectors
    gd1 <- gd(y,x)
    ### risk detectors
    gdrm1 <- riskmean(y, x)
    gdr1 <- gdrisk(y, x)
    ### interaction detectors
    gdi1 <- gdinteract(y, x)
    ### ecological detectors
    gde1 <- gdeco(y,x)
  }
  ### output
  components <- list("factor.detector"=gd1,"interaction.detector"=gdi1,"ecological.detector"=gde1)
  if (length(odclist)==0){
    result <- list("Factor"=gd1[[1]],"Risk.mean"=gdrm1,"Risk"=gdr1,
                   "Interaction"=gdi1[[1]],"Ecological"=gde1[[1]],"components"=components)
  } else {
    result <- list("discretization"=odclist,"Factor"=gd1[[1]],
                   "Risk.mean"=gdrm1,"Risk"=gdr1,
                   "Interaction"=gdi1[[1]],"Ecological"=gde1[[1]],"components"=components)
  }
  ## define class
  class(result) <- "gdm"
  result
}

print.gdm <- function(result){
  ### print optimal discretization
  if (length(result$discretization)==0){
    cat("Explanatory variables are categorical variables.\n\n")
  } else {
    cat("Explanatory variables include continuous variables.\n\n")
    namesxct <- names(result$discretization)
    cat("Optimal discretization results of ", namesxct, ":\n\n")
    print(result$discretization)
  }
  ### print geographical detectors
  cat("\nGeographical detectors results:\n")
  cat("\nFactor detector:\n")
  print(result$Factor)
  cat("\nRisk detector:\n")
  print(result$Risk.mean)
  print(result$Risk)
  if (length(result$components$interaction.detector) > 0){
    print(result$components$interaction.detector)
    cat("\n")
    print(result$components$ecological.detector)
  }
  invisible(result)
}

plot.gdm <- function(result){
  ### plot optimal discretization
  lrd <- length(result$discretization)

  if (lrd == 0){
    cat("\n\nall explanatory variables are categorical variables ...\n\n")
  } else if (lrd == 1){
    cat("\n\nplot optimal discretization process ...\n\n")
    plotd1 <- plot.optidisc(result$discretization[[1]])
    print(plotd1)
  } else {
    cat("\n\nplot optimal discretization process ...\n\n")
    if (lrd > 1 & lrd <= 4) {
      cols <- 2
    } else if (lrd > 4 & lrd <= 9) {
      cols <- 3
    } else {
      cols <- 4
    }
    layout <- t(matrix(seq(1, cols*ceiling(lrd/cols)),
                       ncol = ceiling(lrd/cols), nrow = cols))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    namesrd <- names(result$discretization)
    for (i in 1:lrd) {
      cat("\n\nplot optimal discretization process of", namesrd[i], "...\n\n")
      plotd <- plot.optidisc(result$discretization[[i]])
      index <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plotd, vp = viewport(layout.pos.row = index$row,
                                               layout.pos.col = index$col))
    }
  }

  if (lrd == 0){
    cat("\n")
  } else if (lrd == 1){
    cat("\n\nplot optimal discretization result ...\n\n")
    plotdd1 <- plot.disc(result$discretization[[1]]$discretization)
    print(plotdd1)
  } else {
    cat("\n\nplot optimal discretization result ...\n\n")
    if (lrd > 1 & lrd <= 4) {
      cols <- 2
    } else if (lrd > 4 & lrd <= 9) {
      cols <- 3
    } else {
      cols <- 4
    }
    layout <- t(matrix(seq(1, cols*ceiling(lrd/cols)),
                       ncol = ceiling(lrd/cols), nrow = cols))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    namesrd <- names(result$discretization)
    for (i in 1:lrd) {
      cat("\n\nplot optimal discretization result of", namesrd[i], "...\n\n")
      plotdd <- plot.disc(result$discretization[[i]]$discretization)
      index <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plotdd, vp = viewport(layout.pos.row = index$row,
                                 layout.pos.col = index$col))
    }
  }

  ### plot geographical detectors
  cat("\n\nplot factor detectors ...\n\n")
  plotf <- plot.gd(result$components$factor.detector)
  print(plotf)
  cat("\n\nplot risk mean values ...\n\n")
  plot.riskmean(result$Risk.mean)
  cat("\n\nplot risk detectors ...\n\n")
  plot.gdrisk(result$Risk)
  if (length(result$components$interaction.detector)>0){
    cat("\n\nplot interaction detectors ...\n\n")
    ploti <- plot.gdinteract(result$components$interaction.detector)
    print(ploti)
    cat("\n\nplot ecological detectors ...\n\n")
    plote <- plot.gdeco(result$components$ecological.detector)
    print(plote)
  }
}
