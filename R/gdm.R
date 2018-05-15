#' Geographical detectors: a one-step function.
#'
#' @description A one-step function for optimal discretization and geographical detectors for
#' multiple variables and visualization.
#'
#' @usage gdm(formula, continuous_variable = NULL, data = NULL, discmethod, discitv)
#' \method{print}{gdm}(x, ...)
#' \method{plot}{gdm}(x, ...)
#'
#' @aliases gdm print.gdm plot.gdm
#'
#' @param formula A formula of response and explanatory variables
#' @param continuous_variable A vector of continuous variable names
#' @param data A data.frame includes response and explanatory variables
#' @param discmethod A character vector of discretization methods
#' @param discitv A numeric vector of numbers of intervals
#' @param x A list of \code{gdm} result
#' @param ... Ignore
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
#' ndvigdm <- gdm(NDVIchange ~ Climatezone + Mining + Tempchange + GDP,
#'                continuous_variable = c("Tempchange", "GDP"),
#'                data = ndvi_40,
#'                discmethod = discmethod, discitv = discitv)
#' ndvigdm
#' # plot(ndvigdm)
#' \dontrun{
#' #############
#' ## H1N1: h1n1_100
#' #############
#' ## set optional parameters of optimal discretization
#' discmethod <- c("equal","natural","quantile")
#' discitv <- c(4:6)
#' continuous_variable <- colnames(h1n1_100)[-c(1,11)]
#' ## "gdm" function
#' h1n1gdm <- gdm(H1N1 ~ .,
#'                continuous_variable = continuous_variable,
#'                data = h1n1_100,
#'                discmethod = discmethod, discitv = discitv)
#' h1n1gdm
#' plot(h1n1gdm)
#' }
#'
#' @export

gdm <- function(formula, continuous_variable = NULL, data = NULL, discmethod, discitv){
  formula <- as.formula(formula)
  response <- data[,colnames(data) == as.character(formula[[2]])]
  if (formula[[3]]=="."){
    explanatory <- data[,-which(colnames(data) == as.character(formula[[2]]))]
  } else {
    explanatory <- data[,match(all.vars(formula)[-1], colnames(data))]
  }


  ### result of optimal discretization
  if (!is.null(continuous_variable)){
    explanatory_continuous <- data[,match(continuous_variable, colnames(data))]
    explanatory_continuous <- as.data.frame(explanatory_continuous)
    names(explanatory_continuous) <- continuous_variable
    n_continuous <- ncol(explanatory_continuous)
    odc1 <- optidisc(explanatory_continuous, response, discmethod, discitv)

    for (i in 1:n_continuous){
      stra.x <- stra(explanatory_continuous[[i]], odc1[[i]]$itv)
      explanatory_continuous[,i] <- stra.x
    }

    explanatory[,match(continuous_variable, colnames(explanatory))] <- explanatory_continuous
  }

  response <- as.data.frame(response)
  names(response) <- as.character(formula[[2]])
  data <- cbind(response, explanatory)


  ### geographical detectors

  ### factor detectors
  gd1 <- gd(formula, data)
  ### risk detectors
  gdrm1 <- riskmean(formula, data)
  gdr1 <- gdrisk(formula, data)

  if (ncol(explanatory)==1){
   ### interaction and ecological detectors
    cat("Factor and risk detectors are computed.
        At least two explanatory variables are required for computing
        interaction and ecological detectors.\n")
    gdi1 <- c()
    gde1 <- c()
  } else {
    ### interaction detectors
    gdi1 <- gdinteract(formula, data)
    ### ecological detectors
    gde1 <- gdeco(formula, data)
  }


  ### output
  if (is.null(continuous_variable)){
    result <- list("Factor.detector"=gd1,"Risk.mean"=gdrm1,"Risk.detector"=gdr1,
                   "Interaction.detector"=gdi1,"Ecological.detector"=gde1)
  } else {
    result <- list("Discretization"=odc1,"Factor.detector"=gd1,
                   "Risk.mean"=gdrm1,"Risk.detector"=gdr1,
                   "Interaction.detector"=gdi1,"Ecological.detector"=gde1)
  }
  ## define class
  class(result) <- "gdm"
  result
}



print.gdm <- function(x, ...){
  ### print optimal discretization
  if (length(x$Discretization)==0){
    cat("Explanatory variables are categorical variables.\n\n")
  } else {
    cat("Explanatory variables include continuous variables.\n\n")
    namesxct <- names(x$Discretization)
    cat("Optimal discretization results of ", namesxct, ":\n\n")
    print(x$Discretization)
  }
  ### print geographical detectors
  cat("\nGeographical detectors results:\n")
  cat("\nFactor detector:\n")
  print(x$Factor.detector)
  cat("\nRisk detector:\n")
  print(x$Risk.mean)
  print(x$Risk.detector)
  if (length(x$Interaction.detector) > 0){
    print(x$Interaction.detector)
    cat("\n")
    print(x$Ecological.detector)
  }
  invisible(x)
}

plot.gdm <- function(x, ...){
  ### plot optimal discretization
  lrd <- length(x$Discretization)

  if (lrd == 0){
    cat("\n\nall explanatory variables are categorical variables ...\n\n")
  } else {
    plot.optidisc(x$Discretization)
  }


  ### plot geographical detectors
  cat("\nplot factor detectors ...\n\n")
  plotf <- plot.gd(x$Factor.detector)
  print(plotf)
  cat("\nplot risk mean values ...\n\n")
  plot.riskmean(x$Risk.mean)
  cat("\nplot risk detectors ...\n\n")
  plot.gdrisk(x$Risk.detector)
  if (length(x$Interaction.detector)>0){
    cat("\nplot interaction detectors ...\n\n")
    ploti <- plot.gdinteract(x$Interaction.detector)
    print(ploti)
    cat("\nplot ecological detectors ...\n")
    plote <- plot.gdeco(x$Ecological.detector)
    print(plote)
  }
}
