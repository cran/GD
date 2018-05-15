#' Comparison of size effects of spatial units.
#'
#' @description Function for comparison of size effects of spatial units
#' in spatial heterogeneity analysis.
#'
#' @usage sesu(gdlist, su)
#'
#' @param gdlist A list of \code{gdm} result or \code{gd} result
#' @param su A vector of sizes of spatial units
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_discrete
#' xlab ylab theme_bw
#'
#' @examples 
#' ndvilist <- list(ndvi_30, ndvi_40, ndvi_50)
#' su <- c(30,40,50) ## sizes of spatial units
#' gdlist <- list() ## list of all geographical detectors results
#' ## set optional parameters of optimal discretization
#' ## optional methods: equal, natural, quantile, geometric, sd and manual
#' discmethod <- c("equal","natural","quantile")
#' discitv <- c(4:6)
#' ## "gdm" function
#' for (i in 1:length(su)){
#'   ndvidata <- ndvilist[[i]]
#'   gdlist[[i]] <- gdm(NDVIchange ~ Climatezone + Mining + Tempchange + GDP,
#'                      continuous_variable = c("Tempchange", "GDP"),
#'                      data = ndvidata,
#'                      discmethod = discmethod, discitv = discitv)
#' }
#' sesu(gdlist, su) ## size effects of spatial units
#'
#' @export

sesu <- function(gdlist, su){
  if (length(su) < 2){
    cat("\nAt least two sizes of spatial unit are required for comparison.\n\n")
  } else {
    nv <- length(gdlist[[1]]$Factor.detector$Factor$variable)
    sux <- rep(su,each=nv)
    variable <- c()
    qv <- c()
    sig <- c()
    for (i in 1:length(su)){
      variable <- c(variable, as.character(gdlist[[1]]$Factor.detector$Factor$variable))
      qv <- c(qv, gdlist[[i]]$Factor.detector$Factor$qv)
      sig <- c(sig, gdlist[[i]]$Factor.detector$Factor$sig)
    }
    result <- as.data.frame(cbind(sux, qv, sig))
    result$variable <- variable
    result <- result[which(result$sig <= 0.05),]
    plotsu <- ggplot(data = result, aes(x = sux, y = qv, colour = variable)) +
      geom_point() +
      geom_line() +
      scale_color_discrete(limit = variable[!duplicated(variable)]) +
      xlab("Size of spatial unit") +
      ylab("Q value") +
      theme_bw()
    print(plotsu)
  }
}


