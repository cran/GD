#' Calculate interaction detectors and visualization. The types of interactions
#' include "Enhance, nonlinear", "Independent", "Enhance, bi-",
#' "Weaken, uni-" and "Weaken, nonlinear".
#'
#' @usage gdinteract(y, x)
#' \\method{print}{gdinteract}(result)
#' \\method{plot}{gdinteract}(result)
#'
#' @aliases gdinteract print.gdinteract plot.gdinteract
#'
#' @param y A numeric vector of response variable
#' @param x A vector or a data.frame of explanatory variables
#' @param result A list of interaction detector results
#'
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot aes geom_point scale_x_discrete scale_y_discrete
#' theme_bw
#'
#' @examples
#' gi1 <- gdinteract(ndvi_40$NDVIchange, ndvi_40[,2:3])
#' # gi1
#'
#' @export
gdinteract <- function(y, x){
  ####################
  ## calculate gdinteract
  ####################
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    warning("x should be a dataframe with more than two variables")
  }
  result <- as.data.frame(t(combn(variable,2)))
  names(result) <- c("var1","var2")
  result$qv1 <- NA
  result$qv2 <- NA
  result$qv12 <- NA
  result$interaction <- NA
  for (u in 1:nrow(result)){
    x1 <- x[,which(variable==result$var1[u])]
    x2 <- x[,which(variable==result$var2[u])]
    x12 <- stra2v(x1,x2)
    gd1 <- gd(y, x1)[[1]]
    gd2 <- gd(y, x2)[[1]]
    gd12 <- gd(y, x12)[[1]]
    qv1 <- gd1$qv; qv2 <- gd2$qv; qv12 <- gd12$qv
    result$qv1[u] <- gd1$qv
    result$qv2[u] <- gd2$qv
    result$qv12[u] <- gd12$qv
    if (gd12$qv < min(gd1$qv, gd2$qv)) {
      result$interaction[u] <- c("Weaken, nonlinear")
    } else if (gd12$qv >= min(gd1$qv, gd2$qv) & gd12$qv <= max(gd1$qv, gd2$qv)) {
        result$interaction[u] <- c("Weaken, uni-")
    } else if (gd12$qv > max(gd1$qv, gd2$qv) & gd12$qv < gd1$qv + gd2$qv) {
          result$interaction[u] <- c("Enhance, bi-")
    } else if (gd12$qv == gd1$qv + gd2$qv) {
            result$interaction[u] <- c("Independent")
    } else {
      result$interaction[u] <- c("Enhance, nonlinear")
      }
  }
  result <- list("Interaction"=result)
  ## define class
  class(result) <- "gdinteract"
  result
}

print.gdinteract <- function(result){
  vec <- result[[1]]
  intmatrix <- v2m(round(vec$qv12, digits=4), diag=FALSE)
  intmatrix <- as.data.frame(intmatrix)
  varname <- c(as.character(vec$var1),as.character(vec$var2))
  variable <- varname[!duplicated(varname)]
  names(intmatrix) <- variable
  intmatrix <- cbind(variable, intmatrix)
  cat("Interaction detector:\n")
  print(intmatrix)
  invisible(result)
}

plot.gdinteract <- function(result){
  var1 <- NA; var2 <- NA; qv12 <- NA
  resultdata <- result[[1]]
  if (nrow(resultdata)==1){
    cat("At least three explanatory variables are required for visulizing interaction detector.\n")
  } else {
    varname <- c(as.character(resultdata$var1),as.character(resultdata$var2))
    vars <- varname[!duplicated(varname)]
    plot1 <- ggplot(resultdata, aes(x = var1, y = var2)) +
      geom_point(aes(x = var1, y = var2, size = qv12, fill = interaction), shape = 21) +
      scale_fill_manual(breaks = c("Enhance, nonlinear", "Independent", "Enhance, bi-",
                                   "Weaken, uni-", "Weaken, nonlinear") ,
                        values = c("Enhance, nonlinear"="#EA4848",
                                   "Independent"="#E08338",
                                   "Enhance, bi-"="#F2C55E",
                                   "Weaken, uni-"="#6EE9EF",
                                   "Weaken, nonlinear"="#558DE8")) +
      geom_text(aes(x = var1, y = var2,
                    label = ifelse(qv12==max(qv12, na.rm = TRUE), paste(format(qv12, digits=4)),"")),
                colour = "black", size = 4) +
      scale_x_discrete(limits=c(vars[1:(length(vars)-1)])) +
      scale_y_discrete(limits=c(vars[length(vars):2])) +
      theme_bw()
    plot1
  }
}

