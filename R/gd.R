#' Geographical detectors: factor detector.
#'
#' @description Function for calculating power determinant using factor detector
#' of geographical detectors and visualization.
#'
#' @usage gd(formula, data = NULL)
#' \method{print}{gd}(x, ...)
#' \method{plot}{gd}(x, ...)
#'
#' @aliases gd print.gd plot.gd
#'
#' @param formula A formula of response and explanatory variables
#' @param data A data.frame includes response and explanatory variables
#' @param x A list of factor detector results
#' @param ... Ignore
#'
#' @importFrom stats as.formula sd pf
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_x_discrete ylab
#' coord_flip theme_bw
#'
#' @examples 
#' g1 <- gd(NDVIchange ~ Climatezone, data = ndvi_40)
#' g1
#' \donttest{
#' data <- ndvi_40[,1:3]
#' g2 <- gd(NDVIchange ~ ., data = data)
#' g2
#' }
#'
#' @export
#'
gd <- function(formula, data = NULL){
  formula <- as.formula(formula)
  response <- data[,colnames(data) == as.character(formula[[2]])]
  if (formula[[3]]=="."){
    explanatory <- data[,-which(colnames(data) == as.character(formula[[2]]))]
  } else {
    explanatory <- data[,match(all.vars(formula)[-1], colnames(data))]
  }

  ny <- length(response)
  if (typeof(explanatory)=="list"){
    ncolx <- ncol(explanatory)
    variable <- colnames(explanatory)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  result <- as.data.frame(variable)
  result$qv <- NA
  result$sig <- NA
  for (i in 1:ncolx){
    if (typeof(explanatory)=="list"){
      xi <- explanatory[,i]
    } else {
      xi <- explanatory
    }

    # non-central F test
    xx <- levels(factor(xi))
    nx <- length(xx) # number of strata
    na <- c()
    for (u in 1:nx){
      na[u] <- length(response[which(xi==xx[u])]) * sd(response[which(xi==xx[u])])^2
    }
    qv <- 1 - sum(na, na.rm = T)/(length(response)*sd(response)^2)
    Fv <- (ny - nx)/(nx - 1)*qv/(1 - qv)
    m1 <- c(); m2 <- c()
    for (u in 1:nx){
      m1[u] <- mean(response[which(xi==xx[u])])^2
      m2[u] <- sqrt(length(response[which(xi==xx[u])])) * mean(response[which(xi==xx[u])])
    }
    lambda <- (sum(m1) - sum(m2)^2/ny)/sd(response)^2
    p0 <- pf(Fv, df1 = (nx - 1), df2 = (ny - nx), ncp = lambda)
    sig <- 2*(1-p0)
    result$qv[i] <- qv
    result$sig[i] <- sig
  }
  result <- list("Factor" = result)
  ## define class
  class(result) <- "gd"
  result
}

print.gd <- function(x, ...){
  rs0 <- x[[1]]
  print(rs0)
  invisible(x)
}

plot.gd <- function(x, ...){
  variable <- NA; qv <- NA
  rs0 <- x[[1]]
  rs1 <- rs0[order(rs0$qv, decreasing = TRUE),]
  rs2_k <- which(rs1$sig < 0.05)
  if (length(rs2_k) == 0){
    warning("all spatial associations are not significant at the 0.05 level.")
  } else {
    rs2 <- rs1[rs2_k,]
  }
  if (nrow(rs2) > nrow(rs1)){
    nrow21 <- nrow(rs2) - nrow(rs1)
    cat("\n",nrow21,"variable/variables are removed due to the signficance higher than 0.05.\n")
  }
  varname <- as.character(rs2$variable)
  plotg <- ggplot(rs2, aes(x = variable, y = qv)) +
    geom_bar(stat = "identity", fill = "gray70") +
    geom_bar(data = subset(rs2, qv==max(qv)), aes(x = variable, y = qv),
             fill = "orangered", stat = "identity") +
    scale_x_discrete(limits = rev(as.character(rs2$variable))) +
    geom_text(aes(x = variable, y = qv,
                  label = paste(round(qv, digits = 4))),
              colour = "black", size = 4) +
    ylab("Q value") +
    coord_flip() +
    theme_bw()
  plotg
}
