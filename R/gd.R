#' Calculate geographical detectors
#'
#' @usage gd(y, x)
#' \\method{print}{gd}(result)
#' \\method{plot}{gd}(result)
#'
#' @aliases gd print.gd plot.gd
#'
#' @param y A numeric vector of response variable
#' @param x A vector or a data.frame of explanatory variables
#' @param result A list of factor detector results
#'
#' @importFrom stats sd pf
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_x_discrete ylab
#' coord_flip theme_bw
#'
#' @examples
#' g1 <- gd(ndvi_40$NDVIchange, ndvi_40$Climatezone)
#' # g1
#'
#' @export
#'
gd <- function(y, x){
  ny <- length(y)
  if (typeof(x)=="list"){
    ncolx <- ncol(x)
    variable <- colnames(x)
  } else {
    ncolx <- 1
    variable <- c("var")
  }

  result <- as.data.frame(variable)
  result$qv <- NA
  result$sig <- NA
  for (i in 1:ncolx){
    if (typeof(x)=="list"){
      xi <- x[,i]
    } else {
      xi <- x
    }

    # non-central F test
    xx <- levels(factor(xi))
    nx <- length(xx) # number of strata
    na <- c()
    for (u in 1:nx){
      na[u] <- length(y[which(xi==xx[u])]) * sd(y[which(xi==xx[u])])^2
    }
    qv <- 1 - sum(na, na.rm = T)/(length(y)*sd(y)^2)
    Fv <- (ny - nx)/(nx - 1)*qv/(1 - qv)
    m1 <- c(); m2 <- c()
    for (u in 1:nx){
      m1[u] <- mean(y[which(xi==xx[u])])^2
      m2[u] <- sqrt(length(y[which(xi==xx[u])])) * mean(y[which(xi==xx[u])])
    }
    lambda <- (sum(m1) - sum(m2)^2/ny)/sd(y)^2
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

print.gd <- function(result){
  rs0 <- result[[1]]
  print(rs0)
  invisible(result)
}

plot.gd <- function(result){
  variable <- NA; qv <- NA
  rs0 <- result[[1]]
  rs1 <- rs0[order(rs0$qv, decreasing = TRUE),]
  rs2 <- rs1[which(rs1$sig < 0.05),]
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
