#' Calculate ecological detectors and ecological matrix and visulization
#'
#' @usage gdeco(y, x)
#' \\method{print}{gdeco}(result)
#' \\method{plot}{gdeco}(result)
#'
#' @aliases gdeco print.gdeco plot.gdeco
#'
#' @param y A numeric vector of response variable
#' @param x A vector or a data.frame of explanatory variables
#' @param result A list of ecological detector results
#'
#' @importFrom stats pf
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual geom_text
#' theme_bw xlab ylab theme scale_x_discrete scale_y_discrete
#'
#' @examples
#' ge1 <- gdeco(ndvi_40$NDVIchange, ndvi_40[,2:3])
#' # ge1
#'
#' @export
gdeco <- function(y,x){
  if (typeof(x)!="list"){
    warning("x should contain multiple variables for ecological detector")
  }
  ncolx <- ncol(x)
  variable <- colnames(x)

  fv <- as.data.frame(t(combn(variable,2)))
  names(fv) <- c("var1","var2")

  fv$f <- NA; fv$sig <- NA; fv$eco <- NA
  for (i in 1:nrow(fv)){
    x1 <- x[,which(variable==as.character(fv$var1[i]))]
    x2 <- x[,which(variable==as.character(fv$var2[i]))]
    # F test
    c1 <- aggregate(y, list(x1), length)
    s1 <- aggregate(y, list(x1), sd)
    if (min(c1$x) == 1){
      s1 <- s1[-which(c1$x == 1),]
      c1 <- c1[-which(c1$x == 1),]
    }
    c2 <- aggregate(y, list(x2), length)
    s2 <- aggregate(y, list(x2), sd)
    if (min(c2$x) == 1){
      s2 <- s2[-which(c2$x == 1),]
      c2 <- c2[-which(c2$x == 1),]
    }
    fv$f[i] <- length(x1)*(length(x2)-1)*sum(c1$x*s1$x^2)/(length(x2)*(length(x1)-1)*sum(c2$x*s2$x^2))
    p0 <- pf(fv$f[i], df1 = (length(x1) - 1), df2 = (length(x2) - 1))
    fv$sig[i] <- 2*(1-p0)
    if (fv$sig[i] < 0.05){
      fv$eco[i] <- c("Y")
    } else {
      fv$eco[i] <- c("N")
    }
  }
  fv <- list("Ecological"=fv)
  ## define class
  class(fv) <- "gdeco"
  fv
}

print.gdeco <- function(result){
  vec <- result[[1]]
  ecomatrix <- v2m(vec$eco, diag=FALSE)
  ecomatrix <- as.data.frame(ecomatrix)
  varname <- c(as.character(vec$var1),as.character(vec$var2))
  variable <- varname[!duplicated(varname)]
  names(ecomatrix) <- variable
  ecomatrix <- cbind(variable, ecomatrix)
  cat("Ecological detector:\n")
  print(ecomatrix)
  invisible(result)
}

plot.gdeco <- function(result){
  resultdata <- result[[1]]
  if (nrow(resultdata)==1){
    cat("At least three explanatory variables are required for visulizing ecological detector.\n")
  } else {
    ecomatrix <- v2m(resultdata$eco, diag=FALSE)
    varname <- c(as.character(resultdata$var1),as.character(resultdata$var2))
    ecovar <- varname[!duplicated(varname)]

    ## plot matrix
    Ei <- c(ecomatrix)
    var1 <- rep(ecovar,each=length(ecovar))
    var2 <- rep(ecovar, length(ecovar))
    Em <- as.data.frame(cbind(var1,var2,Ei))

    plot1 <- ggplot(Em, aes(x = var1, y = var2, fill = Ei)) +
      geom_tile(colour = "white") +
      scale_fill_manual(values = c("Y"="#FFA500","N"="#7FDBFF")) +
      geom_text(aes(x = var1, y = var2,
                    label = ifelse(is.na(Ei), "", paste(Ei))),
                colour = "black", size = 4) +
      theme_bw() +
      xlab("Variable") +
      ylab("Variable") +
      theme(legend.position="none") +
      scale_x_discrete(limits = unique(Em$var1)) +
      scale_y_discrete(limits = rev(unique(Em$var1)))
    plot1
  }
}

