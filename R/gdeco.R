#' Geographical detectors: ecological detector.
#'
#' @description Function for ecological detector calculation, ecological
#' matrix and visulization.
#'
#' @usage gdeco(formula, data = NULL)
#' \method{print}{gdeco}(x, ...)
#' \method{plot}{gdeco}(x, ...)
#'
#' @aliases gdeco print.gdeco plot.gdeco
#'
#' @param formula A formula of response and explanatory variables
#' @param data A data.frame includes response and explanatory variables
#' @param x A list of ecological detector results
#' @param ... Ignore
#'
#' @importFrom stats pf
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual geom_text
#' theme_bw xlab ylab theme scale_x_discrete scale_y_discrete
#'
#' @examples 
#' ge1 <- gdeco(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#' ge1
#' \donttest{
#' data <- ndvi_40[,1:3]
#' ge2 <- gdeco(NDVIchange ~ ., data = data)
#' ge2
#' }
#'
#' @export
gdeco <- function(formula, data = NULL){
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
    warning("multiple explanatory variables are required for interaction detector")
  }

  fv <- as.data.frame(t(combn(variable,2)))
  names(fv) <- c("var1","var2")

  fv$f <- NA; fv$sig <- NA; fv$eco <- NA
  for (i in 1:nrow(fv)){
    x1 <- explanatory[,which(variable==as.character(fv$var1[i]))]
    x2 <- explanatory[,which(variable==as.character(fv$var2[i]))]
    # F test
    c1 <- aggregate(response, list(x1), length)
    s1 <- aggregate(response, list(x1), sd)
    if (min(c1$x) == 1){
      s1 <- s1[-which(c1$x == 1),]
      c1 <- c1[-which(c1$x == 1),]
    }
    c2 <- aggregate(response, list(x2), length)
    s2 <- aggregate(response, list(x2), sd)
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

print.gdeco <- function(x, ...){
  vec <- x[[1]]
  ecomatrix <- v2m(vec$eco, diag=FALSE)
  ecomatrix <- as.data.frame(ecomatrix)
  varname <- c(as.character(vec$var1),as.character(vec$var2))
  variable <- varname[!duplicated(varname)]
  names(ecomatrix) <- variable
  ecomatrix <- cbind(variable, ecomatrix)
  cat("Ecological detector:\n")
  print(ecomatrix)
  invisible(x)
}

plot.gdeco <- function(x, ...){
  resultdata <- x[[1]]
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

