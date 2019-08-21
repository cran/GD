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
#' @importFrom stats pf sd
#' @importFrom utils combn
#' @importFrom graphics plot axis text box par
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
  formula.vars <- all.vars(formula)
  response <- subset(data, select = formula.vars[1]) # debug: use subset to select data
  if (formula.vars[2] == "."){
    explanatory <- subset(data, select = -match(formula.vars[1], colnames(data)))
  } else {
    explanatory <- subset(data, select = formula.vars[-1])
  }
  ncolx <- ncol(explanatory)

  if (ncolx == 1){
    stop("multiple explanatory variables are required for interaction detector")
  }

  variable <- colnames(explanatory)
  fv <- as.data.frame(t(combn(variable,2)))
  names(fv) <- c("var1","var2")

  FunF <- function(y, x1, x2){
    c1 <- tapply(y, x1, length); s1 <- tapply(y, x1, sd) # debug: replace aggregate by tapply
    c2 <- tapply(y, x2, length); s2 <- tapply(y, x2, sd)
    c1 <- c1[c1 > 1]; s1 <- s1[c1 > 1]
    c2 <- c2[c2 > 1]; s2 <- s2[c2 > 1]
    nx1 <- length(x1); nx2 <- length(x2)
    fvalue <- nx1 * (nx2 - 1) * sum(c1 * s1^2)/(nx2 * (nx1 - 1) * sum(c2 * s2^2))
    sig <- 1 - pf(fvalue, df1 = nx1 - 1, df2 = nx2 - 1)
    eco <- ifelse(sig < 0.05, "Y", "N")
    eco <- factor(eco, levels = c("Y", "N"))
    result <- cbind(data.frame(f = fvalue, sig), eco)
    return(result)
  }

  y <- response[, 1]
  f.eco <- do.call(rbind, lapply(1:nrow(fv), function(x){ # debug: use lapply to replace for loop
    x1 <- explanatory[,which(variable==as.character(fv$var1[x]))]
    x2 <- explanatory[,which(variable==as.character(fv$var2[x]))]
    f <- FunF(y, x1, x2)
  }))

  fv <- cbind(fv, f.eco)
  fv <- list("Ecological" = fv)
  ## define class
  class(fv) <- "gdeco"
  fv
}

print.gdeco <- function(x, ...){
  vec <- x[[1]]
  ecomatrix <- v2m(as.character(vec$eco), diag=FALSE) # debug: add as.character
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
  if (nrow(resultdata) == 1){
    stop("At least three explanatory variables are required for visulizing ecological detector.\n")
  } else {
    varname <- unique(c(as.character(resultdata$var1),as.character(resultdata$var2)))
    matrix.dim <- length(varname)

    ecomatrix <- v2m(as.character(resultdata$eco), diag = FALSE)
    ecomatrix <- t(ecomatrix[-1, -matrix.dim])

    col.vector <- as.character(resultdata$eco)
    col.vector <- ifelse(col.vector == "Y", "#FFA500", "#7FDBFF")
    col.matrix <- v2m(col.vector, diag = FALSE)
    col.matrix <- t(col.matrix[-1, -matrix.dim])

    # debug: use plot to increase speed
    par(pty = "s")
    plot(row(ecomatrix), col(ecomatrix),
         cex = 30/(matrix.dim - 1), pch = 15, col = col.matrix,
         xlim = c(0.5, (matrix.dim - 1) + 0.5), ylim = c(0.5, (matrix.dim - 1) + 0.5),
         axes = FALSE, ann = FALSE, asp = 1)
    axis(1, at = 1:(matrix.dim - 1), labels = varname[1:(matrix.dim - 1)])
    axis(2, at = 1:(matrix.dim - 1), labels = varname[2:matrix.dim], las = 1)
    title(xlab = "Variable")
    text(row(ecomatrix), col(ecomatrix), labels = ecomatrix)
    box()
    par(pty = "m")
  }
}

