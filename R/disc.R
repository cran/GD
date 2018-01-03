#' Convert continuous data to discretized data
#'
#' @param var A numeric vector
#' @param n A number
#' @param method A character
#' @param ManualItv A numeric vector
#'
#' @importFrom stats na.omit quantile
#' @importFrom ggplot2 ggplot aes geom_histogram theme_bw labs geom_vline
#' @importFrom BAMMtools getJenksBreaks
#'
#' @examples
#' disc(rnorm(100,0,2), 6, method = "quantile")
#' disc(c(1:10,15:20,5:30,10:25), 5, method = "natural")
#' data(Roaddamage)
#' disc(Roaddamage$population, 6, method = "quantile")
#'
#' @export

disc <- function(var, n, method = "quantile", ManualItv){
  if (!is.numeric(var))
    stop("var is not numeric")
  if (any(is.na(var))) {
    warning("var has missing values, omitted in finding classes")
    var <- c(na.omit(var))
  }
  if (method == "equal") {
    itv <- seq(min(var), max(var), length.out = (n + 1))
  } else if (method == "natural") {
    itv <- getJenksBreaks(var, (n + 1))
  } else if (method == "quantile") {
    itv <- quantile(var, probs = seq(0, 1, length = n + 1))
  } else if (method == "geometric") {
    k <- (max(var)/min(var))^(1/n)
    itv <- c(min(var),(min(var)*(k^seq(n))))
  } else if (method == "sd") {
    svar <- scale(var) # standardization: (x-mean)/sd
    pars <- c(attr(svar, "scaled:center"), attr(svar, "scaled:scale"))
    names(pars) <- c("center", "scale")
    sitv <- pretty(x = svar, n = n)
    itv <- c((sitv * pars[2]) + pars[1])
  } else if (method == "manual") {
    if (!is.null(ManualItv)){
      itv <- ManualItv
    } else {
      warning("Input manual interval vector")
    }
  }
  c.itv <- c()
  for (u in 1:n){
    c.itv[u] <- length(which(var>=itv[u] & var<=itv[u+1]))
  }
  var <- as.data.frame(var)
  gg1 <- ggplot(data=var, aes(var)) +
    geom_histogram(breaks=seq(min(var), max(var), by = ((max(var) - min(var))/30))) +
    theme_bw() +
    labs(x = "variable", y = "Frequency") +
    geom_vline(xintercept=itv, color = "red")
  return(list("itv"=itv, "c.itv"=c.itv, "plot.itv"=gg1))
}







