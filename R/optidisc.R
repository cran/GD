#' Optimal discretization
#'
#' @param y A numeric vector of response variable
#' @param x A numeric vector of explanatory variable
#' @param discmethod A character vector of discretization methods
#' @param discitv A numeric vector of numbers of intervals
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw
#' scale_x_continuous scale_y_continuous scale_color_discrete
#' @importFrom reshape2 melt
#'
#' @examples
#' data(Roaddamage)
#' y <- Roaddamage$damage
#' x <- Roaddamage$population
#' discmethod <- c("equal","natural","quantile","geometric","sd")
#' discitv <- c(3:7)
#' optidisc(y, x, discmethod, discitv)
#'
#' @export

optidisc <- function(y = y, x = x, discmethod = discmethod, discitv = discitv){
  n.itv <- length(discitv)
  n.method <- length(discmethod)

  OD <- matrix(NA, n.itv, n.method)
  OD <- as.data.frame(OD)
  names(OD) <- discmethod
  OD <- cbind(discitv,OD)

  for (u in 1:n.itv){ # no.itv
    for (v in 1:n.method){ # discmethod
      dc1 <- disc(x, discitv[u], method = discmethod[v])
      if (min(dc1$c.itv) < 2) {
        # remove number of data within itv < 2 due to sd() in q
        OD[u,v+1] <- NA
      } else {
        stra.var1 <- stra(x, dc1$itv)
        gd1 <- gd(y,stra.var1)
        OD[u,v+1] <- gd1$qv
      }
    }
  }
  # plot optimal discretization results
  value <- c(); variable <- c()
  reshapedata <- melt(OD, id="discitv")
  p1 <- ggplot(data=reshapedata,
               aes(x=discitv, y=value, colour=variable)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name ="Number of intervals") +
    scale_y_continuous(name ="Q value") +
    scale_color_discrete(name = "Method") +
    theme_bw()

  odmatrix <- OD[,-1]
  k <- which(odmatrix==max(odmatrix, na.rm = TRUE))
  nik <- discitv[(k[1]-1)%%5+1]
  dmk <- discmethod[(k[1]-1)%/%5+1]

  dc1 <- disc(x, nik, method = dmk)

  cat("Optimal discretization method:", dmk,
      "Optimal number of interval:", nik,
      sep="\n")
  return(list("method"=dmk, "n.itv"=nik, "itv"=dc1$itv,
              "c.itv"=dc1$c.itv, "plot.itv"=dc1$plot.itv,
              "plot.optidisc"=p1))
}

