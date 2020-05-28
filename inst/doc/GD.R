## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, results="hide", message=FALSE--------------------------------
library("badger")

## ---- echo = FALSE, results='asis'--------------------------------------------
cat(
	badge_cran_release("GD", "orange"),
	badge_cran_download("GD", "grand-total", "blue"),
	badge_cran_download("GD", "last-month", "yellow"),
	badger::badge_custom("Publication doi", "10.1080/15481603.2020.1760434", "green", "https://doi.org/10.1080/15481603.2020.1760434")
)

## ---- echo = FALSE, results='asis'--------------------------------------------
cat(
  "`GD`",
	badge_cran_download("GD", "grand-total", "orange"),
  "`SK`",
	badge_cran_download("SK", "grand-total", "blue"),
	"`EDA`",
	badge_cran_download("EDA", "grand-total", "yellow"),
	"`HS`",
	badge_cran_download("HS", "grand-total", "green")
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("GD")

## -----------------------------------------------------------------------------
library("GD")
data("ndvi_40")
head(ndvi_40)[1:3,]

## ---- eval = FALSE------------------------------------------------------------
#  ## discretization methods: equal, natural, quantile (default), geometric, sd and manual
#  ds1 <- disc(ndvi_40$Tempchange, 4)
#  ds1
#  plot(ds1)

## ---- eval = FALSE------------------------------------------------------------
#  ## set optional discretization methods and numbers of intervals
#  discmethod <- c("equal","natural","quantile","geometric","sd")
#  discitv <- c(4:7)
#  ## optimal discretization
#  odc1 <- optidisc(NDVIchange ~ Tempchange, data = ndvi_40,
#                   discmethod, discitv)
#  odc1
#  plot(odc1)

## ---- eval = FALSE------------------------------------------------------------
#  ## NDVI: ndvi_40
#  ## set optional parameters of optimal discretization
#  ## optional methods: equal, natural, quantile, geometric, sd and manual
#  discmethod <- c("equal","natural","quantile")
#  discitv <- c(4:6)
#  ## "gdm" function
#  ndvigdm <- gdm(NDVIchange ~ Climatezone + Mining + Tempchange + GDP,
#                 continuous_variable = c("Tempchange", "GDP"),
#                 data = ndvi_20,
#                 discmethod = discmethod, discitv = discitv)
#  ndvigdm
#  plot(ndvigdm)
#  
#  ## H1N1: h1n1_100
#  ## set optional parameters of optimal discretization
#  discmethod <- c("equal","natural","quantile","geometric","sd")
#  discitv <- c(3:7)
#  continuous_variable <- colnames(h1n1_100)[-c(1,11)]
#  ## "gdm" function
#  h1n1gdm <- gdm(H1N1 ~ .,
#                 continuous_variable = continuous_variable,
#                 data = h1n1_100,
#                 discmethod = discmethod, discitv = discitv)
#  h1n1gdm
#  plot(h1n1gdm)

## ---- eval = FALSE------------------------------------------------------------
#  ## a categorical explanatory variable
#  g1 <- gd(NDVIchange ~ Climatezone, data = ndvi_40)
#  g1
#  
#  ## multiple categorical explanatory variables
#  g2 <- gd(NDVIchange ~ ., data = ndvi_40[,1:3])
#  g2
#  plot(g2)
#  
#  ## multiple variables inclusing continuous variables
#  discmethod <- c("equal","natural","quantile","geometric","sd")
#  discitv <- c(3:7)
#  data.ndvi <- ndvi_40
#  
#  data.continuous <- data.ndvi[, c(1, 4:7)]
#  odc1 <- optidisc(NDVIchange ~ ., data = data.continuous, discmethod, discitv)
#  data.continuous <- do.call(cbind, lapply(1:4, function(x)
#    data.frame(cut(data.continuous[, -1][, x], unique(odc1[[x]]$itv), include.lowest = TRUE))))
#      # add stratified data to explanatory variables
#  data.ndvi[, 4:7] <- data.continuous
#  
#  g3 <- gd(NDVIchange ~ ., data = data.ndvi)
#  g3
#  plot(g3)

## ---- eval = FALSE------------------------------------------------------------
#  ## categorical explanatory variables
#  rm1 <- riskmean(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#  rm1
#  plot(rm1)
#  ## multiple variables inclusing continuous variables
#  rm2 <- riskmean(NDVIchange ~ ., data = data.ndvi)
#  rm2
#  plot(rm2)

## ---- eval = FALSE------------------------------------------------------------
#  ## categorical explanatory variables
#  gr1 <- gdrisk(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#  gr1
#  plot(gr1)
#  ## multiple variables inclusing continuous variables
#  gr2 <- gdrisk(NDVIchange ~ ., data = data.ndvi)
#  gr2
#  plot(gr2)

## ---- eval = FALSE------------------------------------------------------------
#  ## categorical explanatory variables
#  gi1 <- gdinteract(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#  gi1
#  ## multiple variables inclusing continuous variables
#  gi2 <- gdinteract(NDVIchange ~ ., data = data.ndvi)
#  gi2
#  plot(gi2)

## ---- eval = FALSE------------------------------------------------------------
#  ## categorical explanatory variables
#  ge1 <- gdeco(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
#  ge1
#  ## multiple variables inclusing continuous variables
#  gd3 <- gdeco(NDVIchange ~ ., data = data.ndvi)
#  gd3
#  plot(gd3)

## ---- eval = FALSE------------------------------------------------------------
#  ndvilist <- list(ndvi_20, ndvi_30, ndvi_40, ndvi_50)
#  su <- c(20,30,40,50) ## sizes of spatial units
#  ## "gdm" function
#  gdlist <- lapply(ndvilist, function(x){
#    gdm(NDVIchange ~ Climatezone + Mining + Tempchange + GDP,
#        continuous_variable = c("Tempchange", "GDP"),
#        data = x, discmethod = "quantile", discitv = 6)
#  })
#  sesu(gdlist, su) ## size effects of spatial units

