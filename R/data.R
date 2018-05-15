#' @title Spatial datasets of vegetation index changes.
#'
#' @description "NDVI" dataset is the NDVI change data from 2010 to 2014 in Inner Mongolia, China.
#' This dataset consists of NDVI change and potential variables sampled from six sizes of
#' spatial grid units, including 5 km, 10 km, 20 km, 30 km, 40 km and 50 km.
#' The references of more details and data sources will be added.
#'
#' @name NDVI
#' @format \code{ndvi_5}: A data frame with 46295 rows and 7 variables
#' \code{ndvi_10}: A data frame with 11567 rows and 7 variables
#' \code{ndvi_20}: A data frame with 2892 rows and 7 variables
#' \code{ndvi_30}: A data frame with 1290 rows and 7 variables
#' \code{ndvi_40}: A data frame with 713 rows and 7 variables
#' \code{ndvi_50}: A data frame with 469 rows and 7 variables
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset NDVI
"ndvi_5"

#' @rdname NDVI
"ndvi_10"
#' @rdname NDVI
"ndvi_20"
#' @rdname NDVI
"ndvi_30"
#' @rdname NDVI
"ndvi_40"
#' @rdname NDVI
"ndvi_50"

#' @title Spatial datasets of H1N1 flu incidences
#'
#' @description "H1N1" dataset is the provincial statistical incidences of
#' influenza A virus subtype H1N1 in China in 2013.
#' This dataset consists of H1N1 incidences and potential variables sampled from four sizes of
#' spatial grid units, including 10 km, 20 km, 50 km and 100 km.
#' The references of more details and data sources will be added.
#'
#' @name H1N1
#' @format \code{h1n1_50}: A data frame with 3977 rows and 11 variables
#' \code{h1n1_100}: A data frame with 987 rows and 11 variables
#' \code{h1n1_150}: A data frame with 443 rows and 11 variables
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset H1N1
"h1n1_50"

#' @rdname H1N1
"h1n1_100"
#' @rdname H1N1
"h1n1_150"

#' @title Spatial datasets of road damage conditions.
#'
#' @description "road_GD" dataset is the road damage conditions on 5000 road segments,
#' together with the potential factors of speed limit, local soil type,
#' population within 1-km buffer around the road, and the daily vechile volumes.
#' More detials about the data can be found in Yongze Song (2018)
#' <doi:10.1109/TITS.2018.2805817>.
#'
#' @usage data(road_GD)
#'
#' @name road_GD
#' @format A data frame with 5000 rows and 5 variables
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset road_GD
#'
#' @references Song, Y., et al., 2018. Traffic Volume Prediction With
#' Segment-Based Regression Kriging and its Implementation in Assessing
#' the Impact of Heavy Vehicles.
#' IEEE Transactions on Intelligent Transportation Systems.
"road_GD"
