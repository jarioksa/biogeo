#' biogeo.
#'
#' @name biogeo
#' @docType package
NULL

### Document datasets
#' Initial Landscape Classification and Climatic Parameters
#'
#' A dataset on terrestrial landscape types and climatic parameters in
#' global 0.5 degree grid.
#'
#' @format A data frame 59871 rows and 17 variables:
#' \describe{
#'   \item{lon,lat}{Longitude and Latitude of the centre point of the
#'    0.5 degree cell.}
#'   \item{sagetype}{Landscape type, source \url{http://sage.wisc.edu/}.}
#'   \item{Tmean}{Annual mean temperature.}
#'   \item{Tmomin}{Mean temperature of the coldest month.}
#'   \item{Tmomax}{Mean temperature of the warmest month.}
#'   \item{PET}{Thornthwaite's Potential Evapotranspiration
#'     calculated with \code{\link{PEThorn}} with options
#'     \code{heatlimitUSA=FALSE} and \code{daylimit50=FALSE}.}
#'   \item{Cont}{Conrad's thermal continentality index.}
#'   \item{Prectot}{Total annual precipitation in mm.}
#'   \item{Precsummer}{Precipitation in summer half-year.}
#'   \item{PEdiff}{Difference \code{PET - Prectot}.}
#'   \item{PrecCV}{Coefficient of variation of monthly precipitation.}
#'   \item{PEratio}{Ratio of \code{Prectot/PET}. This is taken as
#'   \eqn{1} if \code{PET == 0}.}
#'   \item{Precsummerat}{Ratio of \code{Precsummer} and \code{Prec}.}
#'   \item{PETHar, PETHarM}{Hargreaves PET and modified Hargreaves PET
#'     calculated with \code{\link[SPEI]{hargreaves}} (\pkg{SPEI} package).}
#'   \item{TDayRange}{Mean difference of daily minimum and maximum
#'     temperatures.}
#' }
"climaveg"
