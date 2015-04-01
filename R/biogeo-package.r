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
#'     calculated with \code{\link{PEThorn}}.}
#'   \item{Cont}{Conrad's thermal continentality index.}
#'   \item{Prectot}{Total annual precipitation in mm.}
#'   \item{Precsummer}{Precipitation in summer half-year.}
#'   \item{PEdiff}{Difference of \code{Prectot} and \code{PET}.}
#'   \item{PrecCV}{Coefficient of variation of monthly precipitation.}
#'   \item{PEratio}{Ratio of \code{Prec} and \code{PET}.}
#'   \item{Precsummerat}{Ratio of \code{Precsummer} and \code{Prec}.}
#'   \item{PETtwist}{\code{PET} with a twist of \code{\link{PEThorn}}.}
#'   \item{PETHar, PETHarM}{Hargreaves PET and modified Hargreaves PET
#'     calculated with \code{\link[SPEI]{hargreaves}} (\pkg{SPEI} package).}
#' }
"climaveg"
