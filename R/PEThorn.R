#' Thornthwaite's Potential Evapotranspiration
#'
#' Function estimates the Potential Evapotranspiration (PET) from
#' monthly mean temperatures following Thornthwaite (1948). The
#' original equations were calibrated for U.S.A., and were the first
#' attempt at finding PET from readily available meteorological
#' data.
#'
#' Thornthwaite (1948) used different models for hot and cool months
#' with limit of 26.5 degrees C. Below 26.5 C, same monthly mean
#' temperature generated lower PET in hot than in cool climate, and
#' this was accommodated for by a \sQuote{heat index}. The monthly
#' contribution to the heat index is \eqn{I = (T/5)^1.514}, where
#' \eqn{T} is the monthly mean temperature, and the heat index is the
#' sum of monthly values. Below 26.5 C the monthly PET for is
#' calculated as \eqn{16 (10 T/I)^A}{16*(10*T/I)^A}, where the
#' exponent \eqn{A} is defined so that with all values of \eqn{I} the
#' monthly PET will be 135 mm at 26.5 degrees C. Thornthwaite (1948)
#' gives a third degree polynomial to find \eqn{A} from \eqn{I}, and
#' this works fairly well within the range \eqn{I = 20 \dots 140}
#' which covers the U.S.A. except Alaska and Northwestern high
#' mountains, but fails badly in hotter and cooler climates. The
#' original polynomial model restricted to the valid range is used
#' with option \code{heatlimitUSA=TRUE}. If this argument is
#' \code{FALSE}, we use instead a more accurate derivation of exponent
#' \eqn{A} that is also valid outside the U.S.A. range, and gives
#' similar results within U.S.A. (this is unpublished and developed
#' here: see the code). Above 26.5 C, Thornthwaite (1948)
#' tabulated PET against monthly mean. The tabulated values seem to be
#' derived from a parabolic model, and we use coefficients of a second
#' degree polynomial fitted to the tabulated values. The monthly PET
#' values are for 12 hours day length, and they are adjusted to the
#' actual day length of the latitude. If \code{daylimit50=TRUE}, we
#' use day length at 50 degrees latitude for all more extreme
#' locations, like suggested by Thornthwaite (1948).
#'
#' Function \code{\link[SPEI]{thornthwaite}} (package \pkg{SPEI})
#' provides an alternative implementation, but it uses the same basic
#' equations also in hot climates (montly mean > 26.5 degrees), and
#' does not restrict the range of accepted \sQuote{heat index}. Small
#' numerical differences are also caused by the algorithms of
#' estimating the day length.
#'
#' @seealso Package \pkg{SPEI} has a wider choice of indices of PET,
#' among them Hargreaves index and its modification
#' (\code{\link[SPEI]{hargreaves}}) which is also suitable for
#' standard meteorological data, and has been calibrated globally.
#'
#' @author Jari Oksanen
#' @return Annual potential evapotranspiration in mm, or monthly
#' values if \code{monthly=TRUE}.
#' @examples
#' PEThorn(c(-9.6,-9.3,-4.8,1.4,7.8,13.5,16.5,14.1,8.9,3.3,-2.8,-7.1),
#'    lat=65)
#' @references Thornthwaite C.W. (1948) An approach toward a rational
#' classification of climate. \emph{Geographical Review} 38, 55--94.
#'
#' @param temp Vector of 12 values of monthly mean temperatures in
#' degrees C.
#' @param lat Latitude in degrees.
#' @param monthly (logical) Return monthly values instead of annual total. 
#' @param heatlimitUSA (logical) Limit heat index to calibrated values
#' within the U.S.A.
#' @param daylimit50 (logical) Do not use latitudes >50 in daylength
#' calculation.
#' @export
`PEThorn` <-
    function(temp, lat, monthly = FALSE, heatlimitUSA = TRUE,
              daylimit50 = TRUE)
{
    tandecl <- c(-0.384925, -0.22606, -0.02156, 0.182725, 0.35105, 0.432297, 0.388417, 0.242569, 0.048042, -0.157378, -0.337603, -0.430845)
    ## Thornthwaite uses 30-days months (Thornthwaite 1948, p. 94). 
    monlen <- c(31,28.24,31,30,31,30,31,31,30,31,30,31)/30
    take <- temp>0
    if (sum(take) == 0)
        return(0)
    PE <- numeric(sum(take))
    monlen <- monlen[take]
    temp <- temp[take]
    ## daylength per 12h
    if (daylimit50) {
        if(lat > 50)
            lat <- 50
        if(lat < -50)
            lat <- -50
    }
    tandecl <- tandecl[take]
    daylen <- -tan(lat/180*pi)*tandecl
    daylen <- acos(pmin(1, pmax(-1, daylen)))/pi*2
    ## coefficients for hot are from fitted parabola to data in
    ## Thornthwaite (1948) Fig. 13, p. 94.
    hot <- temp >= 26.5
    if (any(hot))
        PE[hot] <- -415.855 + 32.244*temp[hot] - 0.43253*temp[hot]^2
    if (any(!hot)) {
        ## Thornthwaite 1948, eq. 9 and around. 
        Ival <- sum((temp/5)^1.514)
        ## Ival is a "heat index" that adjusts the PET curve to local
        ## climate: cooler climates have higher PET with the same
        ## monthly mean temperature. The following equations calibrate
        ## PET so that at temperature 26.5 C the monthly PE is 135
        ## mm. This is valid for range Ival=20..140 (or more exatly
        ## 20.99..139.63) which covers whole U.S.A. except high
        ## mountain Rockies and Alaska, but excludes most of the
        ## world. Below we restrain the Ival at this limit of valid
        ## operation to avoid over-estimation of PET in cool climates
        ## and underestimation in hot tropics.
        if(!heatlimitUSA)
            A <- 2.132686/log(265/Ival)
        else {
            if(Ival < 20)
                Ival <- 20
            if(Ival > 140)
                Ival <- 140
            A <- 6.75e-7*Ival^3 - 7.71e-5*Ival^2 + 1.792e-2*Ival + 0.49239
        }
        PE[!hot] <- 16*(10*temp[!hot]/Ival)^A
    }
    ## PE is for 30-days month with 12h daylight: adjust
    PE <- daylen*monlen*PE
    if (monthly) {
        out <- structure(numeric(12), names = month.abb)
        out[take] <- PE
        out
    } else {
        sum(PE)
    }
}
