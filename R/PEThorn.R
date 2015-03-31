#' Thornthwaite's Potential Evapotranspiration
#'
#' Function estimates the Potential Evapotranspiration (PET) from
#' monthly mean temperatures following Thornthwaite (1948). The
#' original equations were calibrated for U.S.A., and were the first
#' attempt at finding PET from readily available meteorological data.
#'
#' Function \code{\link[SPEI]{thornthwaite}} (package \pkg{SPEI})
#' provides an alternative implementation, with two important
#' differences: (1) Thornthwaite (1948) suggested that the basic
#' equations should be used only up to 26.5 degrees, and then we
#' should use to a parabolic model which was given in tabular
#' form. The \code{PEThorn} uses estimated polynomial coefficients of
#' this parabola for monthly temperatures over 26.5 degrees, whereas
#' \pkg{SPEI} package uses the same basic equations which give much
#' higher values. (2) In winter-seasonal climates, the basic equations
#' can actually give increased PET with cooler climate and shorter
#' period of positive temperatures. The \code{PEThorn} function
#' adjusts the model coefficient (\eqn{I}) for the length of the
#' period with positive temperatures. This is unpublished, but seems
#' to improve the estimates in near-arctic conditions.
#'
#' @seealso Package \pkg{SPEI} has a wider choice of indices of PET,
#' among them Hargreaves index and its modification
#' (\code{\link[SPEI]{hargreaves}}) which is also suitable for
#' standard meteorological data, and has been calibrated globally.
#'
#' @author Jari Oksanen
#' @return Annual potential evapotranspiration in mm. 
#' @examples
#' PEThorn(c(-9.6,-9.3,-4.8,1.4,7.8,13.5,16.5,14.1,8.9,3.3,-2.8,-7.1),
#'    lat=65)
#' @references Thornthwaite C.W. (1948) An approach toward a rational
#' classification of climate. \emph{Geographical Review} 38, 55--94.
#'
#' @param temp Vector of 12 values of monthly mean temperatures in Centigrade. 
#' @param lat Latitude in degrees.
#' @param twist Adapt model to winter-seasonal climate by adjusting
#' the equations to the number of months with positive temperature.
#' 
#' @export
`PEThorn` <-
    function (temp, lat, twist = FALSE)
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
    tandecl <- tandecl[take]
    hot <- temp >= 26.5
    ## coefficients for hot are from fitted parabola to data in
    ## Thornthwaite (1948) Fig. 13, p. 94.
    if (any(hot))
        PE[hot] <- -415.855 + 32.244*temp[hot] - 0.43253*temp[hot]^2
    if (any(!hot)) {
        ## c is daylength per 12h
        c <- -tan(lat/180*pi)*tandecl[!hot]
        c <- acos(pmin(1, pmax(-1, c)))/pi*2
        ## Thornthwaite 1948, eq. 9 and around
        Ival <- sum((temp/5)^1.514)
        ## This is not in the original paper, but it seems that Ival
        ## must be adapted to short growing season in winter-climate,
        ## or shortening period of positive temperatures may increase
        ## PET, and PET can never reach values much below 500mm. This
        ## mainly concerns near-arctic climates where reversals of PET
        ## are common without this twist.
        if (twist)
            Ival <- Ival * 12/length(temp)
        A <- 6.75e-7*Ival^3 - 7.71e-5*Ival^2 + 1.792e-2*Ival + 0.49239
        PE[!hot] <- 16*c*(10*temp[!hot]/Ival)^A
    }
    sum(monlen*PE)
}
