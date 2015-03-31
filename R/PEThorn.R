#' Thornthwaite's Potential Evapotranspiration
#'
#' @references Thornthwaite C.W. (1948) An approach toward a rational
#' classification of climate. \emph{Geographical Review} 38, 55--94.
#'
#' @param temp Vector of 12 values of monthly mean temperatures in Centigrade. 
#' @param lat Latitude in degrees.
#' 
#' @export
`PEThorn` <-
    function (temp, lat) 
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
        A <- 6.75e-7*Ival^3 - 7.71e-5*Ival^2 + 1.792e-2*Ival + 0.49239
        PE[!hot] <- 16*c*(10*temp[!hot]/Ival)^A
    }
    sum(monlen*PE)
}
