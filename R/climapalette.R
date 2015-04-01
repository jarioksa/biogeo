#' Climatic Colour Codes for Vegetation Types
#'
#' Function finds RGB colour codes for vegetation types using three
#' variables. Choosing climatic variables matching the parameter names
#' gives maximum values to equatorial rainforest with abundant rains,
#' equitable non-continental temperature and high temperature.
#'
#' @author Jari Oksanen
#'
#' @examples
#' data(climaveg)
#' clr <- with(climaveg, climapalette(sagetype, -PEratio, Cont, -PET))
#' with(climaveg, plot(lon, lat, pch=15, cex=0.3, asp=1, col = clr[sagetype]))
#' 
#' @param type Community type.
#' @param humidity,continentality,temperature Environmental variables
#' used for red, green and blue in colours.
#'
#' @export
`climapalette` <-
    function (type, humidity, continentality, temperature) 
{
    r <- tapply(humidity, type, mean)
    g <- tapply(continentality, type, mean)
    b <- tapply(temperature, type, mean)
    r <- (r - min(r))/diff(range(r))
    g <- (g - min(g))/diff(range(g))
    b <- (b - min(b))/diff(range(b))
    rgb(red = r, green = g, blue = b)
}
