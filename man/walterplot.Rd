% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walterplot.R
\name{walterplot}
\alias{walterplot}
\title{Walter Diagram of Climate}
\usage{
walterplot(temp, prec, title, south = FALSE, ylim, mrange, lwd = 3)
}
\arguments{
\item{temp}{Vector of twelve monthly temperature values.}

\item{prec}{Vector of twelve monthly precipitation values.}

\item{title}{Main title of the graph.}

\item{south}{(logical): is the location in southern hemisphere?}

\item{ylim}{limits of y-axis in temperature units. The graph will
not be clipped, but the argument can be used to extend the axes.}

\item{mrange}{Minimum range of y-axis in temperature units.}

\item{lwd}{Line width of temperature and precipitation lines.}
}
\description{
Function draws a simplified Walter diagram of monthly temperature
and precipitation.
}
\details{
Walter diagram displays monthly temperature and precipitation
together in the same graph with blue line for precipitation and red
line for temperature. The diagram is scaled so that 10 degrees C
corresponds to 20 mm precipitation. When precipitation line is
above temperature, the period is regarded as humid and coloured
skyblue in the diagram, and when precipitation line is below
temperature, period is regarded as arid and coloured yellow. Dark
blue us used when monthly precipitation exceeds 100 mm.

The function draws a simplified diagram intended to be used in
general presentations and slides. For proper diagrams you need to
find something else.
}
\examples{
walterplot(c(-9.6,-9.3,-4.8,1.4,7.8,13.5,16.5,14.1,8.9,3.3,-2.8,-7.1),
c(31,26,26,20,37,46,71,65,44,45,36,30), "Oulu")

}
\author{
Jari Oksanen
}
\references{
Walter, H. (1985) Vegetation of the Earth and
ecological systems of the geo-biosphere. 3rd ed. Springer.
}

