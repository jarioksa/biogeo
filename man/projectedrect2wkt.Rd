% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectedrect2wkt.R
\name{projectedrect2wkt}
\alias{projectedrect2wkt}
\title{Returns a WKT polygon in long/lat of a projected rectangle}
\usage{
projectedrect2wkt(object, knots = 7)
}
\arguments{
\item{object}{An object with an extent in projected coordinates}

\item{knots}{Number of knots filled between corner points of the
bounding points when defining the polygon.}
}
\description{
Function returns a WKT polygon that gives the limits of a projected
map in longitude/latitude. This polygon can be used to force query
of species occurrence query to the area of projected map instead of
longitude/latitude rectangle (that is not a rectangle in a
projected map).
}

