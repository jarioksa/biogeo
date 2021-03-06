# biogeo

The main purpose of these files was to produce graphics and
analyses for my basic lectures on biogeography and plant community
ecology. Some of these functions may be useful for other use as
well. The main functions are:

## Distribution maps with proper cartographic projections

I wanted to have maps with standard projections such as UTM for
smaller areas and Lambert Azimuthal Equal Area for wider
areas. Function `projectedmap` can be used to draw a projected map
that completely includes the given range of latitude and longitude,
and `occpmap` displays species occurrences in such maps as extracted
from [Global Biodiversity Information Facility](http://gbif.org) using
the **spocc** package.

## Walter diagrams of climate

Function `walterplot` draws simplified, colourful climatic diagrams
that are suitable for slides.

## Thornthwaite's Potential Evapotranspiration

Function `PEThorn` calculates Potential Evapotranspiration from
monthly mean temperatures. It is more faithful to the original paper
than the implementation in the **SPEI** package, and includes some new
twists.
