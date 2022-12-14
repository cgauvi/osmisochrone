% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bounding_box.R
\name{bbox_from_osm}
\alias{bbox_from_osm}
\title{Minimal wrapper over bbox_from_vector that takes a character representing a city/osm place and returns the bbox as a sf polygon}
\usage{
bbox_from_osm(osm_place)
}
\arguments{
\item{osm_place}{}
}
\description{
Need to slipt the lng and lat for osm
}
\details{
e.g. the lat,lng are when compared to sf::st_boox

osmdata::opq(bbox = 'Atlanta')$bbox
vs
sf::st_bbox(osmdata::osmdata_sf(osmdata::opq(bbox = 'Atlanta')))
}
\examples{
\dontrun{
library (mapview)
shp_bbox_disney_osm <- bbox_from_osm('Disney')
mapview(shp_bbox_disney_osm)
}
}
