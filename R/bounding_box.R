
#' Minimal wrapper over bbox_from_vector that takes a character representing a city/osm place and returns the bbox as a sf polygon
#'
#' Need to slipt the lng and lat for osm
#'
#' e.g. the lat,lng are when compared to sf::st_boox
#'
#' osmdata::opq(bbox = 'Atlanta')$bbox
#' vs
#' sf::st_bbox(osmdata::osmdata_sf(osmdata::opq(bbox = 'Atlanta')))
#'
#' @param osm_place
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library (mapview)
#' shp_bbox_disney_osm <- bbox_from_osm('Disney')
#' mapview(shp_bbox_disney_osm)
#' }
bbox_from_osm <- function(osm_place){


  opq <- osmdata::opq(bbox = osm_place)

  bbox <- stringr::str_split(opq$bbox, pattern = ',' )[[1]] %>% as.double()
  names(bbox) <- c( "ymin", "xmin" , "ymax", "xmax" )

  bbox_from_vector(bbox,
                   crs = 4326)
}



#' Create an sf bounding box from a vector with (xmin,ymin,ymax,ymax)
#'
#' This works automatically with raster extents or sf::bbox
#'
#' @param v vector with names (xmin,ymin,ymax,ymax)
#' @param crs
#' @param x_name_min character
#' @param y_name_min character
#' @param x_name_max character
#' @param y_name_max character
#'
#' @return sf object representing bbox
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' shp_trees <-  st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
#' rasterKDECentroids <- st_kde(shp_trees , cellsize = 0.001, bandwith =c(.001, .001 ) )
#' bbox_from_vector(extent(rasterKDECentroids) , crs=4326)
#'
#' }
bbox_from_vector <- function(v,
                             crs,
                             x_name_min="xmin" ,
                             y_name_min="ymin",
                             x_name_max="xmax" ,
                             y_name_max="ymax"){

  require(magrittr)
  require(sf)
  require(sp)


  #IF already produced by sf::bbox will work as identity. If raster extent , then will convert to bbox
  v <- st_bbox(v)

  #Check names. Order can differ, but the xmin, xmax,... should all be there (and nothing else)
  names_extent <- c(x_name_min,y_name_min,x_name_max,y_name_max)
  assertthat::assert_that(all( names_extent %in% names(v) ) & all( names(v) %in%  names_extent),
                          msg='v object must contain names of the coordinates to use for bbox')
  assertthat::assert_that(!is.na(crs))

  #Raster extent
  rast_extent <- raster::extent( v[[x_name_min]],v[[x_name_max]],
                                 v[[y_name_min]],v[[y_name_max]] )

  #Convert to sf
  shp_bbox <- as(rast_extent, "SpatialPolygons") %>%
    st_as_sf() %>%
    st_set_crs(crs)

  #Final check
  bbox_validation(shp_bbox)

  return(shp_bbox)

}
