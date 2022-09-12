

# Author: @cgauvi
# March 28, 2022
# Based on [this repo](https://github.com/mem48/trafficcounts/blob/master/cpprouting.R)



#' Creates a graph with weights given by the mode chosen
#'
#' @param osm_results_streets
#' @param osm_modes
#'
#' @import dodgr
#'
#' @return osm list of sf LINESTRING
#'
#' @examples
st_get_linestring_osm_by_mode <- function(osm_results_streets,
                                          osm_modes= c("motorcar","foot","bicycle")){

  require(sf)
  require(purrr)
  require(dplyr)

  osm <- osm_results_streets$osm_lines
  assertthat::assert_that(!is.null(osm) && nrow(osm)>0,msg = 'Fatal error! no results from osm oeverpass query! make sure query raidius is sufficiently large')
  polys <- osm_results_streets$osm_polygons # needed for roundabouts

  cols_select <- c("osm_id","name","ref","highway",
                   "junction","maxspeed","geometry")


  osm <- osm %>% dplyr::select(any_of(cols_select))

  if(!all(is.na(polys)) & !is.null(polys) & nrow(polys)>0){
    polys <- st_cast(polys, "LINESTRING")
    polys <- polys %>% dplyr::select(any_of(cols_select))
    osm <- rbind(osm, polys)
  }

  assertthat::assert_that( nrow(osm)>0,msg=glue::glue('Fatal error there are no results for that osm query'))
  rm(osm_results_streets, polys)

  #Nested function to attach the max speed by mode
  get_max_speed <- function(mode="motorcar",left_name_join='highway'){

    weights <- dodgr::weighting_profiles$weighting_profiles %>%
      filter(name==mode) %>%
      rename_at('way', ~left_name_join) %>%
      rename(mode=name)

    if('maxspeed' %in% colnames(osm)) osm %<>% dplyr::select(-maxspeed)
    osm %<>% left_join(weights , by= left_name_join )

    assertthat::assert_that(sum(is.na(osm$max_speed))!=nrow(osm),msg=glue::glue('Fatal error for mode {mode} -- all speeds are NA'))

    return(osm)
  }

  osm_by_mode <- map(osm_modes, get_max_speed) %>% set_names(osm_modes)


  return(osm_by_mode)
}




#' Creates a weighted graph whose weight is given by the time required to travel along an edge given a certain mode
#'
#' @param sfdf
#'
#' @import cppRouting stplanr
#'
#' @return
#'
#' @examples
sf_to_graph <- function(sfdf){

  sfdf <- st_transform(sfdf, 4326)
  coords <- as.data.frame(sf::st_coordinates(sfdf))
  coords <- round(coords, 13)
  coords$ID <- stplanr::od_id_szudzik(coords$X, coords$Y, ordermatters = TRUE)

  coords$lengths <- geodist::geodist(coords[,c("X","Y")], sequential = TRUE, pad = TRUE)*10**-3 #in km
  coords$speeds <- sfdf$max_speed[coords$L1] #in km/h - L1 is the index of the original linestring that contains the POINT

  # Convert from hours to minute
  coords$time <- (coords$lengths / coords$speeds) * 60

  roads <- coords[seq(1, nrow(coords)-1),c("ID","L1")]
  roads2 <- coords[seq(2, nrow(coords)),c("ID","time","L1")]
  names(roads) <- c("from","L1a")
  names(roads2) <- c("to","weight","L1b")
  roads <- cbind(roads, roads2)
  rm(roads2)
  roads <- roads[roads$L1a == roads$L1b,]
  roads <- roads[,c("from","to","weight")]
  coords <- coords[,c("ID","X","Y")]
  coords <- coords[!duplicated(coords$ID),]

  assertthat::are_equal(coords %>% is.na() %>% sum, 0)

  #Remove edges with no time to travel -- removed things like time to walk on a motorway or driving on a path
  is_na_idx <- map_lgl( 1:nrow(roads), ~any(is.na(roads[.x,])) )

  gg <- cppRouting::makegraph(roads[!is_na_idx, ], coords = coords, directed = FALSE)

  return(gg)

}



#' Compute isochrones based on a cppRouting graph (whose edges have weight given by time in minutes)
#'
#' The limits depends on the units used for the 'dist' column - for this function requires that the dist be in seconds
#'
#' @param graph
#' @param from
#' @param lim
#'
#' @import concaveman
#'
#' @return
#'
#' @examples
st_make_isochrone_concave_hull <- function(graph,
                           from = "1",
                           limit_minutes = c(5,10,15)){

  require(magrittr)
  require(dplyr)

  iso <- cppRouting::get_isochrone(Graph = graph, from = from,lim = limit_minutes)
  poly<-lapply(iso[[1]],function(x){
    x <- data.frame(noeuds=x,stringsAsFactors = F)
    x <- dplyr::left_join(x,graph$coords,by=c("noeuds"="ID"))
    x <- sf::st_as_sf(x,coords=c("X","Y"),crs=4326, dim = "XY")


    #Edge cases
    if(nrow(x)==3){
      print(glue::glue('Weird results! there are only 3 points! Trying a triangle'))
      x <- sf::st_make_valid(rbind(x,x[1,]))
    }else if(nrow(x)<=2){
      print(glue::glue('Error getting the concave hull! less than 3 points can be reached : check the query radius and the network parameters - use only "highway" preferably '))
      #See https://github.com/r-spatial/sf/issues/354 for empty polyon
      return( st_geometrycollection()  )
    }

    y <- tryCatch({

      y <- concaveman::concaveman(x, length_threshold = 0.01)
      y <- y[[1]][[1]]
      y <- sf::st_make_valid(y)
      y <- st_cast(y, "MULTIPOLYGON")
    },error=function(e){

      print(glue::glue('Fatal error - {e} - trying convex hull instead'))
      y <- sf::st_make_valid(x)
      y <- sf::st_convex_hull(y)
      y <- y %>% mutate(id=1) %>% group_by(id) %>% summarise() %>% st_cast('POLYGON') %>% pull(geometry) %>%  `[[`(1) #dissolve and get the sfc
    })


    return(y)
  })

  poly <- sf::st_as_sfc(poly, crs = 4326)
  poly <- sf::st_as_sf(data.frame(limit_minutes = limit_minutes, geometry = poly))
  poly <- poly[seq(nrow(poly), 1), ]

  return(poly)

}



#' Check to see if a dataframe of points lies within the convex hull of another dataframe of points
#'
#' Geographic 4326 coordinates with X and Y columns for lon and lat for both dataframes
#'
#' @param coords_to_check
#' @param coords_area
#'
#' @return
#' @export
#'
#' @examples
is_within_cvx_hull <- function(coords_to_check,
                               coords_area  ){


  require(sf)
  require(dplyr)

  shp_to_check<- st_as_sf( coords_to_check, coords = c('X','Y'), crs=4326)
  shp_area <- st_as_sf( coords_area,   coords = c('X','Y') , crs=4326)

  #Get the convex hull
  shp_convex_area <-   st_convex_hull(  st_union(shp_area))



  is_within <- all(
    map_lgl( st_within(shp_to_check, shp_convex_area)  ,
             ~!is_empty(.x))
    ) #this means we can have multiple points to check

  return(is_within)

}


#' Wrapper over st_make_isochrone_concave_hull that creates an isochrone based on the weighted graph computed and a starting point
#'
#' @param graph
#' @param df_start_point
#'
#' @return
#'
#' @examples
st_make_isochrone_new_point <- function(graph,
                                     df_start_point,
                                     limit_minutes = c(5,10,15),
                                     walk_speed_kmh=5){

  assertthat::assert_that(nrow(graph$coords)>0,msg='Fatal error! no coordinates in graph')

  #Get node in graph which is closest to the desired point
  get_dist <- function(x,y,x_ref=df_start_point$X,y_ref=df_start_point$Y){
    geodist::geodist( data.frame( lon=x, lat= y),  data.frame(lon=x_ref,lat=y_ref), measure='vincenty') [[1]]
  }

 dist_km <- map_dbl( 1:nrow(graph$coords),
                      ~get_dist( graph$coords$X[[.x]], graph$coords$Y[[.x]] )*10**-3
  )

  id_ref <- graph$coords$ID[[which.min(dist_km)]][[1]] #make sure we only select exactly 1
  assertthat::assert_that( length(id_ref) != 0, msg = 'Fatal error! coul not find a node in the graph')

  #Add in the walk time to the network
  # dodgr::weighting_profiles has 5 km/hour
  walk_speed_kmpermin <- walk_speed_kmh/60
  walk_time_min_reach_network <- (dist_km[which.min(dist_km) ]/walk_speed_kmpermin)

  print(glue::glue('Watch out! It takes at least {round(walk_time_min_reach_network,2)} minutes to walk (fly..) from point ({df_start_point$X},{y_ref=df_start_point$Y}) to the nearest street'))
  limit_minutes_adjusted <- limit_minutes - walk_time_min_reach_network
  limit_minutes_adjusted <- limit_minutes_adjusted[limit_minutes_adjusted >0 ]

  # Make sure we can actually reach the initial time limits
  if(walk_time_min_reach_network>=min(limit_minutes)){
    str_limit_adj <- paste0(round(limit_minutes_adjusted,2), collapse = ',')
    print(glue::glue('Not all time limits can be reached since walking to the network is already very long. Considering only the following times: {str_limit_adj}'))
  }



  # Now the actual isochrone
  isochone <- st_make_isochrone_concave_hull(graph = graph,
                             from = id_ref,
                             limit_minutes = limit_minutes_adjusted #reduce the time we can take since we need to factor approx walk time to network
  )



  #Check if the centroid is within the cvx hull of the isochrone coordinates
  is_within <- is_within_cvx_hull(df_start_point, isochone %>% st_coordinates %>% as.data.frame())
  if(!is_within){
    print(glue::glue('Warning! the centroid does not lie within the convex hull of nodes that can be reached in the network - check the network and the centroid'))
  }


  isochone$limit_minutes_adjusted <-  isochone$limit_minutes
  isochone$limit_minutes     <-  isochone$limit_minutes + walk_time_min_reach_network
  isochone$walk_time_min_reach_network <- walk_time_min_reach_network

  return(isochone)
}



#' Man function to call to get
#'
#' @param shp_point
#' @param buffer_km
#' @param list_osm_params
#' @param limit_minutes
#' @param list_modes
#'
#' @return shp_iso_all_modes: sf POLYGON
#' @export
#'
#' @examples
st_make_isochrone  <- function(shp_point,
                                buffer_km = 5,
                                limit_minutes = c(5,10,15),
                                list_modes= c("motorcar","foot","bicycle"),
                                list_osm_params= list( list(key='highway' ) )){

  #Get the centroid
  df_centroid <- shp_point %>% sf::st_coordinates() %>% as.data.frame

  #OSM overpass query
  osm_results_streets <- osm_buffer_query_helper( shp_point,
                                                  buffer_km = buffer_km,
                                                  list_osm_params=list_osm_params)


  #Create the sf linestring objects with the appropriate weights
  shp_osm_line_by_mode <- st_get_linestring_osm_by_mode(osm_results_streets,list_modes)
  print('...OSM successfully queried...')

  #Create the graphs with the appropriate modes from the corresponding sf objects
  list_graph_modes <-  map(list_modes ,
                            ~sf_to_graph(shp_osm_line_by_mode[[.x]]) ) %>%
    set_names(list_modes)
  print('...Network successfully converted to graph...')

  #Loop over all modes and call st_make_isochrone_new_point
  shp_iso_all_modes_list <- map( seq_along(list_graph_modes),
                                 ~st_make_isochrone_new_point(graph = list_graph_modes[[.x]],
                                                          df_start_point = df_centroid,
                                                          limit_minutes = limit_minutes) %>%
                                  mutate(mode=list_modes[[.x]]))


  shp_iso_all_modes <- do.call(rbind,shp_iso_all_modes_list ) %>%
    st_make_valid()
  print('...Isochrones successfully computed for all modes...')

  if(!all( map_lgl(shp_iso_all_modes$geometry, ~length(.x)>0 ) ) ){
    print('Warning error! not all geometries are valid! could be some geo porcessing error when computing convex/concave hull')
  }

  return(shp_iso_all_modes)

}


#' Add feature, value pairs recursively to an osm overpass query
#'
#' @param opq_query
#' @param list_osm_params
#'
#' @return
#' @export
#'
#' @examples
create_opq_query_add_features <- function(opq_query,
                                          list_osm_params= list( list(key='highway' ) )){


  assertthat::assert_that( length(list_osm_params) >0 ,msg='Fatal error! no parameters passed')

  #Wrapper to set the key and values specifically
  wrapped_add_osm_feature = function(key,value =NULL, ...) {
    return(function(...) osmdata::add_osm_feature(key=key,value=value,...) )
  }

  wrapped_add_osm_feature_with_list <- do.call( wrapped_add_osm_feature, list_osm_params[[1]]  )
  opq_query %<>% wrapped_add_osm_feature_with_list

  if(length(list_osm_params)>=2){
    for(k in 2:length(list_osm_params)){
      wrapped_add_osm_feature_with_list_new <- do.call( wrapped_add_osm_feature, list_osm_params[[k]]  )
      opq_query %<>% wrapped_add_osm_feature_with_list_new
    }
  }



  return(opq_query)

}


#' Get osm objects within a fixed radius of a given point
#'
#' This is useful namely to get the street network around a point
#'
#' @param shp_point
#' @param buffer_km
#' @param list_osm_params
#' @import osmdata
#'
#' @return osm_results_streets osm results: list with linestrings, points and polygons + aux data
#' @export
#'
#' @examples
osm_buffer_query_helper <- function(shp_point,
                                     buffer_km = 5,
                                     list_osm_params= list( list(key='highway' ) )
                                    ){

  shp_buffer <- shp_point  %>%
    sf::st_transform(crs=3857) %>%
    sf::st_buffer(buffer_km*1000) %>%
    sf::st_transform(crs=4326)

  opq_query <- osmdata::opq(bbox = sf::st_bbox(shp_buffer) )


  osm_results_streets <- opq_query %>%
    create_opq_query_add_features( list_osm_params=list_osm_params) %>%
    osmdata::osmdata_sf ()

  return(osm_results_streets)
}






#' Simple helper to get the city centroid from a string (in 4326 geographic coordiantes)
#'
#' @param city_str
#'
#' @return
#' @export
#'
#' @examples st_osm_city_centroid_from_string('Atlanta')
st_osm_city_centroid_from_string <- function(city_str){

  osm <-  osmdata::opq(bbox = city_str)

  shp_bbox <- SfSpHelpers::bbox_from_points(list_lng = as.double(str_split(osm$bbox, ',')[[1]][c(2,4) ]),
                                            list_lat = as.double(str_split(osm$bbox, ',')[[1]][c(1,3)]) )

  assertthat::assert_that( units::drop_units(st_area(shp_bbox %>% st_transform(crs=3857))) >0, msg='Fatal error! bbox has 0 area')


  return(shp_bbox)
}




