#' Create a set of factors that vary between Ecosim model runs
#'
#' The factor set contain lists of alternative fishing effort, environmental response shapes, and other potential factors that might vary between model runs.
#' First, use this function to generate a factor set with one level per factor,then add alternatives with add_ecosim_factor_level().
#'
#' @param m A model created with \code{load_model_from_xml()}.
#' @param default_name Name that the default value
#'
#' @returns A list of Ecosim and Ecospace factors like fishing effort time series and MPA maps with their default values in the model. Only Ecosim factors that transfer to Ecospace simulations are included.
#' @export
#'
#' @examples
#' xmlfile=paste0(ecocx::get_path_to_exampledata(),'anchovy_bay_ecospace_ex.eiixml')
#' m <- ecocx::load_model_from_xml(xmfile,ecospace_scenario="BayOfAnchovies")
#' fset <- new_ecospace_factor_set(m)
new_ecospace_factor_set=function(m, default_name="default")
{
  #everything that carries over from Ecosim
  factor_set=new_ecosim_factor_set(m)
  factor_set$forcing_functions=NULL

  #static maps
  factor_set$env_maps=list()

  #env. drivers
  for(i in 1:length(m$ecospace$envmaps)) {
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]]=list()
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]=m$ecospace$envmaps[[i]]
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]$factor_value=1
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]$type="envmap"
  }

  #habitats
  for(i in 1:length(m$ecospace$habmaps)) {
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]]=list()
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]=m$ecospace$habmaps[[i]]
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]$factor_value=1
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]$type="habitat"
  }

  #MPAs
  for(i in 1:length(m$ecospace$mpamaps)) {
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]]=list()
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]=m$ecospace$mpamaps[[i]]
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]$factor_value=1
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]$type="mpa"
  }

  class(factor_set)="ecocx_factor_set"
  factor_set
}


#' Obtain the model grid and study region
#'
#' @param m The model.
#'
#' @returns Matrix with the model grid and grid cells inside (value \code{0}) and outside (value \code{NA}) the study region.
#' @export
get_basemap_matrix=function(m)
{
  m$ecospace$basemap$values
}


#' Add a level to a map in an Ecospace factor set.
#'
#' @param factor_set The factor set.
#' @param map_type One of \code{"env_maps"}, \code{"habitats"}, or \code{mpas}
#' @param map_name Name of the map for which the level is added. Must already exist in the factor set.
#' @param level_name Name of the new level.
#' @param map_values Values of the new map.
#' @param factor_value Scalar value of the factor representing the magnitude of difference between levels.
#'
#' @returns A factor set including the new level for the map.
add_level_ecospace_map=function(factor_set,map_type,map_name,level_name,map_values,factor_value=NA)
{
  #check if parameters are consistent with model information
  if(!map_type %in% c("env_maps","habitats","mpas")) stop("Parameter map_type must be one of 'env_maps', 'habitat maps', 'mpas'.")
  if(!(map_name %in% names(factor_set[[map_type]]))) {stop(paste("Map",map_name,"not found in factor_set."))}
  if(level_name %in% names(factor_set[[map_type]][[map_name]])) {stop("A factor level with this name already exists. To avoid accidental overwriting, remove it with remove_level_ecospace_map(), then try again.")}
  if(!identical(dim(map_values),dim(factor_set[[map_type]][[map_name]][[1]]$values))) {stop("The x and y values must be a numeric vector with length 1200 (an Ecosim legacy).")}

  #create the new shape level
  new_level=factor_set[[map_type]][[map_name]][[1]]
  new_level$values=map_values
  new_level$factor_value=factor_value

  factor_set[[map_type]][[map_name]][[level_name]]=new_level

  return(factor_set)
}

#' Add a level to an environmental map in an Ecospace factor set.
#'
#' @param factor_set The factor set.
#' @param map_name Name of the map for which the level is added. Must already exist in the factor set.
#' @param level_name Name of the new level.
#' @param map_values Values of the new map.
#' @param factor_value Scalar value of the factor representing the magnitude of difference between levels.
#'
#' @returns A factor set including the new level for the map.
add_level_ecospace_envmap=function(factor_set,map_name,level_name,map_values,factor_value=NA)
{
  add_level_ecospace_map(factor_set,"env_maps",map_name,level_name,map_values,factor_value)
}

#' Add a level to a habitat map in an Ecospace factor set.
#'
#' @param factor_set The factor set.
#' @param map_name Name of the map for which the level is added. Must already exist in the factor set.
#' @param level_name Name of the new level.
#' @param map_values Values of the new map.
#' @param factor_value Scalar value of the factor representing the magnitude of difference between levels.
#'
#' @returns A factor set including the new level for the map.
add_level_ecospace_habmap=function(factor_set,map_name,level_name,map_values,factor_value=NA)
{
  add_level_ecospace_map(factor_set,"habitats",map_name,level_name,map_values,factor_value)
}

#' Add a level to a habitat map in an Ecospace factor set.
#'
#' @param factor_set The factor set.
#' @param map_name Name of the map for which the level is added. Must already exist in the factor set.
#' @param level_name Name of the new level.
#' @param map_values Values of the new map.
#' @param factor_value Scalar value of the factor representing the magnitude of difference between levels.
#'
#' @returns A factor set including the new level for the map.
add_level_ecospace_mpamap=function(factor_set,map_name,level_name,map_values,factor_value=NA)
{
  add_level_ecospace_map(factor_set,"mpas",map_name,level_name,map_values,factor_value)
}



