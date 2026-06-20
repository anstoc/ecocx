#TODO: Don't call them factors and factor levels due to confusion with R names?

#' Create a set of factors that vary between Ecosim model runs
#'
#' The factor set contain lists of alternative fishing effort, environmental response shapes, and other potential factors that might vary between model runs.
#' First, use this function to generate a factor set with one level per factor,then add alternatives with add_ecosim_factor_level().
#'
#' @param m An Ecosim model object created with load_model_from_xml().
#' @param default_name Name that the default value
#'
#' @returns A list of factors like fishing effort and environmental time series with their default values in the model.
#' @export
#'
#' @examples
#' xmlfile=paste0(ecocx::get_path_to_exampledata(),'anchovy_bay_ecosim_ex.eiixml')
#' m <- ecocx::load_model_from_xml(xmfile)
#' flist <- new_ecosim_factor_set(m)
new_ecosim_factor_set=function(m, default_name="default")
{
  factor_set=list()
  class(factor_set)="ecocx_factor_set"

  #foraging response table
  factor_set$foraging_resp_tables=list()
  factor_set$foraging_resp_tables[[default_name]]=m$ecosim$foraging_response_table

  #mediation table
  factor_set$mediation_tables=list()
  factor_set$mediation_tables[[default_name]]=m$ecosim$mediation_table

  #vulnerabilities
  factor_set$vulnerability_tables=list()
  factor_set$vulnerability_tables[[default_name]]=m$ecosim$vulnerabilities

  #vulnerabilities
  factor_set$vulnerability_tables=list()
  factor_set$vulnerability_tables[[default_name]]=m$ecosim$vulnerabilities

  #fishing effort
  factor_set$fishing_effort=list()
  for(i in 1:length(m$ecosim$fishing_effort)) {
    factor_set$fishing_effort[[names(m$ecosim$fishing_effort)[i]]]=list()
    factor_set$fishing_effort[[names(m$ecosim$fishing_effort)[i]]][[default_name]]=m$ecosim$fishing_effort[[i]]
  }

  #forcing functions
  factor_set$forcing_functions=list()
  for(i in 1:length(m$ecosim$forcing_functions)) {
    factor_set$forcing_functions[[names(m$ecosim$forcing_functions)[i]]]=list()
    factor_set$forcing_functions[[names(m$ecosim$forcing_functions)[i]]][[default_name]]=m$ecosim$forcing_functions[[i]]
  }

  #shapes
  factor_set$shapes=list()
  for(i in 1:length(m$ecosim$shapes)) {
    factor_set$shapes[[names(m$ecosim$shapes)[i]]]=list()
    factor_set$shapes[[names(m$ecosim$shapes)[i]]][[default_name]]=m$ecosim$shapes[[i]]
  }

  factor_set

}


