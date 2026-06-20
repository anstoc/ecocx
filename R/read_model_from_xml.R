#' Load an EwE model from an XML file exported with the Ecopath software
#'
#' @param xmlfile Path to .xml file.
#' @returns An object of class 'ecocx_model'. Its elements contain core information about functional groups and their parameters, fisheries, mediation and environmental response functions, and other information. Currently, only
#' Ecopath and Ecosim (but not Ecospace) information is loaded. Furthermore, the package currently supports only models with one Ecosim scenario.
#' @examples
#' xmlfile=paste0(ecocx::get_path_to_exampledata(),'anchovy_bay_ecosim_ex.eiixml')
#' m <- ecocx::load_model_from_xml(xmfile)
#' m$ecopath$basic_estimates
#' m$ecosim$shapes$Tempcold$x[1:5]
#' m$ecosim$shapes$Tempcold$y[1:5]
#' @export
load_model_from_xml=function(xmlfile)
{
  xmldoc=read_eiixml(xmlfile)

  m=list()
  class(m)="ecocx_model"
  m$ecopath=list()
  m$ecosim=list()

  #load basic Ecopath inputs
  m$ecopath$basic_estimates=get_basic_estimates(xmldoc)
  m$ecopath$dietmatrix=get_diet_matrix(xmldoc, m$ecopath$basic_estimates)
  m$ecopath$fleets=get_fleets(xmldoc)
  m$ecopath$catches=get_catches(xmldoc,m$ecopath$fleets,m$ecopath$basic_estimates)

  #load basic Ecosim inputs
  m$ecosim$fleetIDs=get_ecosim_fleetIDs(xmldoc)
  m$ecosim$groupIDs=get_ecosim_groupIDs(xmldoc)
  m$ecosim$scenarios=get_ecosim_scenarios(xmldoc)
  m$ecosim$vulnerabilities=get_vulnerability_matrix(xmldoc,m$ecopath$basic_estimates)
  m$ecosim$timeseries=get_time_series(xmldoc,m$ecopath$basic_estimates ,m$ecopath$fleets)
  m$ecosim$fishing_effort=get_fishing_effort(xmldoc)
  m$ecosim$shapes=get_shapes(xmldoc)
  m$ecosim$forcing_functions=get_forcing_functions(xmldoc)
  m$ecosim$foraging_response_table=get_foraging_response_table(xmldoc)
  m$ecosim$mediation_table=get_mediation_table(xmldoc)

  if(nrow(m$ecosim$scenarios)>1) {stop("Reading models with multiple Ecsoim scenarios is currently not supported. Please provide a copy of your model with only one scenario.")}

  m
}


