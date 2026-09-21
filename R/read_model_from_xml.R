#' Load an EwE model from an XML file exported with the Ecopath software
#'
#' @param xmlfile Path to .xml file.
#' @returns An object of class 'ecocx_model'. Its elements contain core information about functional groups and their parameters, fisheries, mediation and environmental response functions, and other information. Currently, only
#' Ecopath and Ecosim (but not Ecospace) information is loaded. Furthermore, the package currently supports only models with one Ecosim scenario.
#' @examples
#' xmlfile=paste0(ecocx::get_path_to_exampledata(),'anchovy_bay_ecosim_ex.eiixml')
#' m <- ecocx::load_model_from_xml(xmlfile)
#' m$ecopath$basic_estimates
#' m$ecosim$shapes$Tempcold$x[1:5]
#' m$ecosim$shapes$Tempcold$y[1:5]
#' @export
load_model_from_xml=function(xmlfile, ecosim_scenario=NA, ecospace_scenario=NA)
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

  if(nrow(m$ecosim$scenarios)>1) {stop("Reading models with multiple Ecsoim scenarios is currently not supported. Please provide a copy of your model with only one scenario.")}

  m$ecosim$vulnerabilities=get_vulnerability_matrix(xmldoc,m$ecopath$basic_estimates)
  m$ecosim$timeseries=get_time_series(xmldoc,m$ecopath$basic_estimates ,m$ecopath$fleets)
  m$ecosim$fishing_effort=get_fishing_effort(xmldoc)
  m$ecosim$shapes=get_shapes(xmldoc)
  m$ecosim$forcing_functions=get_forcing_functions(xmldoc)
  m$ecosim$foraging_response_table=get_foraging_response_table(xmldoc)
  m$ecosim$mediation_table=get_mediation_table(xmldoc)

  seq_envres=1
  seq_med=1
  unknowns=character(0)
  for(i in 1:length(m$ecosim$shapes)) {
    if(m$ecosim$shapes[[i]]$id %in% m$ecosim$mediation_table$ShapeID) {
        m$ecosim$shapes[[i]]$type="mediation"
        m$ecosim$shapes[[i]]$seq=seq_med
        seq_med=seq_med+1
      } else if(m$ecosim$shapes[[i]]$id %in% m$ecosim$foraging_response_table$ResponseID) {
          m$ecosim$shapes[[i]]$type="envresponse"
          m$ecosim$shapes[[i]]$seq=seq_envres
          seq_envres=seq_envres+1
      } else {
          m$ecosim$shapes[[i]]$type="unknown"
          unknowns=c(unknowns,m$ecosim$shapes[[i]]$name)
      }
  }
  #remove unknown shapes
  if(length(unknowns)>0) {
    warning(paste("Ignoring shapes with unsupported types. They will still be used when executing the model but cannot be changed:",paste(unknowns,collapse=", ")))
    for(shapename in unknowns) {m$ecosim$shapes[[shapename]]=NULL}
  }

  #load Ecospace base maps

  m$ecospace=list()
  m$ecospace$scenarios=get_ecospace_scenarios(xmldoc)
  #handle multiple scenarios
  if(nrow(m$ecospace$scenarios)>0) {
    if(nrow(m$ecospace$scenarios)>1 & is.na(ecospace_scenario)) {
      stop(paste("Parameter 'ecospace_scenario' is required for models with multiple scenarios. Options:",
                 paste(m$ecospace$scenarios$ScenarioName,collapse="; ")))
    } else if(is.na(ecospace_scenario) & nrow(m$ecospace$scenarios)==1) {
      ecospace_scenario=m$ecospace$scenarios$ScenarioName[1]
    } else if(!(ecospace_scenario %in% m$ecospace$scenarios$ScenarioName)) {
      stop(paste("Scenario not found. Options:",
                 paste(m$ecospace$scenarios$ScenarioName,collapse="; ")))}
    #scenario name is valid --> load maps
    m$ecospace$basemap=get_ecospace_basemap(xmldoc,ecospace_scenario)
    m$ecospace$depthmap=get_ecospace_depthmap(xmldoc,ecospace_scenario)
    m$ecospace$envmaps=get_ecospace_envmaps(xmldoc,ecospace_scenario)
    m$ecospace$habmaps=get_ecospace_habmaps(xmldoc,ecospace_scenario)
    m$ecospace$mpamaps=get_ecospace_mpamaps(xmldoc,ecospace_scenario)
  }

  m
}



