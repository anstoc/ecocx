xmldoc=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")

#' Load Ecospace scenario information from EIIXML
#' @param xmldoc XML2 document.
#' @returns Data frame with Ecospace scenario names and IDs.
get_ecospace_scenarios=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcospaceScenario")[[1]]
  df=table_to_df(tab)
  data.frame("ScenarioID"=as.numeric(df$ScenarioID),"ScenarioName"=df$ScenarioName,"TotalTime"=as.numeric(df$TotalTime))
}

#TODO
#env. drivers, habitats, MPAs

#deal with Ecosim scenarios
#finalize tutorial
