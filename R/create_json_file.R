

#' Create and write the Run Console json file from a vector
#'
#' @param i Row of the \code{cx_table} for which to create the file
#' @param cx_table A data frame  with columns 'run_name' (matching the names in the \code{design}), 'model' (path to the EIIXML file), 'folder' (base folder of the run), 'json' (path of the .json file to be created)
#' @param design Data frame describing the experiment, e.g., created with an \code{ecocx::sampler_*} method.
#' @param factor_set The factor set for the experiment.
#'
#' @returns A list of NULLs. Ignore the output.
#' @export
#'
write_json_file=function(i,cx_table,design,factor_set)
{
  file_conn=file(cx_table$json[i])
  on.exit(close(file_conn))
  writeLines(create_json_vector(i,cx_table,design,factor_set), file_conn)
  return()
}

#' Create a json vector from a row in a cx_table
#'
#' @param row Row number in the provided \code{cx_table}.
#' @param cx_table A data frame  with columns 'run_name' (matching the names in the \code{design}), 'model' (path to the EIIXML file), 'folder' (base folder of the run), 'json' (path of the .json file to be created)
#' @param design Data frame describing the experiment, e.g., created with an \code{ecocx::sampler_*} method.
#' @param factor_set The factor set for the experiment.
#'
#' @returns A character vector where each element is a line of the .json file.
#' @export
create_json_vector=function(i,cx_table,design,factor_set)
{
  v_json=create_json_configuration(cx_table$model[i])
  v_json=c(v_json,create_json_ecosim_run(),"}")
  v_json
}


#TODO: switch to relative paths

#' Create a the configuration element of the run console json vector
#'
#' This currently works only for Ecosim models.
#'
#' @param xmlfile Path to the .eiixml file describing the model
#' @param model The model created with \code{create_model_from_xml}. If NA, the modeel is loaded from the provided XML file.
#'
#' @returns Character vector where each element corresponds to a line in the .json file to be generated.
create_json_configuration=function(xmlfile,model=NA)
{
  if(is.na(model)) {model=load_model_from_xml(xmlfile)}

  v_configuration=character(10)
  v_configuration[1]="{"
  v_configuration[2]=" \"Configuration\": {"
  v_configuration[3]=paste0("    \"ModelFile\": \"",xmlfile,"\",")
  v_configuration[4]=paste0("    \"EcosimScenario\": ","1,") #unique(model$ecosim$scenarios$ScenarioID),",")
  v_configuration[5]=paste0("    \"EcosimTimeseries\": 0,")
  v_configuration[6]=paste0("    \"EcospaceScenario\": 0,")
  v_configuration[7]=paste0("    \"SaveWithHeader\": true,")
  v_configuration[8]=paste0("    \"ExtDataConfigFile\": \"\",")
  v_configuration[9]=paste0("    \"RunYears\": ",length(model$ecosim$fishing_effort[[1]]$values)/12)
  v_configuration[10]=paste0("  },")

  v_configuration

}

#' Create the "EcosimRun" element of the run console .json file
#'
#' @returns Character vector where each element corresponds to a line in the .json file to be generated.
#'
create_json_ecosim_run=function()
{
  v_ecosim=character(4)
  v_ecosim[1]="  \"EcosimRun\": {"
  v_ecosim[2]="    \"SaveContentCSV\": [ \"biomass\", \"catch\", \"effort\" ],"
  v_ecosim[3]="  \"SaveAnnual\": true"
  v_ecosim[4]="  }"
  v_ecosim
}

#  "EcosimRun": {
#"SaveContentCSV": [ "biomass", "catch", "effort" ],
#"SaveAnnual": false
#}
