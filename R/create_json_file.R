

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
  v=create_json_vector(i,cx_table,design,factor_set)
  v=v[nchar(v)>0]
  writeLines(v, file_conn)
  return()
}

#' Create a json vector from a row in a cx_table
#'
#' @param x Row number in the provided \code{cx_table}.
#' @param cx_table A data frame  with columns 'run_name' (matching the names in the \code{design}), 'model' (path to the EIIXML file), 'folder' (base folder of the run), 'json' (path of the .json file to be created)
#' @param design Data frame describing the experiment, e.g., created with an \code{ecocx::sampler_*} method.
#' @param factor_set The factor set for the experiment.
#'
#' @returns A character vector where each element is a line of the .json file.
#' @export
create_json_vector=function(x,cx_table,design,factor_set)
{
  v_json=create_json_configuration(cx_table$model[x])
  v_json=c(v_json,create_json_ecosim_run(x,cx_table,design,factor_set),"}")
  v_json
}


#TODO: Per default uses absolute paths. Switch to relative paths.

#' Create a the configuration element of the run console json vector
#'
#' This currently works only for Ecosim models.
#'
#' @param xmlfile Path to the .eiixml file describing the model
#' @param model The model created with \code{create_model_from_xml}. If NA, the model is loaded from the provided XML file.
#'
#' @returns Character vector where each element corresponds to a line in the .json file to be generated.
create_json_configuration=function(xmlfile,model=NA)
{
  if(is.na(model)) {model=load_model_from_xml(xmlfile)}

  v_configuration=character(10)
  v_configuration[1]="{"
  v_configuration[2]=" \"Configuration\": {"
  v_configuration[3]=paste0("    \"ModelFile\": \"",xmlfile,"\",")
  v_configuration[4]=paste0("    \"EcosimScenario\": 1,") #unique(model$ecosim$scenarios$ScenarioID),",")
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
create_json_ecosim_run=function(x,cx_table,design,factor_set)
{
  v_ecosim=character(3)
  v_ecosim[1]="  \"EcosimRun\": {"
  v_ecosim[2]="    \"SaveContentCSV\": [ \"biomass\", \"catch\", \"effort\" ],"
  v_ecosim[3]="  \"SaveAnnual\": true,"
  v_changes=create_json_changes_ecosim(x,design,factor_set)
  c(v_ecosim,v_changes,"  }")

}


#' Create vector with .json file lines for changes.
#'
#'Currently only works for fishing effort.
#'
#' @param x Row number of the design
#' @param design Data frame describing the experiment, e.g., created with an \code{ecocx::sampler_*} method.
#' @param factor_set The factor set for the experiment
#'
#' @returns Character vector where each element corresponds to a line "Changes" section of the .json file to be generated.
#' @export

create_json_changes_ecosim=function(x,design,factor_set)
{
  v_changes=character(ncol(design)+3)
  v_changes[1]='  "Changes": ['
  v_changes[2]='  {'
  v_changes[3]='    "Date": "start",'
  v_changes[4]='    "Modifications": {'
  #add effort modification
  for(i in 1:length(factor_set$fishing_effort))
  {
    fleet=names(factor_set$fishing_effort)[[i]]
    choice=design[x,][[fleet]]
    ecosim_id=i #TODO reference by name once implemented
    values=factor_set$fishing_effort[[i]][[choice]]$values
    v_changes[4+i]=paste0('      "ecosim.effort[',ecosim_id,'].set": [ ',paste(as.character(values),collapse=", "),' ]')
    if(i<length(factor_set$fishing_effort)) {v_changes[4+i]=paste0(v_changes[4+i],",")}
  }
  if(length(factor_set$forcing_functions)>0) {v_changes[4+length(factor_set$fishing_effort)]=paste0(v_changes[4+length(factor_set$fishing_effort)],",")}
  #add forcing function (driver) modification
  for(i in 1:length(factor_set$forcing_functions))
  {
    func=names(factor_set$forcing)[[i]]
    choice=design[x,][[func]]
    ecosim_id=i #TODO reference by name once implemented
    values=factor_set$forcing_functions[[i]][[choice]]$values
    v_changes[4+length(factor_set$fishing_effort)+i]=paste0('      "ecosim.forcingfunction[',ecosim_id,'].set": [ ',paste(as.character(values),collapse=", "),' ]')
    if(i<length(factor_set$forcing_functions)) {v_changes[4+length(factor_set$fishing_effort)+i]=paste0(v_changes[4+length(factor_set$fishing_effort)+i],",")}
  }
  ix=4+length(factor_set$fishing_effort)+length(factor_set$forcing_functions)
  v_changes[ix]=paste0(v_changes[ix],",")
  v_changes[ix+1]=paste0('      "ecosim.vulnerabilities.load": "',normalizePath(design$vulnerability[x],winslash='/'),'"')
  v_changes[ix+2]='      }'
  v_changes[ix+3]='    }'
  v_changes[ix+4]='  ]'
  v_changes
}


