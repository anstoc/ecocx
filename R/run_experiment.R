#' Run a computational experiment
#'
#' Takes a pre-generated description of the required runs and exceutes them. Stores results on disk in one folder per run, which also contains a generated Run Console .json file and the Run Console log. Finally, calculates outputs using the provided functions,
#' which can be default functions from this package or user-generated.
#'
#' @param design Data frame describing the experiment, e.g., created with an \code{ecocx::sampler_*} method.
#' @param xml_model_path Path to XML file describing the model.
#' @param factor_set The factor set for the experiment
#' @param ewe_link Link to the EwE Run Console generated with \code{ecocx::connect_to_ewe}
#' @param out_folder Outputs from each run will be stored in subfolders of this folder.
#' @param output_fun A named list of functions to calculate output indicators. Names will be used in the generated output table.
#' @param parallel Whether to use in case of parallel processing. If \code{true}, you need the \code{future} and \code{future.apply} package installed and call future::plan yourself.
#'
#'
#' @returns A data frame with information about the runs, e.g., where outputs are stored.
#' @export
run_ecosim_experiment=function(design, xml_model_path, factor_set, ewe_link, out_folder,parallel=F)
{
  #set up experiment data, matrix, and folder structure
  folder_names=paste0(out_folder,"/",design$run_name)
  if(!file.exists(out_folder)) {dir.create(out_folder)}
  for(i in 1:length(folder_names)) {if(!file.exists(folder_names[i])) {dir.create(folder_names[i])}}
  cx_table=data.frame("run_name"=design$run_name,"model"=xml_model_path,"folder"=paste0(out_folder,"/",design$run_name),"json"=paste0(paste0(out_folder,"/",design$run_name),"/",design$run_name,".json"))

  #create vulnerability matrix CSVs in base folder
  if(!file.exists(paste0(out_folder,"/data"))) {dir.create(paste0(out_folder,"/data"))}
  for(v_name in names(factor_set$tables$vulnerability)) {write_vulnerability_csv(m,factor_set$tables$vulnerability[[v_name]],
                                                                                   path=paste0(out_folder,"/data/",v_name,".csv")) }

  #replace option for vulnerability with path to CSV
  for(i in 1:nrow(design)) {design$vulnerability[i]=paste0(out_folder,"/data/",design$vulnerability[i],".csv")}

  #create .json files
  dummy=lapply(1:nrow(cx_table),FUN=write_json_file, cx_table=cx_table,design=design,factor_set=factor_set)

  #run model

  if(!parallel) {
    dummy=lapply(1:nrow(cx_table),FUN=execute_run,cx_table=cx_table,ewe_link=ewe_link)
  } else if (requireNamespace("future", quietly = TRUE) & requireNamespace("future.apply", quietly = TRUE)) {
    # future is available, use it
    dummy=future.apply::future_lapply(1:nrow(cx_table),FUN=execute_run,cx_table=cx_table,ewe_link=ewe_link)
  } else {
    stop("Use cores=1 for sequential execution. If cores >1, you need the 'future.apply' and 'future' packages.")
  }

  cx_table

}


#' Execute a single run from a provided table
#'
#' @param i Table row dscribing the run.
#' @param cx_table Table describing all runs.
#' @param ewe_link Link to the EwE Run Console, established with \code{ecocx::connect_to_ewe}.
#'
#' @returns NULL
execute_run=function(i, cx_table, ewe_link)
{
  if(!ewe_link$test_passed) {warning("The provided ewe_link object did not generate the expected outputs when created. Attempting to run the model amyway.")}
  command_str=ewe_link$command_str
  arg_str=paste("-i", cx_table$json[i], "-o",cx_table$folder[i])

  system2(command_str,arg_str)   #run example model

}



