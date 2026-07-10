
folder="C:\\Users\\ANC\\AppData\\Local\\Temp\\RtmpWAeqB9\\jsontest\\R0001_0000" #\\R0001_0000\\ecosim_Scene 1"


#' Obtain time series of biomasses or catch from a single Ecosim run
#'
#' @param folder Path to folder containing the model outputs. If the outputs are not found, the function searches subfolders and uses the first fitting output file found.
#' @param model The model that generated the outputs to be read. Must have been created with \code{create_model_from_xml}.
#' @param groups Character vector with names of the groups for which biomasses should be extracted. If NA (the default), biomasses for all groups are returned.
#'
#' @returns Data frame with a timestep column and one column for each group listed in \code{biomass}.
#' @name get_single_output
#' @export
get_ecosim_output_biomass=function(folder, model, groups=NA)
{
  df=NULL
  files=list.files(folder)
  if(!("biomass_monthly.csv" %in% files)) {
    ix=which(dir.exists(paste0(folder,"/",files)))
    for(sub_folder in files[ix]) {
      if(is.null(df)) {df=get_ecosim_output_biomass(paste0(folder,"/",sub_folder), model, groups)}
    }} else {
      df=read.csv(paste0(folder,"/","biomass_monthly.csv"),skip=14)
      colnames(df)=c("timestep",model$ecopath$basic_estimates$GroupName[model$ecopath$basic_estimates$Sequence])
      if(sum(is.na(groups))==0) {df=df[,colnames(df) %in% c("timestep",groups)]}
    }


    df

}

#' @rdname get_single_output
#' @export
get_ecosim_output_catch=function(folder, model, groups=NA)
{
  df=NULL
  files=list.files(folder)
  if(!("catch_monthly.csv" %in% files)) {
    ix=which(dir.exists(paste0(folder,"/",files)))
    for(sub_folder in files[ix]) {
      if(is.null(df)) {df=get_ecosim_output_catch(paste0(folder,"/",sub_folder), model, groups)}
    }} else {
      df=read.csv(paste0(folder,"/","catch_monthly.csv"),skip=14)
      colnames(df)=c("timestep",model$ecopath$basic_estimates$GroupName[model$ecopath$basic_estimates$Sequence])
      if(sum(is.na(groups))==0) {df=df[,colnames(df) %in% c("timestep",groups)]}
    }
  df
}
