#' Obtain time series of biomasses or catch from a single Ecosim run
#'
#' @param folder Path to folder containing the model outputs. If the outputs are not found, the function searches subfolders and uses the first fitting output file found.
#' @param model The model that generated the outputs to be read. Must have been created with \code{create_model_from_xml}.
#' @param groups Character vector with names of the groups for which biomasses should be extracted. If NA (the default), biomasses for all groups are returned.
#'
#' @returns Data frame with a timestep column and one column for each group listed in \code{biomass}.
#' @name get_single_output
#' @export
get_ecosim_run_biomass=function(folder, model, groups=NA)
{
  df=NULL
  files=list.files(folder)
  if(!("biomass_monthly.csv" %in% files)) {
    ix=which(dir.exists(paste0(folder,"/",files)))
    for(sub_folder in files[ix]) {
      if(is.null(df)) {df=get_ecosim_run_biomass(paste0(folder,"/",sub_folder), model, groups)}
    }} else {
      df=read.csv(paste0(folder,"/","biomass_monthly.csv"),skip=14)
      colnames(df)=c("timestep",model$ecopath$basic_estimates$GroupName[model$ecopath$basic_estimates$Sequence])
      if(sum(is.na(groups))==0) {df=df[,colnames(df) %in% c("timestep",groups)]}
    }


    df

}

#' @rdname get_single_output
#' @export
get_ecosim_run_catch=function(folder, model, groups=NA)
{
  df=NULL
  files=list.files(folder)
  if(!("catch_monthly.csv" %in% files)) {
    ix=which(dir.exists(paste0(folder,"/",files)))
    for(sub_folder in files[ix]) {
      if(is.null(df)) {df=get_ecosim_run_catch(paste0(folder,"/",sub_folder), model, groups)}
    }} else {
      df=read.csv(paste0(folder,"/","catch_monthly.csv"),skip=14)
      colnames(df)=c("timestep",model$ecopath$basic_estimates$GroupName[model$ecopath$basic_estimates$Sequence])
      if(sum(is.na(groups))==0) {df=df[,colnames(df) %in% c("timestep",groups)]}
    }
  df
}

#' Obtain time series of biomasses or catch from all Ecosim runs in a computational experiment.
#'
#' @param cx_table Table describing all runs, as returned by \code{run_ecosim_experiment}.
#' @param model The model that generated the outputs to be read. Must have been created with \code{create_model_from_xml}.
#' @param groups Character vector with names of the groups for which biomasses should be extracted. If NA (the default), biomasses for all groups are returned.
#' @returns Data frame with run name, time step, and biomasses of the groups.
#' @name get_ecosim_cx_
#' @export
get_ecosim_cx_biomass=function(cx_table,model,groups=NA)
{
  df_cx=NULL
  for(i in 1:nrow(cx_table)) {
    df_run=get_ecosim_run_biomass(cx_table$folder[i],model,groups)  #get biomasses for run
    if(is.null(df_cx)) {   #create data frame for all runs if not exisiting yet
      df_cx=as.data.frame(matrix(NA,nrow=nrow(cx_table)*nrow(df_run),ncol=1+ncol(df_run)))
      colnames(df_cx)=c("run_name",colnames(df_run))
      df_cx[,1]=rep(cx_table$run_name,each=nrow(df_run))
    }
    df_cx[df_cx$run_name==cx_table$run_name[i],2:ncol(df_cx)]=df_run  #copy run data into cx data frame
  }
  df_cx
}

#' @rdname get_ecosim_cx_
#' @export
get_ecosim_cx_catch=function(cx_table,model,groups=NA)
{
  df_cx=NULL
  for(i in 1:nrow(cx_table)) {
    df_run=get_ecosim_run_catch(cx_table$folder[i],model,groups)  #get biomasses for run
    if(is.null(df_cx)) {   #create data frame for all runs if not exisiting yet
      df_cx=as.data.frame(matrix(NA,nrow=nrow(cx_table)*nrow(df_run),ncol=1+ncol(df_run)))
      colnames(df_cx)=c("run_name",colnames(df_run))
      df_cx[,1]=rep(cx_table$run_name,each=nrow(df_run))
    }
    df_cx[df_cx$run_name==cx_table$run_name[i],2:ncol(df_cx)]=df_run  #copy run data into cx data frame
  }
  df_cx
}
