#' Plot all runs from a computational experiment
#'
#' Plots one line for each run and output (e.g., biomasses), color-coded by output. Inflexible and slow but fine for quick plotting of a few (<100) runs. For faster and more complex plotting, better use custom packages like \code{ggplot2}.
#'
#' @param df_cx Dataframe with experiment outputs. Must have at least 3 columns: \code{run_name}, \code{timestep}, and at least one column with model outputs (e.g., one column per group biomass).
#' @param run_names Optional vector with names of the runs to plot. Must match run names in \code{df_cx}.
#' @param timesteps Optional vector with timesteps to plot. Best to not skip timesteps; e.g., use \code{timesteps=1:100} to plot the first 100 timesteps only.
#' @param outputs  Optional vector with outputs like group or fleet names to plot. Must match output names (column names) in \code{df_cx}.
#' @param alpha Transparency parameter for plotting.
#'
#' @returns NULL
#' @export
plot_all_runs=function(df_cx,run_names=NA,timesteps=NA, outputs=NA, alpha=1)
{
  if(!is.na(run_names)) {df_cx=df_cx[df_cx$run_name %in% run_names,]}
  if(!is.na(timesteps[1])) {df_cx=df_cx[df_cx$timestep %in% timesteps,]}
  col_ix=3:ncol(df_cx)
  if(!is.na(outputs[1])) {col_ix=which(colnames(df_cx) %in% outputs) }

  omit_cols=which(is.na(colSums(df_cx[-c(1,2)])))
  if(length(omit_cols)>0) {
    warning(paste("Omitting one or more output columns because of NaN values. This can occur, e.g., for relative values where the first value is zero."))
    df_cx=df_cx[,-(2+omit_cols)]
    col_ix=3:ncol(df_cx)
  }




  if(ncol(df_cx)<3 ) {stop("Input data frame must have at least 3 columns that don't have NA values.")} else if(
    !identical(colnames(df_cx)[c(1,2)],c("run_name","timestep"))) {stop("First two columns must be named\"run_name\" and \"timestep\".")} else {
        plot(df_cx[,3]~df_cx$timestep,col="white",xlab="Timestep",ylab="Output",
             ylim=c( min(as.matrix(df_cx[,3:ncol(df_cx)])),max(as.matrix(df_cx[,3:ncol(df_cx)])) ))
        cl=rainbow(ncol(df_cx)-2,alpha=alpha)

        for(r in unique(df_cx$run_name))
        {
          for(i in col_ix) {
            lines(df_cx[df_cx$run_name==r,i]~df_cx$timestep[df_cx$run_name==r],col=cl[i-2])
          }
        }
    }
  NULL
}


