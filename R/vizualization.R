

#' Obtain a vector with hex color codes for Paul Tol's palettes.
#'
#' @param pal Palette to return. One of "bright", "vibrant", "high" (contrast), "muted".
#' @param black Should black be added as an additional color?
#'
#' @returns A vector of color hex codes.
#' "export
tol_colors=function(pal="muted",black=F)
{
  v=""
  if(pal=="bright") {
    v=c("#EE7733","#0077BB","#33BBEE","#EE3377","#CC3311","#009988","#BBBBBB")
  } else if(pal=="vibrant") {
    v=c("#4477AA","#EE6677","#228833","#CCBB44","#66CCEE","#AA3377","#BBBBBB")
  } else if(pal=="high") {
    v=c("#004488","#DDAA33","#BB5566")
  } else if(pal=="muted") {
    v=c("#CC6677","#332288","#DDCC77","#117733","#88ccee","#882255","#44aa99","#999933","#AA4499","#DDDDDD")
  }
  if(black) {v=c("#000000",v)}
  v
}


#' Plot all levels of a factor
#'
#' @param factor_set The factor set.
#' @param name Name of the factor: Must refer to a forcing function/environmental driver time series, mediation or environmental response shape, or fishing effort time series.
#'
#' @returns A base R plot.
#' @export
plot_ecosim_factor_levels=function(factor_set,name)
{
    fac=get_factor_by_name(factor_set,name)
    #construct list with values for plotting
    min_y=99999999
    max_y=0
    l=list()
    for(i in 1:length(fac))
    {
      x=NULL
      y=NULL
      if(class(fac[[i]])=="EcosimShape")  #Shape
      {
        x=fac[[i]]$x
        y=fac[[i]]$y
        if(max(y)>max_y) {max_y=max(y)}
        if(min(y)<min_y) {min_y=min(y)}
      } else if("values" %in% names(fac[[i]])) {#Time series: forcing or anomaly
        x=1:length(fac[[i]]$values)
        y=fac[[i]]$values
        if(max(y)>max_y) {max_y=max(y)}
        if(min(y)<min_y) {min_y=min(y)}
      } else {
        warning("Factor levels cannot be plotted.")
        return()
      }
      l[[i]]=data.frame("x"=x,"y"=y)
    }
    #make plot
    cols=tol_colors("muted")
    p=plot(l[[1]]$y~l[[1]]$x, ylim=c(min_y,max_y),
         xlab="Timestep",ylab="value",main=name,
         type="l",col=cols[1])
    if(length(l)>1)
    {
      for(i in 2:length(l))
      {
        col_ix=(i-1)%%length(cols)+1
        lines(l[[i]]$y~l[[i]]$x,col=cols[col_ix])
      }
    }
    p
}

#' Plot all runs from a computational experiment
#'
#' Plots one line for each run and output (e.g., biomasses), color-coded by output. Inflexible and slow but fine for quick plotting of a few (<100) runs. For faster and more complex plotting, better use custom packages like \code{ggplot2}.
#'
#' @param df_cx Dataframe with experiment outputs. Must have at least 3 columns: \code{run_name}, \code{timestep}, and at least one column with model outputs (e.g., one column per group biomass).
#' @param run_names Optional vector with names of the runs to plot. Must match run names in \code{df_cx}.
#' @param timesteps Optional vector with timesteps to plot. Best to not skip timesteps; e.g., use \code{timesteps=1:100} to plot the first 100 timesteps only.
#' @param outputs  Optional vector with outputs like group or fleet names to plot. Must match output names (column names) in \code{df_cx}.
#'
#' @returns NULL
#' @export
plot_ecosim_all_runs=function(df_cx,run_names=NA,timesteps=NA, outputs=NA)
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
      cl=tol_colors()

      for(r in unique(df_cx$run_name))
      {
        for(i in col_ix) {
          lines(df_cx[df_cx$run_name==r,i]~df_cx$timestep[df_cx$run_name==r],col=cl[i-2])
        }
      }
    }
  NULL
}


#' Plot the variance of Ecosim outputs
#'
#' @param df_cx Data frame with columns \code{run_name}, \code{timestep}, followed by one or more columns with outputs. Can, for example, be created with\code{get_ecosim_cx_biomass}.
#' @param timestep The timestep at which to calculate variances. If \code{NA}, variances are calculated at the last timestep.
#' @param outputs Names of output columns to include in plot. If \code{NA}, all output columns are plotted.
#'
#' @returns Nothing.
#' @export
plot_ecosim_output_variance=function(df_cx,timestep=NA, outputs=NA)
{
  old_par=par(mar = c(10, 4, 4, 2))
  on.exit(par(old_par))

  if(is.na(timestep)) {timestep=max(df_cx$timestep)}
  col_ix=3:ncol(df_cx)
  if(!is.na(outputs[1])) {col_ix=which(colnames(df_cx) %in% outputs) }

  df_cx=df_cx[df_cx$timestep==timestep,]
  out_vars=apply(df_cx[,col_ix],MARGIN = 2,FUN=var)
  names(out_vars)=colnames(df_cx[col_ix])

  barplot(out_vars, las=3, main="Variance of Ecosim output",col="#882255")

}

#' Plot an Ecospace map
#'
#' @param map EcospaceMap object.
#'
#' @returns Nothing.
#' @export
plot_ecospace_map=function(map)
{
  if(class(map)!="EcospaceMap") stop("Please provide a map object.")
  if(0==sum(!is.na(as.numeric(map$values)))) {
    warning("Map contains only NA values. Cannot plot.")
  } else {
    image(t(map$values[nrow(map$values):1,]))}
}
