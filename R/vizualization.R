

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
