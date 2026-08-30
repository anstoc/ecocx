#Functions to calculate elementary effects

#' Calculate elementary effects
#'
#' @param output_name Name of the output (a column in \code{df_ee} for which elementary effects will be calculated.)
#' @param factor_set The factor set from which the elementary effects design was generated.
#' @param df_ee A data frame containing the information from the design (run names, IDs, sub-IDs, and a comment stating which factor was changed) as well as the output value of each run.
#'
#' @returns A list containing (1) a matrix with the elemntary effect of each factor in each run, (2) mu\* for each factor, (3) sigma for each factor, and (4) a matrix where each row contains mu\* calculated from all trajectories to that point. The latter matrix serves to check if the number of trajectories was sufficient to produce a stable ranking of factors.
#' @export
calculate_effects_ee=function(output_name, factor_set, df_ee)
{
  factor_levels=get_factor_scalar_values(factor_set)
  factors=unique(df_ee$comment[df_ee$comment %in% summary(factor_set)$name])
  ees=matrix(-1,nrow=length(unique(df_ee$run_id)),ncol=length(factors))
  rownames(ees)=unique(df_ee$run_id)
  colnames(ees)=factors
  #go through results line by line and calculate the elementary effect of each factor in each run
  for(i in 1:nrow(df_ee))
  {
    if(df_ee$comment[i] %in% factors) {  #otherwise, it's the starting evaluation
      factor_changed=df_ee$comment[i]
      start_level=df_ee[[factor_changed]][i-1]
      start_value=factor_levels$factor_value[factor_levels$name==factor_changed & factor_levels$level==start_level]
      end_level=df_ee[[factor_changed]][i]
      end_value=factor_levels$factor_value[factor_levels$name==factor_changed & factor_levels$level==end_level]

      delta=abs(end_value-start_value)
      dy=df_ee[[output_name]][i]-df_ee[[output_name]][i-1]

      ees[rownames(ees)==df_ee$run_id[i],colnames(ees)==factor_changed]=dy/delta
    }
  }
  #create list with elementary effects, mu*, sigma, and a table for plotting how the effects stabilize over the runs
  results_ee=list()
  results_ee$matrix=ees
  results_ee$mu_star=colMeans(abs(ees))
  results_ee$sigma=apply(ees,MARGIN=2,FUN=sd)

  mean_table=abs(ees)
  for(i in 2:nrow(mean_table))
  {
    mean_table[i,]=colMeans(abs(ees[1:i,]))
  }

  results_ee$mustar_matrix=mean_table

  results_ee

}



