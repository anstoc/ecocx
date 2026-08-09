#all samplers return a data frame with run id, sub_id, run_name, comment, factor_type, factor_name, and factor values for each run. The run_id and sub_id fields denote related runs, like one random walk in the elementary effects method.

#' Sample one entry from a list, returning the index
#'
#' @param factor An ecocx factor (not necessarily a factor in the R sense) from with one or more options.
#'
#' @returns Index of the option.
sample_option_from_list=function(factor)
{
  if(length(factor)<1) {return(NA)} else {return(sample(1:length(factor),size=1))}
}


#' Randomly sample factor levels for standard Monte Carlo runs
#'
#' @param factor_set A factor set including all options to sample from.
#' @param size Sample size (the number of Monte Carlo runs)
#'
#' @returns A data frame with 'size' rows and automatically generated run ID, sub-ID, run_name and comment columns; as well as one column per factor with a random sample.
#' @export
#'
sampler_random=function(factor_set, size=1)
{
  #set up data structures
  df=data.frame("run_id"=rep("",size),"sub_id"=rep("",size),"run_name"=rep("",size), "comment"=rep("",size))
  fac_sum=summary(factor_set)
  #add columns top data frame
  for(fac_name in fac_sum$name)
  {
    df[[fac_name]]=rep("",size)
  }
  for(i in 1:size)
  {
    #new_row=rep("",ncol(df))
    #names(new_row)=colnames(df)
    df[i,][["run_id"]]=formatC(i, width = max(4,nchar(as.character(size))), flag = "0")
    df[i,][["sub_id"]]="0000"
    df[i,][["run_name"]]=paste0("R",df[i,][["run_id"]],"_",df[i,][["sub_id"]])
    df[i,][["comment"]]=paste("Random sample, run",i)
    for(j in 1:nrow(fac_sum))
    {
      choice=sample_option_from_list(factor_set[[fac_sum$type[j]]][[fac_sum$name[[j]]]])
      choice_name=names(factor_set[[fac_sum$type[j]]][[fac_sum$name[[j]]]])[choice]
      df[i,][[fac_sum$name[j]]]=choice_name
    }
  }
  df
}

#' Create a full factorial (cpmoutational) experiment
#'
#' Creates a data frame with all factor combinations in a factor set.
#'
#' @param factor_set A factor set including all factor levels to combine.
#'
#' @returns A data frame with 'size' rows and automatically generated run ID, sub-ID, run_name and comment columns; as well as one column per factor. Each row contains one combination of factors. Together, the data frame covers all possible combinations
#' @export
#'
sampler_full_factorial=function(factor_set)
{
  factor_list=list()
  for(outer in 1:length(factor_set))
  {
    for(inner in 1:length(factor_set[[outer]]))
    {
      factor_name=names(factor_set[[outer]])[inner]
      factor_options=names(factor_set[[outer]][[factor_name]])
      factor_list[[factor_name]]=factor_options
    }
  }
  d_factors=expand.grid(factor_list)

  #make data frame with run ids, names etc., then merge
  df=data.frame("run_id"=rep("",nrow(d_factors)),"sub_id"=rep("",nrow(d_factors)),"run_name"=rep("",nrow(d_factors)), "comment"=rep("",nrow(d_factors)))

  for(i in 1:nrow(df))
  {
    df[i,][["run_id"]]=formatC(i, width = max(4,nchar(as.character(nrow(df)))), flag = "0")
    df[i,][["sub_id"]]="0000"
    df[i,][["run_name"]]=paste0("R",df[i,][["run_id"]],"_",df[i,][["sub_id"]])
    df[i,][["comment"]]=paste("Full factorial experiment, combination",i,"of",nrow(df))
  }

  df=cbind(df,d_factors)

  df

}

#range_table: type,name, start, min, max, p. Include only those factors to set up for EE
create_ee_levels=function(factor_set,range_table, start_change, end_change)
{
  fac_summary=summary(factor_set)
  if(max(fac_summary$options)>=3) {stop("Initial factor set must not contain factors with more than two levels. Factors with two level will be set to binary choices. Factors with one level will be expanded according to the range table (but factors with one level that are not listed in the range table are omitted).")}
  #for factors with two levels, set their scalar values to 0 and 1.
  for(ix in which(fac_summary$options==2))
  {
    factor_set[[fac_summary$type[ix]]][[fac_summary$name[ix]]][[1]]$factor_value=0
    factor_set[[fac_summary$type[ix]]][[fac_summary$name[ix]]][[2]]$factor_value=1
  }
  #for factors with an entry in the range table, create p levels between min and max
  for(i in 1:nrow(range_table))
  {
    if(range_table$p[i] %%2 !=0) {stop("p should be even; 4,6,and 8 are common choices.")}
    if(fac_summary$options[fac_summary$name==range_table$name[i] & fac_summary$type==range_table$type[i]] > 1) {
      warning("Skipping factors in the range table that already have more than one option.")} else {
      #create p levels with start point until start_change, the linear change to the required level based on the range table until end_change, then the new (level) value
      p=range_table$p[i]
      p_levels=0:(p-1)/(p-1)
      #create one option for each level
      for(level in p_levels) {
        #create values
        start=range_table$start[i]
        end=range_table$min[i]+level*(range_table$max[i]-range_table$min[i])
        level_values=rep(start,length(factor_set[[range_table$type[i]]][[range_table$name[i]]][[1]]$values))
        level_values=change_values_add(level_values,(end-start),start_change,end_change)
        if(range_table$type[i]=="fishing_effort") {
          factor_set=add_option_ecosim_effort(factor_set,range_table$name[i],paste0("ee",round(level,2)),level_values,level)
        } else if(range_table$type[i]=="forcing_functions") {
          factor_set=add_option_ecosim_forcing(factor_set,range_table$name[i],paste0("ee",round(level,2)),level_values,level)
        }
      }
    }
    factor_set[[range_table$type[i]]][[range_table$name[i]]][[1]] = NULL
  }

  factor_set

}

sampler_morris=function(factor_set)
{

}

# sampler_sobol=function()
# {
#
# }


