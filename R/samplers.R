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
#' @returns A data frame with automatically generated run ID, sub-ID, run_name and comment columns; as well as one column per factor with a random sample.
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

#' Create a full factorial (computational) experiment
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


#' Create factor levels for calculating elementary effects
#'
#'
#' @param factor_set A factor set where no factor has more than two levels. Factors with two levels will be treated as binary factors (on/off). Factors with one level that are in the \code{range_table} will have \code{p} levels between their minimum and maximum, as specified in \code{range_table}.
#' @param range_table A table describing the minimum and maximum and number of levels (p) for binary factors. Each row describes one factor. Must have columns: (1) type, (2) name, (3) start - the value at the start of simulations, (4) min - then minimum value, max - the maximum value, p - the number of levels to create. Include only continuous factors that should be set up for elementary effects calculation. Note that the number of levels \code{p} is typically 4, 6, or 8.
#' @param start_change Timestep when factor starts to change.
#' @param end_change Timestep when factor reaches its new level.
#'
#' @returns A factor set where (a) two-level factors in the original factor set have level values 0/1 but are otherwise unchanged, and (b) factors with a single level in the original factor set, if listed in the \code{range_table}, have \code{p} levels between their minimum and maximum.
#' @export
#'
#' @examples
#' xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
#' m=ecocx::load_model_from_xml(xml_model)
#' factor_set=new_ecosim_factor_set(m)
#' factor_set=ecocx::add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,length(factor_set$forcing_functions$PPanomaly$default$values)))
#' summary(factor_set)
#' #obtain default scalar values as basis for range table, only modify fishing effort and temperature, keep PPAnomaly as yes/no
#' range_table=ecocx::get_factor_scalar_values(factor_set)
#' range_table=range_table[c(4:8,11),]
#' range_table$start=c(1,1,1,1,1,16.5)
#' range_table$min=c(0,1,0.8,0.8,0.8,16.5)
#' range_table$max=c(1,3.6,1.2,1.2,1.2,21.5)
#' range_table$p=rep(4,nrow(range_table))
#' range_table
#' factor_set_ee=ecocx::create_ee_levels(factor_set,range_table,200,350)
#' summary(factor_set_ee)
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

#' Create a computational experiment for elementary effects calculation (aka the Morris method)
#'
#' Creates a data frame with trajectories through factor space for the calculation of elementary effects.
#'
#' @param factor_set The factor set. Factors with 1 level are ignored. The other factors must have 2,4,6,8,... levels with equidistant scalar values from 0 to 1 and corresponding changes of their time series/maps. Such a factor set can be created with \code{create_ee_levels} to ensure compliance with the elementary effects method's requirements.
#' @param r The number of trajectories.
#'
#' @returns A data frame with \code{run_id} stating the trajectory, \code{sub_id} stating the model evaluation within each trajectory, \code{run_name} a combination of both, \code{comment} the factor that is changed, and the remaining columns stating the level of each factor in the model run.
#' @export

sampler_ee=function(factor_set,r)
{
  fac_summary=get_factor_scalar_values(factor_set)
  fac_summary$id=paste0(fac_summary$type,"_",fac_summary$name)
  h_name=summary(as.factor(fac_summary$name))
  level_counts=data.frame("id"=names(h_name),"p"=h_name)
  level_counts_notchanged=level_counts[level_counts$p==1,]
  level_counts=level_counts[level_counts$p>1,]
  level_counts$delta=level_counts$p/(2*(level_counts$p-1))

  #create design matrix
  n_runs=r*(nrow(level_counts)+1)
  df=data.frame("run_id"=rep("",n_runs),"sub_id"=rep("",n_runs),"run_name"=rep("",n_runs), "comment"=rep("",n_runs))

  #loop over all trajectories
  for(i in 1:r) {

    #when building the first trajectory, create a column for each factor
    factors=rownames(level_counts)
    if(i==1) {for(fac in factors) {df[[fac]]="not set"}}

    #reorder factors randomly
    factors=factors[sample(1:nrow(level_counts))]

    #sample starting level for each factor
    for(fac in factors)
    {
      #set factors to random start levels
      fac_levels=fac_summary$level[fac_summary$name==fac]
      ix=(i-1)*(nrow(level_counts)+1)+1
      df[[fac]][ix]=sample(fac_levels,size=1)

      #add run name, run id, etc.
      digits=max(2,nchar(as.character(r)))
      df$run_id[ix]=sprintf(paste0("%0",digits,"d"), i)
      digits_sub=max(2,nchar(as.character(max(summary(as.factor(fac_summary$id)))+1)))
      df$sub_id[ix]=sprintf(paste0("%0",digits_sub,"d"), 0)
      df$comment[ix]="start"
    }

    #change factor
    for(j in 1:length(factors))
    {
      #change the current factor while keeping other factors at their prior level
      #first: other factors
      current_row=(i-1)*(nrow(level_counts)+1)+j+1
      fac_col=which(colnames(df)==factors[j])
      other_fac_cols=which(colnames(df)!=factors[j] &colnames(df) %in% factors)
      df[current_row,other_fac_cols]=df[current_row-1,other_fac_cols]

      #then get current and sample new level based on delta
      current_level=df[current_row-1,fac_col]
      current_value=fac_summary$factor_value[fac_summary$name==factors[j]&fac_summary$level==current_level]
      direction=sample(c(-1,1),size=1)
      new_value=current_value+direction*level_counts$delta[level_counts$id==factors[j]]
      if(new_value<0 | new_value>1) {new_value=current_value+(-1)*direction*level_counts$delta[level_counts$id==factors[j]]}

      #find corresponding level name
      fac_levels=fac_summary$level[fac_summary$name==factors[j]]
      fac_values=fac_summary$factor_value[fac_summary$name==factors[j]]
      new_level=fac_levels[which.min(abs(fac_values-new_value))]   #allow tolerance in case of rounding issues

      df[current_row,fac_col]=new_level

      #add run_id, sub_id, run_name,comment
      df$run_id[current_row]=sprintf(paste0("%0",digits,"d"), i)
      df$sub_id[current_row]=sprintf(paste0("%0",digits_sub,"d"), j)
      df$comment[current_row]=factors[j]
    }
  }

  #add factors that were not changed: fishing effort, forcing functions, vulnerability
  for(i in 1:nrow(level_counts_notchanged))
  {
    factor_name=rownames(level_counts_notchanged)[i]
    df[[factor_name]]=fac_summary$level[fac_summary$name==factor_name]
  }

  #add run name
  df$run_name=paste0(df$run_id,"_",df$sub_id)

  df

}





# sampler_sobol=function()
# {
#
# }


