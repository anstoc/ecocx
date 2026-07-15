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

# #walk structure and find leaves with their type. This from Claude
# extract_nodes <- function(x, parent = NA, parent_class = NULL) {
#   result <- data.frame(child = character(0), parent = character(0), stringsAsFactors = FALSE)
#   nms <- names(x)
#   for (i in seq_along(x)) {
#     child <- x[[i]]
#     nm <- if (is.null(nms)) NA else nms[i]
#     is_leaf <- !is.list(child)
#     has_class <- !is.null(parent_class) && inherits(child, parent_class)
#     if (is_leaf || has_class) {
#       result <- rbind(result, data.frame(child = nm, parent = parent, stringsAsFactors = FALSE))
#     } else if (is.list(child)) {
#       result <- rbind(result, extract_nodes(child, parent = nm, parent_class = parent_class))
#     }
#   }
#   result
# }

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

# sampler_morris=function(factor_set)
# {
#
# }

# sampler_sobol=function()
# {
#
# }


