#' Obtain information about types of time series
#'
#' 'lut_tscode' provides information about time series types, like descriptive names and units, from an Ecosim-specific code, for example, whether they are drivers (forcing certain values at model runtime) or reference time series (for model fitting).
#' @param code The numerical time series code or NULL.
#' @returns If a code was provided, a data frame with one row containing information about the code provided. If the code was NULL, a look-up table with codes and associated information.
#' @examples
#' lut_tscode(1)
#' lut_tscode("3")
#' lut_tscode()
#' @export
lut_tscode=function(code=NULL)
{
  lut=utils::read.csv(paste0(system.file('extdata', package = 'ecocx'),"/ts_codes.csv"))
  if(is.null(code)) {return(lut)} else
  {
    code=as.numeric(code)
    ix=which(lut$Type==code)
    return(lut[ix,])
  }
}

#' Obtain the path of the folder with raw example data
#' @returns A string with the path.
#' @export
get_path_to_exampledata=function()
{
  paste0(system.file('extdata', package = 'ecocx'),"/")
}

#' Gradually change values by multiplying with a factor
#' @param values Numeric vector with the original values.
#' @param multi Multiplier. For example, 1.2 implies a 20 percent increase and 0.8 a 20 percent decrease.
#' @param start Index where change starts
#' @param end Index where change ends
#' @returns Numeric vector that contains original values before \code{start}, \code{multi} times the original values from \code{end} onwards, and linearly changed values in between.
#' @export
#' @examples
#' v1=c(1,1,1,1,1)
#' ecocx::change_values_mult(v1,1.5,2,4)
change_values_mult=function(values,multi,start,end)
{
  v_mult=numeric(length(values))
  steps=end-start+1
  v_mult[1:(start-1)]=0
  v_mult[start:end]=(1:(length(start:end)))*(multi-1)/steps
  v_mult[end:length(v_mult)]=multi-1
  v_mult=v_mult+1
  values*v_mult
}

#' Gradually change values by adding a number
#' @param values Numeric vector with the original values.
#' @param summand The number to add.
#' @param start Index where change starts.
#' @param end Index where change ends.
#' @returns Numeric vector that contains original values before \code{start}, the original values plus \code{summand} from \code{end} onwards, and linearly changed values in between.
#' @export
#' @examples
#' v1=c(2,2,2,2,2)
#' ecocx::change_values_add(v1,1,2,4)
change_values_add=function(values,summand,start,end)
{
  v_add=numeric(length(values))
  steps=end-start+1
  v_add[1:(start-1)]=0
  v_add[start:end]=(1:(length(start:end)))*(summand)/steps
  v_add[end:length(v_add)]=summand
  values+v_add

}
