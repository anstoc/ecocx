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
