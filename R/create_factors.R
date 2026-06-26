#TODO: Don't call them factors and factor levels due to confusion with R names?

#' Create a set of factors that vary between Ecosim model runs
#'
#' The factor set contain lists of alternative fishing effort, environmental response shapes, and other potential factors that might vary between model runs.
#' First, use this function to generate a factor set with one level per factor,then add alternatives with add_ecosim_factor_level().
#'
#' @param m An Ecosim model object created with load_model_from_xml().
#' @param default_name Name that the default value
#'
#' @returns A list of factors like fishing effort and environmental time series with their default values in the model.
#' @export
#'
#' @examples
#' xmlfile=paste0(ecocx::get_path_to_exampledata(),'anchovy_bay_ecosim_ex.eiixml')
#' m <- ecocx::load_model_from_xml(xmfile)
#' flist <- new_ecosim_factor_set(m)
new_ecosim_factor_set=function(m, default_name="default")
{
  factor_set=list()
  class(factor_set)="ecocx_factor_set"

  factor_set$tables=list()

  #foraging response table
  factor_set$tables$foraging_resp=list()
  factor_set$tables$foraging_resp[[default_name]]=m$ecosim$foraging_response_table

  #mediation table
  factor_set$tables$mediation=list()
  factor_set$tables$mediation[[default_name]]=m$ecosim$mediation_table

  #vulnerabilities
  factor_set$tables$vulnerability=list()
  factor_set$tables$vulnerability[[default_name]]=m$ecosim$vulnerabilities

  #fishing effort
  factor_set$fishing_effort=list()
  for(i in 1:length(m$ecosim$fishing_effort)) {
    factor_set$fishing_effort[[names(m$ecosim$fishing_effort)[i]]]=list()
    factor_set$fishing_effort[[names(m$ecosim$fishing_effort)[i]]][[default_name]]=m$ecosim$fishing_effort[[i]]
  }

  #forcing functions
  factor_set$forcing_functions=list()
  for(i in 1:length(m$ecosim$forcing_functions)) {
    factor_set$forcing_functions[[names(m$ecosim$forcing_functions)[i]]]=list()
    factor_set$forcing_functions[[names(m$ecosim$forcing_functions)[i]]][[default_name]]=m$ecosim$forcing_functions[[i]]
  }

  #shapes
  factor_set$shapes=list()
  for(i in 1:length(m$ecosim$shapes)) {
    factor_set$shapes[[names(m$ecosim$shapes)[i]]]=list()
    factor_set$shapes[[names(m$ecosim$shapes)[i]]][[default_name]]=m$ecosim$shapes[[i]]
  }

  factor_set

}


#' Get length of a forcing function time series
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param fleet_name Name of the fleet
#'
#' @returns Length of the time series (i.e., the number of time steps).
#' @export
get_ecosim_effort_length=function(factor_set,fleet_name)
{
  length(factor_set$fishing_effort[[fleet_name]][[1]]$values)
}



#' Add an additional option for the fishing effort time series of a fleet
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param fleet_name Name of the fleet
#' @param option_name Name for the new option
#' @param effort_values A numeric vector with the new effort values. It must have the same length as the existing effort time series for the fleet.
#' @returns An updated factor set object.
#' @export
add_option_ecosim_effort=function(factor_set,fleet_name,option_name,effort_values)
{
  #check if inputs are consistent with model information and effort values are >=0
  if(!(fleet_name %in% names(factor_set$fishing_effort))) {stop(paste("Fleet",fleet_name,"not found in factor_set."))}
  if(option_name %in% names(factor_set$fishing_effort[[fleet_name]])) {stop("A factor level with this name already exists. To avoid accidental overwriting, remove it with remove_option_ecosim_effort(), then try again")}
  if(!(is.numeric(effort_values) & length(effort_values)==get_ecosim_effort_length(factor_set,fleet_name))) {stop("The parameter effort_values must be a numeric vector of the same length as the effort time series in the original model")}
  if(is.na(sum(effort_values)) | length(which(effort_values<0)>0)) {stop("The parameter effort_values must not contain NAs or negative numbers.")}

  #create the new effort option
  new_option=factor_set$fishing_effort[[fleet_name]][[1]]
  new_option$values=effort_values
  factor_set$fishing_effort[[fleet_name]][[option_name]]=new_option

  return(factor_set)
}



#' Remove an option for the effort time series of a fleet.
#'
#'At least one option must remain at all times. To replace the last option, add a new option first, then remove the olds one.
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param fleet_name Name of the fleet
#' @param option_name Name of the option to remove
#'
#' @returns An updated factor set object.
#' @export

remove_option_ecosim_effort=function(factor_set,fleet_name,option_name)
{
  #check if inputs are consistent with model information and not removing the last option for the effort
  if(!(fleet_name %in% names(factor_set$fishing_effort))) {stop(paste("Fleet",fleet_name,"not found in factor_set."))}
  if(!(option_name %in% names(factor_set$fishing_effort[[fleet_name]]))) {stop("No option with this name.")}
  if(length(factor_set$fishing_effort[[fleet_name]])==1) {stop("Cannot delete the last remaining options for this factor. Please add new options before deleting this one.")}

  #remove from list
  factor_set$fishing_effort[[fleet_name]][[option_name]] <- NULL
  return(factor_set)
}

###

#' Get length of a forcing function (time series)
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param forcing_name Name of the forcing function
#'
#' @returns Length of the forcing function time series (i.e., the number of time steps).
#' @export
get_ecosim_forcing_length=function(factor_set,forcing_name)
{
  length(factor_set$forcing_functions[[forcing_name]][[1]]$values)
}

#' Add an additional option for a forcing function time series
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param forcing_name Name of the forcing function
#' @param option_name Name of the new option
#' @param forcing_values Numeric vector with the new values for the forcing function. It must have the same length as the existing effort time series for the fleet.
#' @returns An updated factor set object.
#' @export
add_option_ecosim_forcing=function(factor_set,forcing_name,option_name,forcing_values)
{
  #check if inputs are consistent with model information
  if(!(forcing_name %in% names(factor_set$forcing_functions))) {stop(paste("Forcing function",forcing_name,"not found in factor_set."))}
  if(option_name %in% names(factor_set$forcing_functions[[forcing_name]])) {stop("A factor level with this name already exists. To avoid accidental overwriting, remove it with remove_option_ecosim_forcing(), then try again.")}
  if(!(is.numeric(forcing_values) & length(forcing_values)==get_ecosim_forcing_length(factor_set,forcing_name))) {stop("The parameter forcing_values must be a numeric vector of the same length as the forcing time series in the original model.")}
  if(is.na(sum(forcing_values))) {stop("The parameter forcing_values must not contain NAs.")}

  #create the new forcing option
  new_option=factor_set$forcing_functions[[forcing_name]][[1]]
  new_option$values=forcing_values
  factor_set$forcing_functions[[forcing_name]][[option_name]]=new_option

  return(factor_set)
}

#' Remove an option for a forcing time series.
#'
#'At least one option must remain at all times. To replace the last option, add a new option first, then remove the old one.
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param forcing_name Name of the forcing function
#' @param option_name Name of the option to remove
#'
#' @returns An updated factor set object.
#' @export
remove_option_ecosim_forcing=function(factor_set,forcing_name,option_name)
{
  #check if inputs are consistent with model information and not removing the last option for the effort
  if(!(forcing_name %in% names(factor_set$forcing_functions))) {stop(paste("Forcing function",fleet_name,"not found in factor_set."))}
  if(!(option_name %in% names(factor_set$forcing_functions[[forcing_name]]))) {stop("No option with this name.")}
  if(length(factor_set$forcing_functions[[forcing_name]])==1) {stop("Cannot delete the last remaining options for this factor. Please add new options before deleting this one.")}

  #remove from list
  factor_set$forcing_functions[[forcing_name]][[option_name]] <- NULL
  return(factor_set)
}

##

#' Add an additional option for a (environmental response or mediation) shape
#'
#' @param factor_set A factor set created with new_ecosim_factor_set()
#' @param shape_name Name of the shape
#' @param option_name Name of the new option
#' @param shape_x Numeric vector with the new x values for the shape. Must have a length of 1200 (an Ecosim convention).
#' @param shape_y Numeric vector with the new y values for the shape. Must have a length of 1200 (an Ecosim convention).
#' @returns  Updated factor set object.
#' @export
add_option_ecosim_shape=function(factor_set,shape_name,option_name,shape_x, shape_y)
{
  #check if inputs are consistent with model information
  if(!(shape_name %in% names(factor_set$shapes))) {stop(paste("Shape",shape_name,"not found in factor_set."))}
  if(option_name %in% names(factor_set$shapes[[shape_name]])) {stop("A factor level with this name already exists. To avoid accidental overwriting, remove it with remove_option_ecosim_shape(), then try again.")}
  if(!(is.numeric(shape_x) & is.numeric(shape_y) & length(shape_x)==1200 & length(shape_y)==1200)) {stop("The x and y values must be a numeric vector with length 1200 (an Ecosim legacy).")}
  if(is.na(sum(shape_x) | is.na(sum(shape_y)))) {stop("The x and y values must not contain NAs.")}

  #create the new shape option
  new_option=factor_set$shapes[[shape_name]][[1]]
  new_option$y=shape_y
  new_option$x=shape_x
  new_option$xmin=min(shape_x)
  new_option$xmax=max(shape_x)

  factor_set$shapes[[shape_name]][[option_name]]=new_option

  return(factor_set)
}




#' Remove an option for a shape.
#'
#'At least one option must remain at all times. To replace the last option, add a new option first, then remove the old one.
#'
#' @param factor_set A factor set, e.g.,created with new_ecosim_factor_set()
#' @param shape_name Name of the shape
#' @param option_name Name of the option to remove
#'
#' @returns An updated factor set object.
#' @export
remove_option_ecosim_shape=function(factor_set,shape_name,option_name)
{
  #check if inputs are consistent with model information and not removing the last option for the effort
  if(!(shape_name %in% names(factor_set$shapes))) {stop(paste("Shape",shape_name,"not found in factor_set."))}
  if(!(option_name %in% names(factor_set$shapes[[shape_name]]))) {stop("No option with this name.")}
  if(length(factor_set$shapes[[shape_name]])==1) {stop("Cannot delete the last remaining options for this factor. Please add new options before deleting this one.")}

  #remove from list
  factor_set$shapes[[shape_name]][[option_name]] <- NULL
  return(factor_set)
}



#' Add an alternative vulnerability matrix.
#'
#' @param factor_set A factor set, e.g., created with new_ecosim_factor_set().
#' @param option_name Name of the new option.
#' @param v_matrix New vulnerability matrix. It must have the same dimensions as the original model's vulnerability matrix, contain numbers where the original model's matrix contains numbers, and NAs where the original model's matrix contains NAs.
#'
#' @returns An updated factor set including the new option.
#' @export
add_option_ecosim_vulnerability=function(factor_set,option_name,v_matrix)
{
  #check if matrix dimensions are right and if all pred-prey pairs have data or NA like in the original matrix
  if(!identical(dim(v_matrix),dim(factor_set$tables$vulnerability[[1]]))) {stop("The provided vulnerability matrix must have the same dimensions as the matrix in the original models.")}
  if(!identical(is.na(as.numeric(v_matrix)),is.na(as.numeric(factor_set$tables$vulnerability[[1]])))) {stop("The provided vulnerability matrix must have numbers where the original model's matrix has numbers, and NAs where the original model's matrix has NAs.")}

  #add to option list
  factor_set$tables$vulnerability[[option_name]]=v_matrix
  factor_set

}

#' Remove an option for the vulnerability table.
#'
#'At least one option must remain at all times. To replace the last option, add a new option first, then remove the old one.
#'
#' @param factor_set A factor set, e.g.,created with new_ecosim_factor_set().
#' @param option_name Name of the option to remove.
#'
#' @returns An updated factor set object.
#' @export
remove_option_ecosim_vulnerability=function(factor_set,option_name)
{
  if(length(factor_set$tables$vulnerability)==1) {stop("Cannot remove the last option for the vulnerability matrix. If you wish to replace it, add the replacement first, then remove the old option.")}
  if(!(option_name %in% names(factor_set$tables$vulnerability))) {stop("No option with this name in the factor set.")}
  factor_set$tables$vulnerability[[option_name]]=NULL
  factor_set
}


#' Summary function for factor sets
#'
#' @param object The factor set
#' @param ... Unused
#'
#' @returns A data frame with the number of options per factor.
#' @exportS3Method base::summary ecocx_factor_set
summary.ecocx_factor_set=function(object, ...)
{
  # d=data.frame(Type=character(),Name=character(),Options=numeric(),stringsAsFactors=FALSE)
  # for(name in names(object$forcing_functions)) {
  #   new_row=data.frame(Type="Forcing function",Name=name,Options=length(names(object$forcing_functions[[name]])))
  #   d=rbind(d,new_row)
  # }
  # for(name in names(object$fishing_effort)) {
  #   new_row=data.frame(Type="Fishing effort",Name=name,Options=length(names(object$fishing_effort[[name]])))
  #   d=rbind(d,new_row)
  # }
  # for(name in names(object$shapes)) {
  #   new_row=data.frame(Type="Shape",Name=name,Options=length(names(object$shapes[[name]])))
  #   d=rbind(d,new_row)
  # }
  # d=rbind(d,data.frame(Type="Table",Name="Vulnerability table",Options=length(object$vulnerability_tables)))
  # d=rbind(d,data.frame(Type="Foraging response table",Name="Foraging response table",Options=length(object$foraging_resp_tables)))
  # d=rbind(d,data.frame(Type="Mediation table",Name="Mediation table",Options=length(object$mediation_tables)))

  d=data.frame(Type=character(),Name=character(),Options=numeric(),stringsAsFactors=FALSE)
  for(type in names(object)) {
    for(name in names(object[[type]])) {
      new_row=data.frame("type"=type,"name"=name,"options"=length(object[[type]][[name]]))
      d=rbind(d,new_row)
    }
  }
  d
}



#TODO

#options for foraging response tables
#options for mediation response tables


#' Plot function for factor sets
#'
#' @param object The factor set
#' @param ... Unused
#'
#' Shows one plot for each option in the factor set.
#'
#' @returns Nothing.
#' @exportS3Method base::plot ecocx_factor_set
plot.ecocx_factor_set <- function(x, ...) {

  col <- "steelblue"

  ## ---- pretty category labels (fallback: prettified name) ---------------
  pretty_category <- function(key) {
    lbl <- c(fishing_effort       = "Fishing effort",
             forcing_functions    = "Forcing function",
             shapes               = "Shape",
             vulnerability_tables = "Vulnerability table",
             mediation_tables     = "Mediation table",
             foraging_resp_tables = "Foraging response table")
    if (key %in% names(lbl)) lbl[[key]]
    else tools::toTitleCase(gsub("_", " ", key))
  }

  make_title <- function(parent, element, node, sc) {
    nm  <- names(node); pos <- match(sc, nm)
    head <- if (is.null(element) || identical(element, parent))
      parent else paste0(parent, " - ", element)
    sprintf("%s: %s (%d/%d)", head, sc, pos, length(nm))
  }

  ## ---- leaf drawing routines, keyed by class ----------------------------
  draw_timeseries <- function(obj, title) {           # EcosimEffortTS / EcosimForcingF
    op <- par(mar = c(4, 4, 2.5, 1)); on.exit(par(op))
    v <- obj$values
    plot(seq_along(v), v, type = "l", col = col,
         main = title, xlab = "Time step", ylab = "Value", ...)
  }
  draw_shape <- function(obj, title) {                # EcosimShape
    op <- par(mar = c(4, 4, 2.5, 1)); on.exit(par(op))
    xv <- if (!is.null(obj$x)) obj$x else seq_along(obj$y)
    plot(xv, obj$y, type = "l", col = col,
         main = title, xlab = "x", ylab = "y", ...)
  }
  draw_heat <- function(m, title) {                   # numeric matrix
    nr <- nrow(m); nc <- ncol(m)
    op <- par(mar = c(7, 7, 3, 2)); on.exit(par(op))
    image(seq_len(nc), seq_len(nr), t(m[nr:1, , drop = FALSE]),
          axes = FALSE, xlab = "", ylab = "", main = title,
          col = hcl.colors(24, "YlOrRd", rev = TRUE))
    axis(1, at = seq_len(nc), labels = colnames(m), las = 2, cex.axis = 0.7)
    axis(2, at = seq_len(nr), labels = rev(rownames(m)), las = 2, cex.axis = 0.7)
    box()
    for (i in seq_len(nr)) for (j in seq_len(nc)) {
      v <- m[i, j]
      if (!is.na(v)) text(j, nr - i + 1, formatC(v, digits = 2, format = "g"), cex = 0.6)
    }
  }
  draw_table <- function(df, title) {                 # data.frame
    nr <- nrow(df); nc <- ncol(df)
    op <- par(mar = c(1, 1, 3, 1)); on.exit(par(op))
    plot(NA, xlim = c(0, nc), ylim = c(0, nr + 1),
         axes = FALSE, xlab = "", ylab = "", main = title)
    text(seq_len(nc) - 0.5, nr + 0.5, colnames(df), font = 2, cex = 0.8)
    for (i in seq_len(nr)) {
      yr <- nr - i + 0.5
      for (j in seq_len(nc)) text(j - 0.5, yr, as.character(df[i, j]), cex = 0.75)
    }
    abline(h = 0:nr, col = "grey85"); abline(v = 0:nc, col = "grey85")
    abline(h = nr, col = "grey40")
  }

  # decide drawing routine for one leaf; returns a function(title) or NULL
  dispatch_leaf <- function(leaf) {
    cls <- attr(leaf, "class")
    if (!is.null(cls)) {
      if ("EcosimShape" %in% cls)                   return(function(t) draw_shape(leaf, t))
      if (any(c("EcosimEffortTS", "EcosimForcingF") %in% cls))
        if (!is.null(leaf$values))                  return(function(t) draw_timeseries(leaf, t))
    }
    if (is.matrix(leaf) && is.numeric(leaf))        return(function(t) draw_heat(leaf, t))
    if (is.data.frame(leaf))                        return(function(t) draw_table(leaf, t))
    if (is.list(leaf) && !is.null(leaf$values))     return(function(t) draw_timeseries(leaf, t))
    if (is.list(leaf) && !is.null(leaf$y))          return(function(t) draw_shape(leaf, t))
    NULL
  }

  ## ---- walk the tree: category -> element -> scenario -------------------
  panels <- list()  # each: list(draw = function(title), title = chr)

  for (cat in names(x)) {
    catnode <- x[[cat]]
    if (!is.list(catnode)) next
    catlabel <- pretty_category(cat)

    for (elem in names(catnode)) {
      node <- catnode[[elem]]

      # Case A: the element node is ITSELF a drawable leaf (the tables:
      #   vulnerability_tables$default is the matrix; mediation_tables$default
      #   is the data frame). There is no scenario level beneath it.
      drawer <- dispatch_leaf(node)
      if (!is.null(drawer)) {
        sc    <- elem                       # the element name acts as scenario
        title <- make_title(catlabel, NULL, catnode, sc)
        panels[[length(panels) + 1L]] <- list(draw = drawer, title = title)
        next
      }

      # Case B: the element node is a container of scenarios (fleets, forcing
      #   functions, shapes): category -> element -> scenario -> leaf.
      if (!is.list(node)) next

      element_is_redundant <- identical(elem, cat) ||
        (length(names(catnode)) == 1L && identical(tolower(elem), "default"))

      for (sc in names(node)) {
        leaf   <- node[[sc]]
        drawer <- dispatch_leaf(leaf)
        if (is.null(drawer)) next

        label <- if (is.list(leaf) && !is.null(leaf$name)) leaf$name else elem
        if (element_is_redundant) label <- NULL

        title <- make_title(catlabel, label, node, sc)
        panels[[length(panels) + 1L]] <- list(draw = drawer, title = title)
      }
    }
  }

  n <- length(panels)
  if (n == 0L) {
    warning("No plottable components found.")
    return(invisible(x))
  }

  ## ---- draw, one panel per leaf, advancing on key stroke ----------------
  if (interactive() && n > 1L) {
    oask <- devAskNewPage(TRUE); on.exit(devAskNewPage(oask))
  }

  for (p in panels) p$draw(p$title)

  invisible(x)
}


