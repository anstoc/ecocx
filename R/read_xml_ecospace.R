#' Load Ecospace scenario information from EIIXML
#' @param xmldoc XML2 document.
#' @returns Data frame with Ecospace scenario names and IDs.
#' @noRd
get_ecospace_scenarios=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcospaceScenario")[[1]]
  df=table_to_df(tab)
  data.frame("ScenarioID"=as.numeric(df$ScenarioID),"ScenarioName"=df$ScenarioName,"TotalTime"=as.numeric(df$TotalTime))
}


#' Load Ecospace depth map from EIIXML
#'
#' @param xmldoc XML2 document.
#' @param scenario_name Name of the scenario.
#'
#' @returns Object of class \code{EcospaceMap} containing information about the map (e.g., number of rows and columns) and depth values.
#' @noRd
get_ecospace_depthmap=function(xmldoc,scenario_name)
{
  tab=get_tables_from_name(xmldoc,"EcospaceScenario")[[1]]
  df=table_to_df(tab)
  scen_id=df$ScenarioID[df$ScenarioName==scenario_name]

  if(length(scen_id)==0) {stop(paste("Scenario not found. Options:",paste0(unique(df$ScenarioName),collapse=", ")))}

  depthmap=list()
  depthmap$name="Depth"
  depthmap$scenario=scenario_name
  depthmap$nrow=as.numeric(df$Inrow[df$ScenarioID==scen_id])
  depthmap$ncol=as.numeric(df$Incol[df$ScenarioID==scen_id])
  #parse depth matrix
  m=matrix(NA,nrow=depthmap$nrow,ncol=depthmap$ncol)
  if(startsWith(df$DepthMap[df$ScenarioID==scen_id],"gzip:")) {   #compressed
    str_depth=decode_ewe_map(df$DepthMap[df$ScenarioID==scen_id])
    for(i in 1:(length(str_depth))) {m[i,]=str_depth[[i]]}
  } else { #not compressed
    str_depth=unlist(strsplit(df$DepthMap[df$ScenarioID==scen_id],split="\""))[2] #remove leading quotation mark
    str_depth=unlist(strsplit(str_depth,split=";"))
    for(i in 1:(length(str_depth)))
    {
        m[i,]=as.numeric(unlist(strsplit(str_depth[i],split=" ")))
    }
  }
  depthmap$values=m
  class(depthmap)="EcospaceMap"
  depthmap
}


#' #' Load map of Ecospace study area from EIIXML
#'
#' @param xmldoc XML2 document.
#' @param scenario_name Name of the scenario.
#'
#' @returns Object of class \code{EcospaceMap} containing information about the map (e.g., number of rows and columns) and depth values.
#' @export
get_ecospace_basemap=function(xmldoc,scenario_name)
{
  map=get_ecospace_depthmap(xmldoc,scenario_name)
  map$values[map$values==0]=NA
  map$values[!is.na(map$values)]=0
  map$name="StudyArea"
  map
}


#' Parse map data string from Ecospace EIIXML
#'
#' @param mapdata String with map data from EIICML. May be plain or compressed.
#' @param basemap The basemap (0 in water cells/the study area, NA elsewhere).
#' @param mpa Boolean: is this an MPA map? EwE stores MPA maps are stored in a different format.
#'
#' @returns Matrix with the map data.
extract_map_data=function(mapdata,basemap, mpa=F)
{
  m=basemap$values
  if(!startsWith(mapdata,"gzip") & !grepl("[0-9]", mapdata)) {  #not compressed, no numbers
    warning("Map contained no data. Returning NULL.")
    return(NULL)
  } else if(startsWith(mapdata,"gzip")) {  #compressed
    str_map=decode_ewe_map(mapdata,nodata=0) #MPA: has specific format
    if(mpa) {
      for(j in 1:nrow(m)) {
        ix=which(!is.na(m[j,]))
        if(length(ix)>0 & length(str_map[[j]]>0)) {
          str_map[[j]][is.na(str_map[[j]])]=0
          m[j,ix]=m[j,ix]+str_map[[j]]}
      }
    } else { #other maps (not MPAs)
      for(j in 1:nrow(m)) {
        ix=which(!is.na(m[j,]))
        if(length(ix)>0) {m[j,ix]=str_map[[j]]}
      }
    }
  } else {
    str_map=unlist(strsplit(mapdata,split=";"))
    for(j in 1:nrow(m))
    {
      ix=which(!is.na(m[j,]))
      if(length(ix)>0) {
        values=as.numeric(unlist(strsplit(str_map[j],split=" ")))
        m[j,ix]=values
      }
    }
  }
  m
}

#' #' Load static environmental maps from EIIXML
#'
#' @param xmldoc XML2 document.
#' @param scenario_name Name of the scenario.
#'
#' @returns List of \code{EcospaceMap} objects, each containing one environmental map.
#' @export
get_ecospace_envmaps=function(xmldoc,scenario_name)
{
  #load basemap as only data for in-AOI cells are stored
  basemap=get_ecospace_basemap(xmldoc, scenario_name)

  #load scenario information
  scenarios=get_ecospace_scenarios(xmldoc)

  #load driver node
  tab=get_tables_from_name(xmldoc,"EcospaceScenarioDriverLayer")[[1]]

  #load data table
  df=table_to_df(tab)
  df=df[df$ScenarioID==scenarios$ScenarioID[scenarios$ScenarioName==scenario_name],]

  #create list of drivers, then parse values for each driver
  l_drivers=list()
  for(i in 1:nrow(df))
  {
    driver=list()
    driver$name=df$LayerName[i]
    driver$id=df$LayerID[i]
    driver$values=extract_map_data(df$LayerMAP[i],basemap)
    class(driver)="EcospaceMap"
    l_drivers[[driver$name]]=driver
  }
  l_drivers
}

#' #' Load static habitat maps from EIIXML
#'
#' @param xmldoc XML2 document.
#' @param scenario_name Name of the scenario.
#'
#' @returns List of \code{EcospaceMap} objects, each containing one habitat map.
#' @export
get_ecospace_habmaps=function(xmldoc,scenario_name)
{
  #load basemap as only data for in-AOI cells are stored
  basemap=get_ecospace_basemap(xmldoc, scenario_name)

  #load scenario information
  scenarios=get_ecospace_scenarios(xmldoc)

  #load driver node
  tab=get_tables_from_name(xmldoc,"EcospaceScenarioHabitat")[[1]]

  #load data table
  df=table_to_df(tab)
  df=df[df$ScenarioID==scenarios$ScenarioID[scenarios$ScenarioName==scenario_name],]

  #create list of drivers, then parse values for each driver
  l_maps=list()
  for(i in 1:nrow(df))
  {
    map=list()
    map$name=df$HabitatName[i]
    map$id=df$HabitatID[i]
    map$values=extract_map_data(df$HabitatMap[i],basemap)
    class(map)="EcospaceMap"
    l_maps[[map$name]]=map
  }
  l_maps
}

#' Load static MPA maps from EIIXML
#'
#' @param xmldoc XML2 document.
#' @param scenario_name Name of the scenario.
#'
#' @returns List of \code{EcospaceMap} objects, each containing one MPA.
#' @export
get_ecospace_mpamaps=function(xmldoc,scenario_name)
{
  #load basemap as only data for in-AOI cells are stored
  basemap=get_ecospace_basemap(xmldoc, scenario_name)

  #load scenario information
  scenarios=get_ecospace_scenarios(xmldoc)

  #load driver node
  tab=get_tables_from_name(xmldoc,"EcospaceScenarioMPA")[[1]]

  #load data table
  df=table_to_df(tab)
  df=df[df$ScenarioID==scenarios$ScenarioID[scenarios$ScenarioName==scenario_name],]

  #create list of MPAs, then parse spatial data for each
  l_maps=list()
  for(i in 1:nrow(df))
  {
    map=list()
    map$name=df$MPAname[i]
    map$id=df$MPAID[i]
    map$months=df$MPAmonth[i]
    map$values=extract_map_data(df$MPAMap[i],basemap,mpa=T)
    if(is.null(map$values)) {map$values=basemap}
    class(map)="EcospaceMap"
    l_maps[[map$name]]=map
  }
  l_maps
}


#TODO
#load Ecospace data into model
#factor for static Ecospace maps
#deal with Ecosim scenarios
#finalize tutorial


