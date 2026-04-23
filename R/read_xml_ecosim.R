
#' Get information about Ecosim scenarios from XML
#' @param xmldoc XML2 document
#' @returns Data frame with scenario, Ecoapth and Ecosim group IDs
#' @noRd
get_ecosim_scenarios=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcosimScenario")[[1]]
  df=table_to_df(tab)
  data.frame("ScenarioID"=as.numeric(df$ScenarioID),"ScenarioName"=df$ScenarioName)
}

#' Extract Ecosim group IDs, which can be different from Ecopath group IDs, from XML
#' @param xmldoc XML2 document
#' @returns Data frame with scenario IDs, Ecopath group IDs, and Ecosim group IDs
#' @noRd
get_ecosim_groupIDs=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcosimScenarioGroup")[[1]]
  df=table_to_df(tab)
  data.frame("EcopathGroupID"=as.numeric(df$EcopathGroupID),"EcosimGroupID"=as.numeric(df$GroupID),"ScenarioID"=as.numeric(df$ScenarioID))
}

#' Read Ecosim fleet IDs, which can be different from Ecopath fleet IDs, from XML
#' @param xmldoc XML2 document
#' @returns Data frame with scenario IDs, Ecopath fleet IDs, and Ecosim fleet IDs
#' @noRd
get_ecosim_fleetIDs=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcosimScenarioFleet")[[1]]
  df=table_to_df(tab)
  data.frame("EcopathFleetID"=as.numeric(df$EcopathFleetID),"EcosimFleetID"=as.numeric(df$FleetID),"ScenarioID"=as.numeric(df$ScenarioID))
}


#' Read vulnerability matrix from XML
#' @param xmldoc XML2 document
#' @param d_basic Data frame with basic estimates created with get_basic_estimates()
#' @returns Matrix with vulnerabilities
#' @noRd
get_vulnerability_matrix=function(xmldoc,d_basic)
{
  tab=get_tables_from_name(xmldoc,"EcosimScenarioForcingMatrix")[[1]]
  d=table_to_df(tab)

  ids=get_ecosim_groupIDs(xmldoc)

  #create matrix
  m=matrix(-9999,nrow=nrow(d_basic),ncol=nrow(d_basic))
  colnames(m)=d_basic$GroupID
  rownames(m)=d_basic$GroupID

  #fill matrix
  for(i in 1:nrow(d))
  {
    pred_ix=which(colnames(m)==ids$EcopathGroupID[ids$EcosimGroupID==d$PredID[i]])
    prey_ix=which(colnames(m)==ids$EcopathGroupID[ids$EcosimGroupID==d$PreyID[i]])
    m[prey_ix,pred_ix]=d$vulnerability[i]
  }
  m=apply(m, 2, as.numeric)     #make numeric
  #rename with group names
  colnames(m)=d_basic$GroupName
  rownames(m)=d_basic$GroupName
  m[m<0]=NA
  m[,colSums(m,na.rm=T)>0]
}

#' Read Ecosim time series from XML
#' @param xmldoc XML2 document
#' @param d_basic Data frame with basic estimates created with get_basic_estimates()
#' @param d_fleets Data frame with fleet information created with get_fleets()
#' @returns A list with two elements: (1) A list of time series used as drivers (i.e., for forcing) and (2) a list of time series used as references (i.e., for fitting). These time series lists are instances of the class EcosimTSList. Each time series contains, among other information, values, times, target group or fleet, type (e.g., absolute biomass).
#' @noRd
get_time_series=function(xmldoc, d_basic,d_fleets)
{
  tabs=get_tables_from_name(xmldoc,c("EcosimTimeSeriesDataset","EcosimTimeSeries","EcosimTimeSeriesFleet","EcosimTimeSeriesGroup"))
  df_data=table_to_df(tabs$EcosimTimeSeriesDataset)
  if(!is.null(df_data)) {
    if(nrow(df_data)>1) stop("XML file refers to more than one set of time series. Please provide a
                                copy of the model with at most one set of time series (time series sets can be deleted in the EwE software GUI).")
    years=as.numeric(df_data$FirstYear[1]):(as.numeric(df_data$FirstYear[1])+as.numeric(df_data$NumPoints[1])-1)
    #extract each time series into an object (list) with type, target group ID and name, target fleet ID and name, years, and values
    df_ts=table_to_df(tabs$EcosimTimeSeries)
    df_fleets=table_to_df(tabs$EcosimTimeSeriesFleet)
    df_groups=table_to_df(tabs$EcosimTimeSeriesGroup)

    #create list
    ts_list=list() #will contain the time series for forcing and reference
    ts_list$drivers=list(); class(ts_list$drivers)="EcosimTSList"
    ts_list$references=list(); class(ts_list$references)="EcosimTSList"
    for(i in 1:nrow(df_ts)) {
      ts=list()
      class(ts)="EcosimTS"
      ts$tsid=df_ts$TimeSeriesID[i]
      ts$tsname=df_ts$DatName[i]
      ts$tstype=lut_tscode(df_ts$DatType[i])
      ts$tstype=ts$tstype[colnames(ts$tstype) %in% c("TimeSeriesType","Type","Unit","DriverOrReference")]
      ts$time=years
      ts$values=as.numeric(unlist(strsplit(df_ts$TimeValues[i],split=" ")))
      #set target group
      ix_group=which(df_groups$TimeSeriesID==ts$tsid)
      if(length(ix_group)>0) {
        ts$groupID=as.numeric(df_groups$GroupID[ix_group])
        ts$groupname=d_basic$GroupName[d_basic$GroupID==ts$groupID]
      } else {ts$groupID=NA; ts$groupname=NA}
      #set target fleet
      ix_fleet=which(df_fleets$TimeSeriesID==ts$tsid)
      if(length(ix_fleet)>0) {
        ts$fleetID=as.numeric(df_fleets$FleetID[ix_fleet])
        ts$fleetname=d_fleets$FleetName[d_fleets$FleetID==ts$fleetID]
      } else {ts$fleetID=NULL; ts$fleetname=NULL}
      #add to the drivers or references list, accordingly
      if(ts$tstype$DriverOrReference=="Driver") {
        ts_list$drivers[[gsub(" ", "",ts$tsname)]]<-ts} else {
          ts_list$references[[gsub(" ", "",ts$tsname)]]<-ts}
    }
  }
  ts_list
}
