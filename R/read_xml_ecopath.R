#' Open XML file describing an Ecosim or Ecospace model in the format exported with the EwE software.
#' @param in_file Path to .xml file.
#' @returns XML2 object.
#' @noRd
load_model_from_xml=function(in_file)
{
  xml2::read_xml(in_file)
}

#' Get tables in an opened XML file that have one of the provided names according to their attribute 'Name'.
#' @param xmldoc XML2 object pointing to a node.
#' @param tablenames description
#' @returns List containing XML2 objects pointing to the nodes that are tables and have one of the names provided.
#' @noRd
get_tables_from_name=function(xmldoc,tablenames) {
  table_list=list()
  tables=xml2::xml_find_all(xmldoc,"//Table")
  for(tab in tables) {
    tab_name=xml2::xml_attr(tab,"Name")
    if(tab_name %in% tablenames) {table_list[[tab_name]]=tab}
  }
  table_list
}

#' Get column names provided as attribute to a table.
#' @param tab XML2 node representing a table
#' @returns Character vector with the table's column names.
#' @noRd
get_colnames=function(tab) {
  str_col_names=xml2::xml_attr(tab,"Columns")
  col_names=unlist(strsplit(str_col_names, split=',',fixed=T))   #now a vector of strings
  unlist(lapply(col_names,FUN=function(x) {strsplit(x,split=':',fixed=T)[[1]][1]}))
}


#' Create character vector from row node containing comma-separated text
#' @param row XML node representing a row
#' @returns Character vector with comma-separated values.
#' @noRd
row_to_vector=function(row) {
  str_row=xml2::xml_text(row)
  v=unlist(strsplit(str_row, split=',',fixed=T))
  sep_count=sum(unlist(strsplit(str_row,"",fixed=T))==",")  #count commas, then add an empty string at end which is omitted by strsplit if needed
  if(length(v)==sep_count) {v=c(v,"")}
  v
}

#' Create a dataframe from an XML table node.
#' @param tab XML table node.
#' @returns The table as a dataframe.
#' @noRd
table_to_df=function(tab) {
  rows=xml2::xml_find_all(tab,".//Row")   #get all rows
  if(length(rows)<1) {df=NULL} else {
    col_names=get_colnames(tab)
    df=as.data.frame(matrix("",nrow=length(rows),ncol=length(col_names)))
    colnames(df)=col_names
    for(i in 1:length(rows)) {df[i,]=row_to_vector(rows[i])} }
  df
}

#' Reads the basic estimates table from the exported XML file of an Ecosim or Ecospace model
#' @param xmldoc XML2 document
#' @returns A data frame containing the basic estimates for the model.
#' @noRd
get_basic_estimates=function(xmldoc)
{
  #Obtain data frame with basic estimates
  tab=get_tables_from_name(xmldoc,"EcopathGroup")[[1]]      #find table
  dh=table_to_df(tab)

  d_basic=data.frame("GroupID"=as.numeric(dh$GroupID),
                     "Sequence"=as.numeric(dh$Sequence),
                     "GroupName"=dh$GroupName,
                     "Biomass"=as.numeric(dh$Biomass),
                     "PoB"=as.numeric(dh$ProdBiom),
                     "QoB"=as.numeric(dh$ConsBiom),
                     "EE"=as.numeric(dh$EcoEfficiency))

  d_basic=d_basic[order(d_basic$Sequence),]

  d_basic$Biomass[d_basic$Biomass<0]=NA
  d_basic$PoB[d_basic$PoB<0]=NA
  d_basic$QoB[d_basic$QoB<0]=NA
  d_basic$EE[d_basic$EE<0]=NA

  d_basic
}

#' Reads the diet table from the exported XML file of an Ecosim or Ecospace model
#' @param xmldoc XML2 document
#' @param d_basic Data frame with basic estimates generated with get_basic_estimates()
#' @return Diet matrix as data frame.
#' @noRd
get_diet_matrix=function(xmldoc, d_basic)
{
  tab=get_tables_from_name(xmldoc, "EcopathDietComp")[[1]]
  df_diet=table_to_df(tab)

  #create matrix
  m=matrix(0,nrow=nrow(d_basic),ncol=nrow(d_basic))
  colnames(m)=d_basic$GroupID
  rownames(m)=d_basic$GroupID

  #fill matrix
  for(i in 1:nrow(df_diet))
  {
    pred_ix=which(colnames(m)==df_diet$PredID[i])
    prey_ix=which(colnames(m)==df_diet$PreyID[i])
    m[prey_ix,pred_ix]=df_diet$Diet[i]
  }
  m=apply(m, 2, as.numeric)     #make numeric
  #rename with group names
  colnames(m)=d_basic$GroupName
  rownames(m)=d_basic$GroupName

  m[,colSums(m)>0]
}

#' Reads fleet IDs and names from from the exported XML file of an Ecosim or Ecospace model
#' @param xmldoc XML2 document.
#' @return Data frame with fleet IDs and names.
#' @noRd
get_fleets=function(xmldoc)
{
  tab=get_tables_from_name(xmldoc,"EcopathFleet")[[1]]
  dh=table_to_df(tab)
  data.frame("FleetID"=as.numeric(dh$FleetID),
             "FleetName"=dh$FleetName)
}

#' Reads landings and discards from from the exported XML file of an Ecosim or Ecospace model
#' @param xmldoc XML2 document
#' @param d_fleets Data frame with fleet IDs and names, created with get_fleets().
#' @param d_basic  Data frame with basic estimates generated with get_basic_estimates(...)
#' @returns List with two elements (1) a data frame with landings and (2) a data frame with discards.
#' @noRd
get_catches=function(xmldoc, d_fleets, d_basic)
{
  tab=get_tables_from_name(xmldoc,"EcopathCatch")[[1]]
  d=table_to_df(tab)
  #create matrices for landings and discards
  m_landings=matrix(0,nrow=nrow(d_basic),ncol=nrow(d_fleets))
  colnames(m_landings)=d_fleets$FleetID
  rownames(m_landings)=d_basic$GroupID
  m_discards=m_landings
  #fill matrix
  for(i in 1:nrow(d))
  {
    fleet_ix=which(colnames(m_landings)==d$FleetID[i])
    group_ix=which(rownames(m_landings)==d$GroupID[i])
    m_discards[group_ix,fleet_ix]=d$Discards[i]
    m_landings[group_ix,fleet_ix]=d$Landing[i]
  }
  m_landings=apply(m_landings, 2, as.numeric)     #make numeric
  m_discards=apply(m_discards, 2, as.numeric)
  #rename with group and fleet names
  colnames(m_landings) <- colnames(m_discards) <- d_fleets$FleetName
  rownames(m_landings) <- rownames(m_discards) <- d_basic$GroupName


  list(landings=m_landings, discards=m_discards)
}


