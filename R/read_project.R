#' Open XML file describing an Ecosim or Ecospace model in the format exported with the EwE software.
#' @param in_file Path to .xml file.
#' @returns XML2 object.
#' @NoRd
load_model_from_xml=function(in_file)
{
  xml2::read_xml(in_file)
}

#' Get tables in an opened XML file that have one of the provided names according to their attribute 'Name'.
#' @param xmldoc XML2 object pointing to a node.
#' @param tablenames description
#' @returns List containing XML2 objects pointing to the nodes that are tables and have one of the names provided.
#' @NoRd
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
#' @NoRd
get_colnames=function(tab) {
  str_col_names=xml2::xml_attr(tab,"Columns")
  col_names=unlist(strsplit(str_col_names, split=',',fixed=T))   #now a vector of strings
  unlist(lapply(col_names,FUN=function(x) {strsplit(x,split=':',fixed=T)[[1]][1]}))
}


#' Create character vector from row node containing comma-separated text
#' @param row XML node representing a row
#' @returns Character vector with comma-separated values.
#' @NoRd
row_to_vector=function(row) {
  str_row=xml2::xml_text(row)
  unlist(strsplit(str_row, split=',',fixed=T))
}

#' Create a dataframe from an XML table node.
#' @param tab XML table node.
#' @returns The table as a dataframe.
#' @NoRd
table_to_df=function(tab) {
  rows=xml2::xml_find_all(tab,".//Row")   #get all rows
  col_names=get_colnames(tab)
  df=as.data.frame(matrix("",nrow=length(rows),ncol=length(col_names)))
  colnames(df)=col_names
  for(i in 1:length(rows)) {df[i,]=row_to_vector(rows[i])}
  df
}

#' Extracts the basic estimates table from the exported XML file of an Ecosim or Ecospace model
#' @param xmldoc XML2 document
#' @returns A data frame containing the basic estimates for the model.
#' @NoRd
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
