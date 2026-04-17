test_that("XML reading works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  expect_equal(class(m),c("xml_document","xml_node"))
  expect_identical(names(xml2::as_list(m)),"EwEModel")
})

test_that("Finding XML tables by name works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,c("EcopathGroup","EcopathFleet"))
  expect_length(tablist,2)
  expect_identical(class(tablist[[2]]),"xml_node")
})

test_that("Column names are extracted correctly from XML table", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,"EcopathGroup")
  col_names=get_colnames(tablist[[1]])
  expect_length(col_names,43)
  expect_identical(col_names[6],"Biomass")
  expect_identical(col_names[43],"Winf")
})

test_that("Row is read from XML correctly correctly", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,"EcopathGroup")
  rows=xml2::xml_find_all(tablist[[1]],".//Row")
  row=row_to_vector(rows[1])
  expect_length(row,43)
  expect_equal(as.numeric(row[15]),742.149)
})

test_that("Data frame is read from XML table correctly", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,"EcopathGroup")
  df=table_to_df(tablist[[1]])
  expect_equal(dim(df),c(11,43))
  expect_true("Biomass" %in% colnames(df))
  expect_no_error(as.numeric(df$Biomass))
  expect_equal(as.numeric(df$Biomass[10]),14.8)
})

test_that("Basic estimates are read from XML file correctly", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/Anchovy Bay_Ecosim_ex.eiixml"))
  d=get_basic_estimates(m)
  expect_all_true(c("GroupID", "Sequence","GroupName", "Biomass", "PoB", "QoB", "EE") %in% colnames(d))
  expect_true(is.na(d$EE[2]))
  expect_equal(d$QoB[d$GroupName=="Mackerel"],4.4)
})
