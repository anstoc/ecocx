test_that("Opening XML document works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  expect_equal(class(m),c("xml_document","xml_node"))
  expect_identical(names(xml2::as_list(m)),"EwEModel")
})

test_that("Finding tables by name in XML file works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,c("EcopathGroup","EcopathFleet"))
  expect_length(tablist,2)
  expect_identical(class(tablist[[2]]),"xml_node")
})

test_that("Extracting column names from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,"EcopathGroup")
  col_names=get_colnames(tablist[[1]])
  expect_length(col_names,43)
  expect_identical(col_names[6],"Biomass")
  expect_identical(col_names[43],"Winf")
})

test_that("Reading rows from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,"EcopathGroup")
  rows=xml2::xml_find_all(tablist[[1]],".//Row")
  row=row_to_vector(rows[1])
  expect_length(row,43)
  expect_equal(as.numeric(row[15]),742.149)
})

test_that("Reading data frames from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  tablist=get_tables_from_name(m,c("EcopathGroup","EcopathCatchCode"))
  df=table_to_df(tablist$EcopathGroup)
  expect_equal(dim(df),c(11,43))
  expect_true("Biomass" %in% colnames(df))
  expect_no_error(as.numeric(df$Biomass))
  expect_equal(as.numeric(df$Biomass[10]),14.8)
  expect_null(table_to_df(tablist$EcopathCatchCode))
})

test_that("Reading basic estimates from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d=get_basic_estimates(m)
  expect_all_true(c("GroupID", "Sequence","GroupName", "Biomass", "PoB", "QoB", "EE") %in% colnames(d))
  expect_true(is.na(d$EE[2]))
  expect_equal(d$QoB[d$GroupName=="Mackerel"],4.4)
})

test_that("Reading the diet matrix from XML  works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  b=get_basic_estimates(m)
  d=get_diet_matrix(m,b)
  expect_equal(nrow(d),length(unique(b$GroupID)))
  expect_true(ncol(d)>0 & ncol(d)<nrow(d)) #excluding producers and detritus in columns
  expect_all_true(abs(colSums(d)-1)<0.001) #should be approximately 1
})

test_that("Reading the fleet IDs and names from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d=get_fleets(m)
  expect_equal(d$FleetName[3],"Seiners")
})

test_that("Reading the discard and landings tables from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_fleets=get_fleets(m)
  d_basic=get_basic_estimates(m)
  l=get_catches(m,d_fleets,d_basic)
  expect_equal(l$landings[3,2],0.45)
  expect_all_equal(as.numeric(l$discards),0)
})

