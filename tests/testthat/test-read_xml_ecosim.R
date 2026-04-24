test_that("Reading scenario information works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_scen=get_ecosim_scenarios(m)
  expect_equal(c(d_scen$ScenarioID[1],d_scen$ScenarioName[1]),c(2,"Scene 1"))
})

test_that("Reading Ecosim group IDs works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_ids=get_ecosim_groupIDs(m)
  expect_equal(d_ids$EcopathGroupID[d_ids$ScenarioID==2 & d_ids$EcosimGroupID==15],5)
})

test_that("Reading the vulnerability matrix from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_basic=get_basic_estimates(m)
  d_vuln=get_vulnerability_matrix(m, d_basic)
  expect_equal(dim(d_vuln),c(11,9))
  expect_true(is.na(d_vuln[rownames(d_vuln)=="Shrimp",colnames(d_vuln)=="Whales"]))
  expect_equal(round(d_vuln[rownames(d_vuln)=="Cod",colnames(d_vuln)=="Seals"],2),22.67)
  expect_equal(round(d_vuln[rownames(d_vuln)=="Detritus",colnames(d_vuln)=="Benthos"],3),1.137)
})

test_that("Reading Ecosim fleet IDs works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_ids=get_ecosim_fleetIDs(m)
  expect_equal(d_ids$EcopathFleetID[d_ids$ScenarioID==2 & d_ids$EcosimFleetID==8],3)
})

test_that("Reading Ecosim time series works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  d_basic=get_basic_estimates(m)
  d_fleets=get_fleets(m)
  ts=get_time_series(m,d_basic,d_fleets)
  expect_equal(length(ts$drivers),2)
  expect_equal(length(ts$references),4)
  expect_equal(c(ts$drivers[[2]]$time[36],ts$drivers[[2]]$values[30]),c(2005,3.08))
  expect_equal(ts$references$CodB$groupname,"Cod")
})

test_that("Reading Ecosim forcing functions works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  ff=get_forcing_functions(m)
  expect_equal(ff$PPanomaly$name,"PP anomaly")
  expect_equal(ff$Tbottom$id,33)
  expect_length(ff$PPanomaly$values,493)  #number of time steps
  expect_equal(ff$Tbottom$values[316],18.7)
})


