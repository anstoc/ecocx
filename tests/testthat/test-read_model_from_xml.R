test_that("Loading example model from XML works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  expect_equal(m$ecopath$basic_estimates$QoB[m$ecopath$basic_estimates$GroupName=="Cod"],2.58)
  expect_equal(m$ecopath$fleets$FleetName[2],"Trawlers")
  expect_equal(m$ecopath$dietmatrix[rownames(m$ecopath$dietmatrix)=="Benthos", colnames(m$ecopath$dietmatrix)=="Cod"],0.84)
  expect_equal(m$ecopath$catches$landings[rownames(m$ecopath$catches$landings)=="Anchovy",
                                          colnames(m$ecopath$catches$landings)=="Seiners"],1.2)
  expect_equal(m$ecopath$catches$discards[rownames(m$ecopath$catches$landings)=="Anchovy",
                                          colnames(m$ecopath$catches$discards)=="Seiners"],0)

  #WEITER:Ecosim
})
