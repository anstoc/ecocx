test_that("Loading Ecospace scenarios works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  expect_equal(scenarios$ScenarioName,c("BayOfAnchovies", "New Ecospace scenario"))
  expect_equal(scenarios$ScenarioID,c(1,2))
})
