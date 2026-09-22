test_that("Creating an Ecospace factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml"),ecospace_scenario="BayOfAnchovies")
  factor_set=new_ecospace_factor_set(m)
  s=summary(factor_set)
  expect_equal(unique(s$levels),1)
  expect_equal(sum(s$type=="mpas"),2)
  expect_equal(sum(s$type=="habitats"),4)
  expect_equal(s$name[s$type=="env_maps"],c("Temperature","Distance from coast"))
})
