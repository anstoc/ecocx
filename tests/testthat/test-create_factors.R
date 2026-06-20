test_that("Creating a default factor set from a model works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  expect_equal(factor_list$fishing_effort$Trawlers$default, m$ecosim$fishing_effort$Trawlers)
  expect_equal(factor_list$forcing_functions$Tbottom$default, m$ecosim$forcing_functions$Tbottom)
})
