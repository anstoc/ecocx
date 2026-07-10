test_that("Reading Ecosim output, biomasses and catch, from a single run works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  folder=paste0(system.file('extdata', package = 'ecocx'),"/ex_outputs/R0001_0000")
  df_bio=get_ecosim_output_biomass(folder,m,groups=c("Seals","Cod"))

  expect_equal(ncol(df_bio),3)
  expect_equal(nrow(df_bio),length(m$ecosim$fishing_effort[[1]]$values))
  expect_lte(abs(df_bio$Seals[10]-0.056786165),0.0001)

  df_catch=get_ecosim_output_catch(folder,m)
  expect_all_true(unique(m$ecopath$basic_estimates$GroupName) %in% colnames(df_catch))
  expect_lte(abs(df_catch$Anchovy[15]-0.50848126),0.0001)



})
