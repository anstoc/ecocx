test_that("Reading Ecosim output, biomasses and catch, from a single run works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  folder=paste0(system.file('extdata', package = 'ecocx'),"/ex_outputs/R0001_0000")
  df_bio=get_ecosim_run_biomass(folder,m,groups=c("Seals","Cod"))

  expect_equal(ncol(df_bio),3)
  expect_equal(nrow(df_bio),length(m$ecosim$fishing_effort[[1]]$values))
  expect_lte(abs(df_bio$Seals[10]-0.056786165),0.000001)

  df_catch=get_ecosim_run_catch(folder,m)
  expect_all_true(unique(m$ecopath$basic_estimates$GroupName) %in% colnames(df_catch))
  expect_lte(abs(df_catch$Anchovy[15]-0.50848126),0.000001)

})

test_that("Reading Ecosim output, biomasses and catch, from a computational experiment works (all runs)", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  folder1=paste0(system.file('extdata', package = 'ecocx'),"/ex_outputs/R0001_0000")
  folder2=paste0(system.file('extdata', package = 'ecocx'),"/ex_outputs/R0002_0000")
  cx_table=data.frame("run_name"=c("R0001_0000","R0002_0000"),"folder"=c(folder1,folder2))

  df_bio=get_ecosim_cx_biomass(cx_table,m,groups=c("Seals","Cod"))
  expect_equal(ncol(df_bio),4)
  expect_equal(nrow(df_bio),2*length(m$ecosim$fishing_effort$Shrimpers$values))
  expect_lte(abs(df_bio$Seals[df_bio$timestep==492 & df_bio$run_name=="R0002_0000"]-0.09143834),0.000001)

  df_catch=get_ecosim_cx_catch(cx_table,m)
  expect_equal(ncol(df_catch),2+nrow(m$ecopath$basic_estimates))
  expect_equal(nrow(df_catch),2*length(m$ecosim$fishing_effort$Shrimpers$values))
  expect_lte(abs(df_catch$Cod[df_catch$timestep==490 & df_catch$run_name=="R0001_0000"]-0.13524292),0.000001)

})
