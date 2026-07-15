test_that("Random sampler works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_effort(factor_list,"Baitboats","phase_out",rep(0,get_ecosim_effort_length(factor_list,"Baitboats")))
  df_sample=sampler_random(factor_list, 8)
  expect_length(unique(df_sample$Trawlers),1)
  expect_true(length(unique(df_sample$Baitboats))<=2)
  expect_equal(nrow(df_sample),8)
  expect_equal(ncol(df_sample),4+nrow(summary(factor_list)))
  expect_all_true(summary(factor_list)$name %in% colnames(df_sample))
})

test_that("Full factorial sampler works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_set=new_ecosim_factor_set(m)
  factor_set=add_option_ecosim_effort(factor_set,"Baitboats","phase_out",rep(0,get_ecosim_effort_length(factor_set,"Baitboats")))
  factor_set=ecocx::add_option_ecosim_forcing(factor_set,"Tbottom","plus1deg",factor_set$forcing_functions$Tbottom$default$values+1)
  factor_set=ecocx::add_option_ecosim_forcing(factor_set,"Tbottom","min1deg",factor_set$forcing_functions$Tbottom$default$values-1)

  df_sample=sampler_full_factorial(factor_set)
  expect_equal(nrow(df_sample),2*3)
  expect_equal(as.numeric(summary(as.factor(df_sample$Sealers))),6)
  expect_equal(as.numeric(summary(as.factor(df_sample$Baitboats))),c(3,3))
  expect_equal(as.numeric(summary(as.factor(df_sample$Tbottom))),c(2,2,2))

})
