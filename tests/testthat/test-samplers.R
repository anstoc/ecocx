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

test_that("Automated factor creation for elemntary effects method works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  factor_set=new_ecosim_factor_set(m)

  factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,length(factor_set$forcing_functions$PPanomaly$default$values)))


  #obtain default scalar values as basis for range table, only modify fishing effort and temperature, keep PPAnomaly as yes/no
  range_table=get_factor_scalar_values(factor_set)
  range_table=range_table[c(4:8,11),]

  range_table$start=c(1,1,1,1,1,16.5)
  range_table$min=c(0,1,0.8,0.8,0.8,16.5)
  range_table$max=c(1,3.6,1.2,1.2,1.2,21.5)
  range_table$p=rep(4,nrow(range_table))

  factor_set_ee=create_ee_levels(factor_set,range_table,200,350)

  expect_true(identical(factor_set_ee$fishing_effort$Sealers$ee0$values[199],
                   factor_set_ee$fishing_effort$Sealers$ee0.33$values[199],
                   factor_set_ee$fishing_effort$Sealers$ee0.67$values[199],
                   factor_set_ee$fishing_effort$Sealers$ee1$values[199]))
  expect_false(identical(factor_set_ee$fishing_effort$Sealers$ee0$values[201],
                        factor_set_ee$fishing_effort$Sealers$ee0.33$values[201],
                        factor_set_ee$fishing_effort$Sealers$ee0.67$values[201],
                        factor_set_ee$fishing_effort$Sealers$ee1$values[201]))

  h=get_factor_scalar_values(factor_set_ee)
  expect_equal(max(h$factor_value,na.rm=T),1)
  expect_equal(min(h$factor_value,na.rm=T),0)

  expect_length(unique(h$factor_value[h$name=="Tbottom"]),4)
  expect_length(unique(h$factor_value[h$name=="PPanomaly"]),2)

})
