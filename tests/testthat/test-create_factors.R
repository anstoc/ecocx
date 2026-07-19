test_that("Creating a factor set from a model works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  expect_equal(factor_list$fishing_effort$Trawlers$default$values, m$ecosim$fishing_effort$Trawlers$values)
  expect_equal(factor_list$forcing_functions$Tbottom$default$values, m$ecosim$forcing_functions$Tbottom$values)
})

test_that("Obtaining the effort time series length from a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  expect_equal(length(factor_list$fishing_effort$Seiners$default$values), get_ecosim_effort_length(factor_list,"Seiners"))
})

test_that("Adding a new fishing effort option to a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_effort(factor_list,"Seiners","test_option",rep(0.5,get_ecosim_effort_length(factor_list, "Seiners")))
  expect_true("test_option" %in% names(factor_list$fishing_effort$Seiners))
  expect_length(names(factor_list$fishing_effort$Seiners),2)
  expect_equal(factor_list$fishing_effort$Seiners$test_option$values[10],0.5)
})

test_that("Removing a  fishing effort option from a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_effort(factor_list,"Seiners","test_option",rep(0.5,get_ecosim_effort_length(factor_list, "Seiners")))
  factor_list=remove_option_ecosim_effort(factor_list,"Seiners","default")
  expect_equal(names(factor_list$fishing_effort$Seiners),"test_option")
})

test_that("Obtaining the forcing function length from a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  expect_equal(length(factor_list$forcing_functions$PPanomaly$default$values), get_ecosim_forcing_length(factor_list,"PPanomaly"))
})

test_that("Adding a new forcing function option to a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_forcing(factor_list,"Tbottom","test_option",rep(0.99,get_ecosim_forcing_length(factor_list, "Tbottom")))
  expect_true("test_option" %in% names(factor_list$forcing_functions$Tbottom))
  expect_length(names(factor_list$forcing_functions$Tbottom),2)
  expect_equal(factor_list$forcing_functions$Tbottom$test_option$values[c(10,100)],c(0.99,0.99))
})

test_that("Removing a  forcing function option from a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_forcing(factor_list,"Tbottom","test_option",rep(0.99,get_ecosim_forcing_length(factor_list, "Tbottom")))
  factor_list=remove_option_ecosim_forcing(factor_list,"Tbottom","default")
  expect_equal(names(factor_list$forcing_functions$Tbottom),"test_option")
})

test_that("Adding a new shape option to a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_shape(factor_list,"Twhiting","test_option",shape_x=1:1200,shape_y=rep(0.66,1200))
  expect_true("test_option" %in% names(factor_list$shapes$Twhiting))
  expect_length(names(factor_list$shapes$Twhiting),2)
  expect_equal(factor_list$shapes$Twhiting$test_option$xmax,1200)
  expect_equal(factor_list$shapes$Twhiting$test_option$y[1000],0.66)
})

test_that("Adding and removing a new vulnerability matrix option to a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  m1=factor_list$tables$vulnerability$default
  m1[!is.na(m1)]=2
  factor_list=add_option_ecosim_vulnerability(factor_list,"twos",m1)
  expect_equal(factor_list$tables$vulnerability$twos,m1)
  #remove the default option
  factor_list=remove_option_ecosim_vulnerability(factor_list,"default")
  expect_equal(names(factor_list$tables$vulnerability),"twos")
})

test_that("Summary function for a factor set works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_list=new_ecosim_factor_set(m)
  factor_list=add_option_ecosim_effort(factor_list,"Baitboats","phase_out",rep(0,get_ecosim_effort_length(factor_list,"Baitboats")))
  h=summary(factor_list)

  expect_equal(h$options[h$name=="Tempcold"],1)
  expect_equal(h$options[h$name=="Baitboats"],2)
})

test_that("Setting and reading scalar values for each factor works", {
  m=load_model_from_xml(paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml"))
  factor_set=new_ecosim_factor_set(m)
  factor_set=add_option_ecosim_effort(factor_set,"Baitboats","phase_out",rep(0,get_ecosim_effort_length(factor_list,"Baitboats")),factor_value=0.01)
  h=get_factor_scalar_values(factor_set)
  expect_equal(h$factor_value[h$level=="phase_out"],0.01)

  h$factor_value=1:nrow(h)
  factor_set=set_factor_scalar_values(factor_set,h)
  expect_equal(factor_set$fishing_effort$Baitboats$default$factor_value,7)
  expect_equal(factor_set$fishing_effort$Baitboats$phase_out$factor_value,8)
})
