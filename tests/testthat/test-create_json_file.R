test_that("Creating change vector for .json file works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  factor_set=new_ecosim_factor_set(m)
  design=sampler_random(factor_set,size=1)
  out_folder=paste0(tempdir(),"/jsontest")

  v_json=create_json_changes_ecosim(1,design,factor_set)

  expect_length(v_json,21)
  expect_true(startsWith(trimws(v_json[5]),"\"ecosim.effort[1].set\": [ 1"))

})

test_that("Creating Ecosim run vector for .json file works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  factor_set=new_ecosim_factor_set(m)
  design=sampler_random(factor_set,size=1)
  out_folder=paste0(tempdir(),"/jsontest")
  cx_table=data.frame("run_name"=design$run_name,"model"=xml_model,"folder"=paste0(out_folder,"/",design$run_name),"json"=paste0(paste0(out_folder,"/",design$run_name),"/",design$run_name,".json"))

  v_json=create_json_ecosim_run(1,cx_table, design,factor_set)


  expect_length(v_json,25)
  expect_true(endsWith(v_json[length(v_json)],"}"))
  expect_true(startsWith(trimws(v_json[10]),"\"ecosim.effort[3].set\": [ 1"))

})

test_that("Creating the .json configuration vector works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")

  v_json=create_json_configuration(xml_model)

  expect_true(startsWith(trimws(v_json[2]),"\"Configuration\""))
  expect_true(endsWith(trimws(v_json[length(v_json)]),"},"))

})

test_that("Writing the .json vector to a file works", {
  xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
  m=load_model_from_xml(xml_model)
  factor_set=new_ecosim_factor_set(m)
  design=sampler_random(factor_set,size=1)
  out_folder=paste0(tempdir(),"/jsontest")
  cx_table=data.frame("run_name"=design$run_name,"model"=xml_model,"folder"=paste0(out_folder,"/",design$run_name),"json"=paste0(paste0(out_folder,"/",design$run_name),"/",design$run_name,".json"))

  if(!file.exists(out_folder)) {dir.create(out_folder)}
  if(!file.exists(cx_table$folder[1])) {dir.create(cx_table$folder[1])}

  write_json_file(1,cx_table,design,factor_set)

  expect_true(file.exists(cx_table$json))

})



create_json_configuration
