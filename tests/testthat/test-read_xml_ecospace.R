test_that("Loading Ecospace scenarios works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  expect_equal(scenarios$ScenarioName,c("BayOfAnchovies", "New Ecospace scenario"))
  expect_equal(scenarios$ScenarioID,c(1,2))
})

test_that("Loading Ecospace depth map works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  depth_map=get_ecospace_depthmap(xmldoc,scenarios$ScenarioName[1])
  expect_equal(dim(depth_map$values),c(20,20))
  expect_equal(depth_map$values[12,12],83)
})

test_that("Loading Ecospace base map works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  base_map=get_ecospace_basemap(xmldoc,scenarios$ScenarioName[1])
  expect_equal(dim(base_map$values),c(20,20))
  expect_equal(sum(!is.na(base_map$values[8,])),17)
})

test_that("Loading Ecospace environemntal maps works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  envmaps=get_ecospace_envmaps(xmldoc,scenarios$ScenarioName[1])
  expect_equal(names(envmaps),c("Temperature","Distance from coast"))
  expect_equal(envmaps$`Distance from coast`$values[8,6],80.04280)
})

test_that("Loading Ecospace habitat maps works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  maps=get_ecospace_habmaps(xmldoc,scenarios$ScenarioName[1])
  expect_equal(names(maps),c("All","Coastal","Sand","Rocky","Deep"))
  expect_equal(round(maps$Rocky$values[12,15],4),0.4444)
})

test_that("Loading Ecospace MPA maps works", {
  xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
  xmldoc=read_eiixml(xmlfile)
  scenarios=get_ecospace_scenarios(xmldoc)
  maps=get_ecospace_mpamaps(xmldoc,scenarios$ScenarioName[1])
  expect_equal(dim(maps[[1]]$values),c(20,20))
  expect_equal(rowSums(maps[[2]]$values[10:13,],na.rm=T),c(0,2,3,3))
})
