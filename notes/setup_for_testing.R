ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.23/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)

factor_set=add_option_ecosim_forcing(factor_set,"Tbottom","warmer_2deg",factor_set$forcing_functions$Tbottom$default$values+2)

factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))

factor_set=add_option_ecosim_effort(factor_set,"Sealers","higher10p",1.1*factor_set$fishing_effort$Sealers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Sealers","lower10p",0.9*factor_set$fishing_effort$Sealers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Trawlers","higher10p",1.1*factor_set$fishing_effort$Trawlers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Trawlers","lower10p",0.9*factor_set$fishing_effort$Trawlers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Seiners","higher10p",1.1*factor_set$fishing_effort$Seiners$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Seiners","lower10p",0.9*factor_set$fishing_effort$Seiners$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Baitboats","higher10p",1.1*factor_set$fishing_effort$Baitboats$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Baitboats","lower10p",0.9*factor_set$fishing_effort$Baitboats$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","higher10p",1.1*factor_set$fishing_effort$Shrimpers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","lower10p",0.9*factor_set$fishing_effort$Shrimpers$default$values)

design=sampler_random(factor_set,size=50)

out_folder=paste0(tempdir(),"/jsontest")

cx_table=data.frame("run_name"=design$run_name,"model"=xml_model,"folder"=paste0(out_folder,"/",design$run_name),"json"=paste0(paste0(out_folder,"/",design$run_name),"/",design$run_name,".json"))
