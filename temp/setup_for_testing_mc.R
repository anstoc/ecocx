ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.27/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)

factor_set=add_option_ecosim_forcing(factor_set,"Tbottom","warmer_2deg",factor_set$forcing_functions$Tbottom$default$values+2,factor_value=3)

factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))

factor_set=add_option_ecosim_effort(factor_set,"Sealers","higher20p",1.2*factor_set$fishing_effort$Sealers$default$values,factor_value=1.2)
factor_set=add_option_ecosim_effort(factor_set,"Sealers","lower20p",0.8*factor_set$fishing_effort$Sealers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Trawlers","higher20p",1.2*factor_set$fishing_effort$Trawlers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Trawlers","lower20p",0.8*factor_set$fishing_effort$Trawlers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Seiners","higher20p",1.2*factor_set$fishing_effort$Seiners$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Seiners","lower20p",0.8*factor_set$fishing_effort$Seiners$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Baitboats","higher20p",1.2*factor_set$fishing_effort$Baitboats$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Baitboats","lower20p",0.8*factor_set$fishing_effort$Baitboats$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","higher20p",1.2*factor_set$fishing_effort$Shrimpers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","lower20p",0.8*factor_set$fishing_effort$Shrimpers$default$values)

design=sampler_random(factor_set,size=20)

out_folder=paste0(tempdir(),"/mctest")


cx_table=run_ecosim_experiment(design,xml_model,factor_set,ewe_link,out_folder,parallel=T)

df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)

plot_all_runs(df_cx,alpha=0.02)

out_folder=paste0(tempdir(),"/fftest")
design_ff=sampler_full_factorial(factor_set)
library(future.apply)
plan(multisession)
cx_table=run_ecosim_experiment(design_ff,xml_model,factor_set,ewe_link,out_folder,parallel=T)
df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)


