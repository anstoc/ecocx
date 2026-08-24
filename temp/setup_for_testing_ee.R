ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.27/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)

factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,length(factor_set$forcing_functions$PPanomaly$default$values)))

summary(factor_set)

#obtain default scalar values as basis for range table, only modify fishing effort and temperature, keep PPAnomaly as yes/no
range_table=get_factor_scalar_values(factor_set)
range_table=range_table[c(4:8,11),]

range_table$start=c(1,1,1,1,1,16.5)
range_table$min=c(0,1,0.8,0.8,0.8,16.5)
range_table$max=c(1,3.6,1.2,1.2,1.2,21.5)
range_table$p=rep(4,nrow(range_table))

factor_set_ee=create_ee_levels(factor_set,range_table,200,350)

#generate design table
design_ee=sampler_ee(factor_set_ee,20)

#execute design
out_folder=paste0(tempdir(),"/eetest")

cx_table=run_ecosim_experiment(design_ee,xml_model,factor_set_ee,ewe_link,out_folder,parallel=T)

df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)

plot_all_runs(df_cx,alpha=0.02)

#TODO
#Calculate elementary effects
#Tutorial
#Release
#Porsangerfjord case study
#Draft paper
