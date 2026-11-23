ewe_link=connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.61/EwERunConsole.exe")

xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchbay_ecospace.eiixml")
#xml_model="C:\\Users\\ANC\\OneDrive - NIVA\\Projects\\2025\\2025CLIMAX\\WP1\\Test_psfj_model\\final_model_sustainability.eiixml"
m=load_model_from_xml(xmlfile,ecospace_scenario="BayOfAnchovies")
factor_set=new_ecospace_factor_set(m)
summary(factor_set)

#sample
out_folder=paste0(tempdir(),"/ecospacetest")
design_mc=sampler_random(factor_set,5)

cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)

df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
#View(df_cx_mc)

plot_ecosim_all_runs(df_cx_mc)



### Try changing fishing effort
#Fishing fleets
#Sealers
factor_set$fishing_effort$Sealers$default$values=rep(1,length(factor_set$fishing_effort$Sealers$default$values))
factor_set=add_level_ecosim_effort(factor_set,"Sealers","higher20p",change_values_mult(factor_set$fishing_effort$Sealers$default$values,1.2,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Sealers","lower20p",change_values_mult(factor_set$fishing_effort$Sealers$default$values,0.8,150,350))
plot_ecosim_factor_levels(factor_set,"Sealers")

#Trawlers
factor_set$fishing_effort$Trawlers$default$values=rep(1,length(factor_set$fishing_effort$Trawlers$default$values))
factor_set=add_level_ecosim_effort(factor_set,"Trawlers","higher20p",change_values_mult(factor_set$fishing_effort$Trawlers$default$values,1.2,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Trawlers","lower20p",change_values_mult(factor_set$fishing_effort$Trawlers$default$values,0.8,150,350))

#Seiners
factor_set$fishing_effort$Seiners$default$values=rep(1,length(factor_set$fishing_effort$Seiners$default$values))
factor_set=add_level_ecosim_effort(factor_set,"Seiners","higher20p",change_values_mult(factor_set$fishing_effort$Seiners$default$values,1.2,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Seiners","lower20p",change_values_mult(factor_set$fishing_effort$Seiners$default$values,0.8,150,350))

#Baitboats
factor_set$fishing_effort$Baitboats$default$values=rep(1,length(factor_set$fishing_effort$Baitboats$default$values))
factor_set=add_level_ecosim_effort(factor_set,"Baitboats","higher20p",change_values_mult(factor_set$fishing_effort$Baitboats$default$values,1.2,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Baitboats","lower20p",change_values_mult(factor_set$fishing_effort$Baitboats$default$values,0.8,150,350))

#Shrimpers
factor_set$fishing_effort$Shrimpers$default$values=rep(1,length(factor_set$fishing_effort$Shrimpers$default$values))
factor_set=add_level_ecosim_effort(factor_set,"Shrimpers","higher20p",change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,1.2,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Shrimpers","lower20p",change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,0.8,150,350))

#run with changing effort
design_mc=sampler_random(factor_set,10)
cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)
df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
plot_ecosim_all_runs(df_cx_mc)


### Try changing MPAs ###

factor_set=new_ecospace_factor_set(m)
summary(factor_set)

## add MPAs
h=factor_set$mpas$`MPA 1`$default$values*0
factor_set=add_level_ecospace_mpamap(factor_set,"MPA 1","off",h)
factor_set=add_level_ecospace_mpamap(factor_set,"MPA 2","off",h)

#sample
out_folder=paste0(tempdir(),"/ecospacetest")
design_mc=sampler_random(factor_set,5)

library(future.apply)
plan(multisession)

cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=T)

df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
#View(df_cx_mc)

plot_ecosim_all_runs(df_cx_mc)


### Change temperature: 2 degrees warmer
factor_set=new_ecospace_factor_set(m)
h=factor_set$env_maps$Temperature$default$values+2
factor_set=add_level_ecospace_envmap(factor_set,"Temperature","plus2",h)
design_mc=sampler_random(factor_set,5)
cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)
df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
plot_ecosim_all_runs(df_cx_mc)

#remove habitat: rocky
factor_set=new_ecospace_factor_set(m)
h=factor_set$habitats$Rocky$default$values*0.1
factor_set=add_level_ecospace_habmap(factor_set,"Rocky","loss90",h)
design_mc=sampler_random(factor_set,5)
cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)
df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
plot_ecosim_all_runs(df_cx_mc)

# change vulnerabilities
h=factor_set$tables$vulnerability$default*0+2
factor_set=add_level_ecosim_vulnerability(factor_set,"twos",h)
design_mc=sampler_random(factor_set,5)
cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)
df_cx_mc=get_ecospace_cx_biomass(cx_table_mc, m,relative=T)
plot_ecosim_all_runs(df_cx_mc)
