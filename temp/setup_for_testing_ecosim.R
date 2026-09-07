ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.35/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)
summary(factor_set)

#Temperature +/- 1 degree
factor_set=add_option_ecosim_forcing(factor_set,"Tbottom","warmer_1deg",ecocx::change_values_add(factor_set$forcing_functions$Tbottom$default$values,1,150,350))
factor_set=add_option_ecosim_forcing(factor_set,"Tbottom","colder_1deg",ecocx::change_values_add(factor_set$forcing_functions$Tbottom$default$values,-1,150,350))
plot(factor_set$forcing_functions$Tbottom$default$values,type="l", ylim=c(15,21))
lines(factor_set$forcing_functions$Tbottom$warmer_1deg$values,col="red")
lines(factor_set$forcing_functions$Tbottom$colder_1deg$values,col="blue")

#Switch primary production anomaly on or off
factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))
plot(factor_set$forcing_functions$PPanomaly$default$values,type="l")
lines(factor_set$forcing_functions$PPanomaly$none$values,col="red")

#Fishing fleets
#Sealers
factor_set$fishing_effort$Sealers$default$values=rep(1,length(factor_set$fishing_effort$Sealers$default$values))
factor_set=add_option_ecosim_effort(factor_set,"Sealers","higher20p",change_values_mult(factor_set$fishing_effort$Sealers$default$values,1.2,150,350))
factor_set=add_option_ecosim_effort(factor_set,"Sealers","lower20p",change_values_mult(factor_set$fishing_effort$Sealers$default$values,0.8,150,350))
plot(factor_set$fishing_effort$Sealers$default$values,type="l", ylim=c(0.7,1.3))
lines(factor_set$fishing_effort$Sealers$higher20p$values,col="red")
lines(factor_set$fishing_effort$Sealers$lower20p$values,col="blue")

#Trawlers
factor_set$fishing_effort$Trawlers$default$values=rep(1,length(factor_set$fishing_effort$Trawlers$default$values))
factor_set=add_option_ecosim_effort(factor_set,"Trawlers","higher20p",change_values_mult(factor_set$fishing_effort$Trawlers$default$values,1.2,150,350))
factor_set=add_option_ecosim_effort(factor_set,"Trawlers","lower20p",change_values_mult(factor_set$fishing_effort$Trawlers$default$values,0.8,150,350))

#Seiners
factor_set$fishing_effort$Seiners$default$values=rep(1,length(factor_set$fishing_effort$Seiners$default$values))
factor_set=add_option_ecosim_effort(factor_set,"Seiners","higher20p",change_values_mult(factor_set$fishing_effort$Seiners$default$values,1.2,150,350))
factor_set=add_option_ecosim_effort(factor_set,"Seiners","lower20p",change_values_mult(factor_set$fishing_effort$Seiners$default$values,0.8,150,350))

#Baitboats
factor_set$fishing_effort$Baitboats$default$values=rep(1,length(factor_set$fishing_effort$Baitboats$default$values))
factor_set=add_option_ecosim_effort(factor_set,"Baitboats","higher20p",change_values_mult(factor_set$fishing_effort$Baitboats$default$values,1.2,150,350))
factor_set=add_option_ecosim_effort(factor_set,"Baitboats","lower20p",change_values_mult(factor_set$fishing_effort$Baitboats$default$values,0.8,150,350))

#Shrimpers
factor_set$fishing_effort$Shrimpers$default$values=rep(1,length(factor_set$fishing_effort$Shrimpers$default$values))
factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","higher20p",change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,1.2,150,350))
factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","lower20p",change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,0.8,150,350))

summary(factor_set)

#set up simple Monte Carlo
out_folder=paste0(tempdir(),"/mctest")

design_mc=sampler_random(factor_set, 50)
View(design_mc)

library(future.apply)
plan(multisession)

cx_table_mc=run_ecosim_experiment(design_mc,xml_model,factor_set,ewe_link,out_folder,parallel=T)
View(cx_table_mc)

df_cx_mc=get_ecosim_cx_biomass(cx_table_mc, m,relative=T)
View(df_cx_mc)

plot_all_runs(df_cx_mc,alpha=0.2)

#set up full factorial
design_ff=sampler_full_factorial(factor_set)
View(design_ff)
out_folder=paste0(tempdir(),"/fftest")

cx_table_ff=run_ecosim_experiment(design_ff,xml_model,factor_set,ewe_link,out_folder,parallel=T)


df_cx_ff=get_ecosim_cx_biomass(cx_table_ff, m,relative=T)
y_ff=df_cx_ff[df_cx_ff$timestep==max(df_cx_ff$timestep),]
df_ff=cbind(design_ff[order(design_ff$run_name),],y_ff[order(y_ff$run_name),])

barplot(apply(df_ff[,21:29],MARGIN = 2, FUN=var))

sensitivity_results=calculate_effects_ff("Shrimp",colnames(design_ff[,-(1:4)]),df_ff)

effect_matrix=rbind(main_effect=sensitivity_results$main_effect, interaction_effect=sensitivity_results$total_effect - sensitivity_results$main_effect)
colnames(effect_matrix) <- sensitivity_results$factor

barplot(
  effect_matrix,
  beside     = FALSE,                     # FALSE = stacked (default), TRUE would be side-by-side
  col        = c("steelblue", "orange"),
  legend.text = c("Main effect", "Interaction effect (total - main)"),
  args.legend = list(x = "topright", bty = "n"),
  ylab       = "Proportion of variance explained",
  xlab       = "Factor",
  main       = paste("Total effects (main+interaction) on",sensitivity_results$output[1]),
  las        = 2)                          # rotates factor labels if names are long


#test elementary effects
#obtain default scalar values as basis for range table, only modify fishing effort and temperature, keep PPAnomaly as yes/no
factor_set_ee=new_ecosim_factor_set(m)

factor_set_ee=add_option_ecosim_forcing(factor_set_ee,"PPanomaly","none",rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))

range_table=get_factor_scalar_values(factor_set_ee)
range_table=range_table[c(4:8,11),]

range_table$start=c(1,1,1,1,1,17.5)
range_table$min=c(0.8,0.8,0.8,0.8,0.8,16.5)
range_table$max=c(1.2,1.2,1.2,1.2,1.2,18.5)
range_table$p=rep(4,nrow(range_table))

factor_set_ee=create_ee_levels(factor_set_ee,range_table,200,350)

#generate design table
design_ee=sampler_ee(factor_set_ee,20)

#execute design
out_folder=paste0(tempdir(),"/eetest")

cx_table_ee=run_ecosim_experiment(design_ee,xml_model,factor_set_ee,ewe_link,out_folder,parallel=T)

df_cx_ee=get_ecosim_cx_biomass(cx_table_ee, m,relative=T)
#plot_all_runs(df_cx,alpha=0.1)

y_ee=df_cx_ee[df_cx_ee$timestep==max(df_cx_ee$timestep),]   #limit to the end of the run
df_ee=cbind(design_ee[order(design_ee$run_name),],y_ee[order(y_ee$run_name),])

results_ee=calculate_effects_ee("Shrimp",factor_set_ee,df_ee)

plot(results_ee$mu_star~results_ee$sigma,type="n",xlab="sigma",ylab="mu*")
text(labels=names(results_ee$mu_star),x=results_ee$sigma,y=results_ee$mu_star)

for(i in 1:ncol(results_ee$mustar_matrix))
{
  if(i==1) plot(results_ee$mustar_matrix[,i],col=1, type="l",ylim=c(min(results_ee$mustar_matrix),max(results_ee$mustar_matrix))) else {
    lines(results_ee$mustar_matrix[,i],col=i)
  }
}

