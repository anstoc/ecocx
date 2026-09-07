ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.27/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)

#add options to "switch off" temperature responses
# factor_set=add_option_ecosim_shape(factor_set,"Tempwarm","off",
#                                    factor_set$shapes$Tempwarm$default$x,
#                                    rep(1,length(factor_set$shapes$Tempwarm$default$y)))
#
# factor_set=add_option_ecosim_shape(factor_set,"Tempcold","off",
#                                    factor_set$shapes$Tempcold$default$x,
#                                    rep(1,length(factor_set$shapes$Tempcold$default$y)))
#
# factor_set=add_option_ecosim_shape(factor_set,"Twhiting","off",
#                                    factor_set$shapes$Twhiting$default$x,
#                                    rep(1,length(factor_set$shapes$Twhiting$default$y)))

summary(factor_set)

#obtain default scalar values as basis for range table, only modify fishing effort and temperature, keep PPAnomaly as yes/no
range_table=get_factor_scalar_values(factor_set)
range_table=range_table[c(10),]

range_table$start=c(17.5)
range_table$min=c(15.5)
range_table$max=c(19.5)
range_table$p=rep(4,nrow(range_table))

factor_set_ee=create_ee_levels(factor_set,range_table,200,350)

#generate design table
design_ee=sampler_ee(factor_set_ee,20)

#execute design
out_folder=paste0(tempdir(),"/eetest")

library(future.apply)
plan(multisession)
cx_table=run_ecosim_experiment(design_ee,xml_model,factor_set_ee,ewe_link,out_folder,parallel=T)

df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)
plot_all_runs(df_cx,alpha=0.1)

y_ee=df_cx[df_cx$timestep==max(df_cx$timestep),]   #limit to the end of the run
df_ee=cbind(design_ee[order(design_ee$run_name),],y_ee[order(y_ee$run_name),])

results_ee=calculate_effects_ee("Whiting",factor_set_ee,df_ee)

plot(results_ee$mu_star~results_ee$sigma,type="n",xlab="mu_star",ylab="sigma")
text(labels=names(results_ee$mu_star),x=results_ee$sigma,y=results_ee$mu_star)

for(i in 1:ncol(results_ee$mustar_matrix))
{
  if(i==1) plot(results_ee$mustar_matrix[,i],col=1, type="l",ylim=c(min(results_ee$mustar_matrix),max(results_ee$mustar_matrix))) else {
    lines(results_ee$mustar_matrix[,i],col=i)
  }
}

### Try full factorial
factor_set_ff=new_ecosim_factor_set(m)

 #add options to "switch off" temperature responses
factor_set_ff=add_option_ecosim_shape(factor_set_ff,"Tempwarm","off",
                                   factor_set$shapes$Tempwarm$default$x,
                                   rep(1,length(factor_set$shapes$Tempwarm$default$y)))

factor_set_ff=add_option_ecosim_shape(factor_set_ff,"Tempcold","off",
                                   factor_set$shapes$Tempcold$default$x,
                                   rep(1,length(factor_set$shapes$Tempcold$default$y)))

factor_set_ff=add_option_ecosim_shape(factor_set_ff,"Twhiting","off",
                                   factor_set$shapes$Twhiting$default$x,
                                   rep(1,length(factor_set$shapes$Twhiting$default$y)))


factor_set_ff=add_option_ecosim_forcing(factor_set_ff,"Tbottom","warmer_2deg",ecocx::change_values_add(factor_set$forcing_functions$Tbottom$default$values,2,150,350))
factor_set_ff=add_option_ecosim_forcing(factor_set_ff,"Tbottom","colder_2deg",ecocx::change_values_add(factor_set$forcing_functions$Tbottom$default$values,-2,150,350))
plot(factor_set_ff$forcing_functions$Tbottom$default$values,type="l", ylim=c(15,21))
lines(factor_set_ff$forcing_functions$Tbottom$warmer_2deg$values,col="red")
lines(factor_set_ff$forcing_functions$Tbottom$colder_2deg$values,col="blue")

design_ff=sampler_full_factorial(factor_set_ff)
out_folder=paste0(tempdir(),"/fftest")

library(future.apply)
plan(multisession)
cx_table_ff=run_ecosim_experiment(design_ff,xml_model,factor_set_ff,ewe_link,out_folder,parallel=T)


df_cx_ff=get_ecosim_cx_biomass(cx_table_ff, m,relative=T)
y_ff=df_cx_ff[df_cx_ff$timestep==max(df_cx_ff$timestep),]
df_ff=cbind(design_ff[order(design_ff$run_name),],y_ff[order(y_ff$run_name),])

barplot(apply(df_ff[,21:29],MARGIN = 2, FUN=var))

sensitivity_results=calculate_effects_ff("Whiting",colnames(design_ff[,-(1:4)]),df_ff)

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


