ewe_link=ecocx::connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.27/EwERunConsole.exe")

xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)
factor_set=new_ecosim_factor_set(m)

factor_set=add_option_ecosim_forcing(factor_set,"Tbottom","warmer_2deg",factor_set$forcing_functions$Tbottom$default$values+2)

factor_set=add_option_ecosim_forcing(factor_set,"PPanomaly","none",rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))

factor_set=add_option_ecosim_effort(factor_set,"Sealers","higher20p",1.2*factor_set$fishing_effort$Sealers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Sealers","lower20p",0.8*factor_set$fishing_effort$Sealers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Trawlers","higher20p",1.2*factor_set$fishing_effort$Trawlers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Trawlers","lower20p",0.8*factor_set$fishing_effort$Trawlers$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Seiners","higher20p",1.2*factor_set$fishing_effort$Seiners$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Seiners","lower20p",0.8*factor_set$fishing_effort$Seiners$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Baitboats","higher20p",1.2*factor_set$fishing_effort$Baitboats$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Baitboats","lower20p",0.8*factor_set$fishing_effort$Baitboats$default$values)

factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","higher20p",1.2*factor_set$fishing_effort$Shrimpers$default$values)
factor_set=add_option_ecosim_effort(factor_set,"Shrimpers","lower20p",0.8*factor_set$fishing_effort$Shrimpers$default$values)

out_folder=paste0(tempdir(),"/fftest")

design_ff=sampler_full_factorial(factor_set)

library(future.apply)
plan(multisession)
cx_table=run_ecosim_experiment(design_ff,xml_model,factor_set,ewe_link,out_folder,parallel=T)

df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)
y_ff=df_cx[df_cx$timestep==max(df_cx$timestep),]
df_ff=cbind(design_ff[order(design_ff$run_name),],y_ff[order(y_ff$run_name),])

sensitivity_results=calculate_effects_ff("Mackarel",colnames(design_ff[,-(1:4)]),df_ff)

#from Claude:
effect_matrix <- rbind(
  main_effect         = sensitivity_results$main_effect,
  interaction_effect  = sensitivity_results$total_effect - sensitivity_results$main_effect
)
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
