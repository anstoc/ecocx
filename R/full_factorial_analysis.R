#contains functions to analyze full factorial experiments

calculate_effects_ff=function(output_name, factor_names, df_ff)
{
  for (f in factor_names) {df_ff[[f]]=as.factor(df_ff[[f]])}

  grand_mean=mean(df_ff[[output_name]])
  total_variance=mean((df_ff[[output_name]] - grand_mean)^2) #mean calculated by dividing by n, not by (n-1), because it's the population mean. Keep this consistent throughout calculations!

  #function calculating main effect of one factor,  applied below to all factors
  calculate_main_effect=function(df_ff, factor_name, output_name, grand_mean, total_variance)
  {
    level_means = tapply(df_ff[[output_name]], df_ff[[factor_name]], mean)
    variance_of_level_means = mean((level_means - grand_mean)^2)
    variance_of_level_means / total_variance
  }

  #function calculating total effect of one factor,  applied below to all factors
  calculate_total_effect <- function(df_ff, factor_name, output_name, factor_names, grand_mean, total_variance)
  {
    other_factors=setdiff(factor_names, factor_name)
    grouping_factors = df_ff[other_factors]
    averaged_over_factor = tapply(df_ff[[output_name]], grouping_factors, mean)

    variance_without_factor = mean((averaged_over_factor - grand_mean)^2, na.rm = F)
    1 - (variance_without_factor / total_variance)
  }

  #apply functions
  main_effects = sapply(factor_names, calculate_main_effect, df_ff = df_ff, output_name = output_name,
                          grand_mean = grand_mean, total_variance = total_variance)

  total_effects = sapply(factor_names, calculate_total_effect, df_ff = df_ff, factor_names=factor_names, output_name = output_name,
                        grand_mean = grand_mean, total_variance = total_variance)

  data.frame("factor"=factor_names, "output"=output_name, "main_effect"=main_effects, "total_effect"=total_effects, "interaction_part"=total_effects-main_effects)
}




