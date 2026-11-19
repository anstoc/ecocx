ewe_link=connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.61/EwERunConsole.exe")

xmlfile=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecospace_ex.eiixml")
#xml_model="C:\\Users\\ANC\\OneDrive - NIVA\\Projects\\2025\\2025CLIMAX\\WP1\\Test_psfj_model\\final_model_sustainability.eiixml"
m=load_model_from_xml(xmlfile,ecospace_scenario="BayOfAnchovies")
factor_set=new_ecospace_factor_set(m)
summary(factor_set)

#sample
out_folder=paste0(tempdir(),"/ecospacetest")
design_mc=sampler_random(factor_set,10)

cx_table_mc=run_ecospace_experiment(design_mc,xmlfile,factor_set,m,ewe_link,out_folder,parallel=F)

