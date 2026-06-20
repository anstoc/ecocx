
#Note: no unit test for this function as it relies on a Windos executable and CRAN cautions

#' Connect to and execute a test run with the local EwE run console.
#'
#' Calls the local executable of the EwE run console with example data and checks if expected outputs are generated. This function must be called before any models can be run.
#'
#' @param path_to_ewe The path to the run console executable on the local computer.
#'
#' @returns An object containing the path, the output folder, test arguments pointing to an internal example  model provided with the package, and whether a test run created the expected results.
#'
#' @export
connect_to_ewe=function(path_to_ewe)
{
  temp_out_folder=paste0(tempdir(),"/","connect_test",round(100000*runif(1)))

  conf_file=paste0(system.file('extdata', package = 'ecocx'),"/ecocx1_runinfo.json") #internal test data

  command_str=path_to_ewe

  arg_str=paste("-i", shQuote(conf_file), "-o",shQuote(temp_out_folder))

  system2(command_str,arg_str)   #run example model

  #create output object
  ewe_link=list()
  ewe_link$command_str=command_str
  ewe_link$test_arg_str=arg_str
  class(ewe_link)="ewe_link"
  if( "biomass_annual.csv" %in% list.files(paste0(temp_out_folder,"/ecocx1_runinfo/ecosim_Scene 1"))) #did the test run create this expected output file?
  {
    ewe_link$test_passed=T
  } else
  {
    ewe_link$test_passed=F
    warning("A test run did not create the expected output. Run system2(x$command_str,x$test_arg_str) to see the console output, with x standing for the returned object.")
  }

  ewe_link

}

