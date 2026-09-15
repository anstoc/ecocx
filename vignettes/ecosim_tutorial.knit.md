---
title: "Tutorial: Typical workflow for computational experiments with Ecosim models and discrete factors"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{ecosim_tutorial}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

EcoCX is an R package for computational experiments with Ecopath with Ecosim and Ecospace (EwE) models. In essence, it runs an Ecosim or Ecospace model repeatedly with systematic changes to inputs to quantify the effects of these inputs. This approach allows, e.g., uncertainty and sensitivity analyses or identifying the most important drivers of simulated ecosystem changes.

This tutorial shows a typical workflow for sensitivity analyses with an Ecosim model:

- Open and explore a model, typically built with the EwE software's graphical user interface (henceforth, the EwE GUI).
- Define which inputs' effects on the outputs you want to explore. These inputs are called factors. Currently, factors can only take discrete levels; direct support for sampling factors from continuous distributions will eventually be implemented. 
- Define alternative levels for these inputs - for example, an increase and a decrease of a fleet's fishing effort compared to the baseline, or temperature time series according to different climate scenarios.
- Create an experimental design - here, that means a plan to run the model repeatedly with random or systematic changes to the inputs, using the levels above. 
- Run the Ecosim model repeatedly; each run's inputs are different, according to the design.
- Read and analyze selected model outputs. 

Note that several of these steps depend on the question you're trying to answer: the choice of factors and their levels, the experiment design, and which outputs to analyze and how.  



## Getting EcoCX

To install and load the development version of EcoCX:


``` r
remotes::install_github("anstoc/ecocx")
```

``` r
library(ecocx)
```

In addition, you need the EwE Run Console, a command line interface to EwE. It is available on Github. Each release of EcoCX is tested against a specific release of the run console. To obtain a link to this recommended version:


``` r
ewe_link=get_run_console_link()
```

You can use a newer version if available, but be aware that we cannot guarantee compatibility.

## The example model

EcoCX reads Ecosim and Ecospace models in XML format, which can be exported via the EwE GUI. Here, we use an an example Ecosim model of Anchovy Bay included  with EcoCX. It is borrowed from the free EwE textbook (https://pressbooks.bccampus.ca/ewemodel/). The textbook is an excellent resource if you're unfamiliar with EwE modeling; this tutorial assumes at least superficial familiarity with how Ecosim works. 

You can access the example model like so:


``` r
xml_model=paste0(system.file('extdata', package = 'ecocx'),"/anchovy_bay_ecosim_ex.eiixml")
```

When working with your own model, you can export it as an .eiixml from the EwE GUI.

We now load the model and explore its contents, like the functional groups, fleets, and time series of environmental drivers. First, however, a caveat: the model was built for teaching purposes and some of its components, like a mediation function linking seals, mackerel, and anchovy, are unrealistic. 


``` r
m=load_model_from_xml(xml_model)
#show the model's functional groups
m$ecopath$basic_estimates$GroupName
```

```
#>  [1] "Whales"        "Seals"         "Cod"           "Whiting"      
#>  [5] "Mackerel"      "Anchovy"       "Shrimp"        "Benthos"      
#>  [9] "Zooplankton"   "Phytoplankton" "Detritus"
```

``` r
#show the model's fleet names
m$ecopath$fleets$FleetName
```

```
#> [1] "Sealers"    "Trawlers"   "Seiners"    "Bait boats" "Shrimpers"
```

``` r
#show Ecosim fishing effort time series for Trawlers
plot(m$ecosim$fishing_effort$Trawlers$values, type="l", main="Trawlers", ylab="Effort multiplier", xlab="Time step")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
#show Ecosim fishing effort time series for Sealers
plot(m$ecosim$fishing_effort$Sealers$values, type="l", main="Sealers", ylab="Effort multiplier", xlab="Time step")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

``` r
#show Ecosim temperature time series
plot(m$ecosim$forcing_functions$Tbottom$values, type="l", main="Bottom temperature", ylab="Temperature [°C]", xlab="Time step")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

``` r
#show Ecosim primary production anomaly
plot(m$ecosim$forcing_functions$PPanomaly$values, type="l", main="Prim. prod. anomaly", ylab="Production multiplier", xlab="Time step")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

The model runs for 41 years (with 12 months each, so 492 time steps). Trawling effort more than triples over this period, sealing is phased out, and the annual mean bottom temperature increases from around 17°C to about 19°C. Furthermore, the model includes a primary production anomaly estimated to make it fit historical data better. Note that all of these are inputs to the model; we need some further setup before running it.

## Defining factors and levels

The model inputs whose effects we investigate are called factors. As we have seen above, in the Anchovy Bay model, there are drastic changes to sealing and trawl fisheries over time, major warming, and a large primary production anomaly. For the sake of this example, let's say there are also discussions about increasing or decreasing the fishing effort for Shrimpers by 50%. We will explore how much these five factors together affect functional group biomasses, and then attribute these changes exactly to the factors.

For this, we need to define plausible alternative levels for each factors - e.g., whether an intervention like phasing out sealing is implemented at all, or how much temperatures rise based on uncertainty about the future climate. Be aware that those levels affect each factor's effect size; a 0.1°C change in temperature will have a smaller effect than a 10°C change. 

The first step is creating a 'factor set' - a collection of alternative levels for each factor. At present, EcoCX only supports discrete factor levels. To create a factor set from a model:


``` r
factor_set=new_ecosim_factor_set(m)
summary(factor_set)
```

```
#>                 type                  name levels
#> 1             tables         vulnerability      1
#> 2     fishing_effort               Sealers      1
#> 3     fishing_effort              Trawlers      1
#> 4     fishing_effort               Seiners      1
#> 5     fishing_effort             Baitboats      1
#> 6     fishing_effort             Shrimpers      1
#> 7  forcing_functions             PPanomaly      1
#> 8  forcing_functions               Tbottom      1
#> 9             shapes Seal-Mackerel-Anchovy      1
#> 10            shapes              Tempcold      1
#> 11            shapes              Tempwarm      1
#> 12            shapes              Twhiting      1
```

The summary above lists all the factors that you might consider changing: The vulnerability table, fishing effort for the five fleets, the two environmental driver time series (in EwE, those are called 'forcing functions'), and the shapes of a mediation function and two environmental response functions. For each factor, one level is defined. We will add additional levels to ask: What if we don't phase out sealing? What if we keep trawling at its current level? What if there was no climate anomaly? And what if there were two degrees less or more of warming?  

EcoCX provides functions to add and visualize additional levels for these four factors. 


``` r
#Temperature add +/- 2 degrees, linearly changing
factor_set=add_level_ecosim_forcing(factor_set,"Tbottom","warmer_2deg",
      change_values_add(factor_set$forcing_functions$Tbottom$default$values,2,150,350))
factor_set=add_level_ecosim_forcing(factor_set,"Tbottom","colder_2deg",
      change_values_add(factor_set$forcing_functions$Tbottom$default$values,-2,150,350))
plot_ecosim_factor_levels(factor_set,"Tbottom")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```
#> NULL
```

``` r
#Primary production anomaly: switch on or off
factor_set=add_level_ecosim_forcing(factor_set,"PPanomaly","none",
      rep(1,get_ecosim_forcing_length(factor_set,"PPanomaly")))
plot_ecosim_factor_levels(factor_set,"PPanomaly")
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```
#> NULL
```

``` r
#Sealers: keep as current
factor_set=add_level_ecosim_effort(factor_set,"Sealers","keep",
      rep(1,length(factor_set$fishing_effort$Sealers$default$values)))
plot_ecosim_factor_levels(factor_set,"Sealers")      
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```
#> NULL
```

``` r
#Trawlers: keep the current effort instead of phasing out
factor_set=add_level_ecosim_effort(factor_set,"Trawlers","keep",
      rep(1,length(factor_set$fishing_effort$Sealers$default$values)))
plot_ecosim_factor_levels(factor_set,"Trawlers") 
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```
#> NULL
```

``` r
#Shrimpers: consider changing the current effort +/- 50%
factor_set=add_level_ecosim_effort(factor_set,"Shrimpers","plus50p",
      change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,1.5,150,350))
factor_set=add_level_ecosim_effort(factor_set,"Shrimpers","minus50p",
      change_values_mult(factor_set$fishing_effort$Shrimpers$default$values,0.5,150,350))
plot_ecosim_factor_levels(factor_set,"Shrimpers") 
```

![](C:/Users/ANC/AppData/Local/Temp/RtmpishMcf/preview-111018146bd1.dir/ecosim_tutorial_files/figure-html/unnamed-chunk-7-5.png)<!-- -->

```
#> NULL
```

``` r
summary(factor_set)
```

```
#>                 type                  name levels
#> 1             tables         vulnerability      1
#> 2     fishing_effort               Sealers      2
#> 3     fishing_effort              Trawlers      2
#> 4     fishing_effort               Seiners      1
#> 5     fishing_effort             Baitboats      1
#> 6     fishing_effort             Shrimpers      3
#> 7  forcing_functions             PPanomaly      2
#> 8  forcing_functions               Tbottom      3
#> 9             shapes Seal-Mackerel-Anchovy      1
#> 10            shapes              Tempcold      1
#> 11            shapes              Tempwarm      1
#> 12            shapes              Twhiting      1
```

## Creating an experimental design

Now that we have created plausible alternative levels for each factor of interest, we need to make a plan (henceforth, a 'design') describing how often we will run the model with different factor levels, and what level should be used for each factor in each run. EcoCX currently contains functions to create three kinds of designs. The next subsection describes the simplest design, random Monte Carlo. The other two designs are the elementary effects method and full factorial experiments. They are described later in the tutorial, after explaining how to run a design and obtain the outputs of all runs. Furthermore, you're free to write your own designs, as long as they are described in a data frame resembling the output of the functions provided by EcoCX.

## Random Monte Carlo

The code below creates a random Monte Carlo design with 20 model runs. In each run, each factor takes on one of the levels defined above with equal probability.


``` r
out_folder=paste0(tempdir(),"/mctest")
design_mc=sampler_random(factor_set, 20)
head(design_mc)
```

```
#>   run_id sub_id   run_name              comment vulnerability Sealers Trawlers
#> 1   0001   0000 R0001_0000 Random sample, run 1       default    keep     keep
#> 2   0002   0000 R0002_0000 Random sample, run 2       default default  default
#> 3   0003   0000 R0003_0000 Random sample, run 3       default    keep  default
#> 4   0004   0000 R0004_0000 Random sample, run 4       default    keep  default
#> 5   0005   0000 R0005_0000 Random sample, run 5       default default     keep
#> 6   0006   0000 R0006_0000 Random sample, run 6       default    keep     keep
#>   Seiners Baitboats Shrimpers PPanomaly     Tbottom Seal-Mackerel-Anchovy
#> 1 default   default  minus50p   default warmer_2deg               default
#> 2 default   default   plus50p      none colder_2deg               default
#> 3 default   default   default      none colder_2deg               default
#> 4 default   default   default   default colder_2deg               default
#> 5 default   default   default      none     default               default
#> 6 default   default   plus50p      none colder_2deg               default
#>   Tempcold Tempwarm Twhiting
#> 1  default  default  default
#> 2  default  default  default
#> 3  default  default  default
#> 4  default  default  default
#> 5  default  default  default
#> 6  default  default  default
```

The function `sampler_random` returns a data frame with one run for each planned model run. It contains the following columns:

- `run_id`: An ID for the run. For the Monte Carlo design, each run has a unique ID.
- `sub_id`: A sub-id used in the elementary effects method (see below). Not used in the MC design.
- `run_name`: Concatenated `run_id` and `sub_id`. This serves as the run's unique identifier in all designs.
- `comment`: A description of the run. Can also be used to store additional information about the run (see, e.g., the elementary effects method below).
- One column for each factor in the factor set, stating the name of the factor's level in the run.  Also factors with only one level are included as columns, but have the same value in each row.

Beyond the built-in designs like those created with `sampler_random`, you can create your own and then execute it, by providing a data frame in the format described above.  

## Running the experiments, sequentially or in parallel

To run EwE models from EcoCX, first connect to the Run Console, providing a path to the executable that you downloaded:


``` r
#replace path with your own
ewe_link=connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.35/EwERunConsole.exe")
```

If you have the `future.apply` package, you can speed up executing your computations by executing the runs in your design in parallel: 


``` r
library(future.apply)
```

```
#> Loading required package: future
```

``` r
plan(multisession)
```

In this case, they will be executed according to the plan you chose. Otherwise, they will be executed sequentially. 

Let's finally execute the runs and store the outputs in a temporary folder:


``` r
out_folder=paste0(tempdir(),"/mctest")
cx_table_mc=run_ecosim_experiment(design_mc,xml_model,factor_set,ewe_link,out_folder,parallel=T)
colnames(cx_table_mc)
```

```
#> [1] "run_name" "model"    "folder"   "json"
```

The returned data frame `cx_table_mc` contains for each run:

- the `run_name` from the design
- path to the EIIXML file of the model executed
- path to the folder where the run's outputs are stored
- path to the JSON file describing the run (this is used by the Run Console - see next section) 

## What's happening in the background?

## Reading and analyzing the outputs

## Full factorial experiments

## Factor screening with the elementary effects method

## Entry points for adding your own methods

EcoCX is designed to allow users to add their own methods. Typical entry points to insert your own functions include:

- The experimental design. You can write a function returning a design table like the built-in functions and then use EcoCX to execute the model runs it describes.
- Harvesting model outputs. EcoCX has built-in functions to obtain Ecosim biomasses and fishery catches, but you're free to explore the generated model outputs and write functions that read others or calculate new ecological indicators from them. Those can then be fed into the design-specific analysis functions.
- Analyzing . For example, you could use EcoCX' simple Monte Carlo sampler to run a model with many factors 100,000 times (if you have the computing power and storage space for outputs), then use machine learning methods of your choice to extract information from the gigantic pile of data you just created.


