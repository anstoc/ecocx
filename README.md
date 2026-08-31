
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecocx

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<img src="./man//figures/logo_niva.png" style="width:30.0%" /><img src="./man//figures/logo_eii.png" style="width:30.0%" />
<img src="./man//figures/logo_rcn.png" style="width:30.0%" />

EcoCX is an R package enabling computational experiments with Ecopath
with Ecosim and Ecospace (EwE) models. Its development is supported by a
FRIPRO grant from the Research Council of Norway (project CLIMAX,
2025-2028).

EcoCX contains functions to:

- read an EwE model
- create alternative inputs for, e.g., fishing effort time series and
  environmental drivers like projected ocean warming
- design computational experiments, i.e., repeated model runs that
  systematically change the inputs to learn about their effects and
  interactions
- execute the computational experiments by invoking the EwE Run Console
  (<https://github.com/Official-EwE/Eii.Ecopath.Runner>)
- summarize, visualize, and analyze the outputs of the model runs

For example, EcoCX might be used to quantify the effects of different
nature conservation actions (e.g., creating spatial no-fishing zones
versus a generic reduction in fishing effort), or to quantify how much
fishing and climate change interact in a simulated food web.

Supported experimental designs include:

- Random Monte Carlo. For batch-running EwE models, e.g., to generate
  input-output vectors for supervised machine learning
- Elementary effects, aka the Morris method, for identifying the inputs
  that have the largest effect on selected outputs with few model runs
- Full factorial experiments, for decomposing the variance of one or
  more model outputs into direct and interaction effects of each input

At present, EcoCX allows changing fishing effort time series and
environmental drivers in Ecosim. Supported model outputs are
species/group biomasses and fishery landings, but users can add own
functions to calculate ecological indicators based on them. Functions to
change other inputs like mediation functions and Ecospace input maps are
in the test phase and are scheduled for release in late 2026. Additional
experimental designs are planned for 2027. Meanwhile, users are free to
create their own experimental designs, output indicators, etc.

## Installation

You can install the development version of EcoCX like so:

``` r
remotes::install_github("anstoc/ecocx")
```

EcoCX builds on the EwE Run Console - in essence, a command line
interface for running EwE models. To execute Ecosim and Ecospace models,
you need to install the run console separately. It is available here:
<https://github.com/Official-EwE/Eii.Ecopath.Runner>.

## Example

This is a basic example running Monte Carlo simulations modifying
fishing effort in an example model. The built-in example model comes
from the free EwE textbook, which we highly recommend (get yours at
<https://pressbooks.bccampus.ca/ewemodel/>).

``` r
library(ecocx)
## load example Ecosim model
xml_model=paste0(get_path_to_exampledata(),"anchovy_bay_ecosim_ex.eiixml")
m=load_model_from_xml(xml_model)

##see the functional groups and their input parameters
m$ecopath$basic_estimates
#>    GroupID Sequence     GroupName Biomass         PoB   QoB  EE
#> 2        2        1        Whales  0.0800   0.0500000  9.00  NA
#> 3        3        2         Seals  0.0609   0.1539000 15.00  NA
#> 4        4        3           Cod  3.0000   0.3100000  2.58  NA
#> 5        5        4       Whiting  1.8000   0.5810000  3.10  NA
#> 6        6        5      Mackerel  1.2000   0.7233334  4.40  NA
#> 7        7        6       Anchovy  7.0000   1.2000000  9.13  NA
#> 8        8        7        Shrimp  0.8000   3.0000000    NA  NA
#> 9        9        8       Benthos      NA   3.0000000    NA 0.6
#> 10      10        9   Zooplankton 14.8000  35.0000000    NA  NA
#> 11      11       10 Phytoplankton 18.0000 120.0000000    NA  NA
#> 1        1       11      Detritus 10.0000          NA    NA 0.0

##create a set of factors (inputs that change in each model run): +/- 20% effort for the model's five fishing fleets 
factor_set=new_ecosim_factor_set(m)

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

summary(factor_set)
#>                 type                  name options
#> 1             tables         foraging_resp       1
#> 2             tables             mediation       1
#> 3             tables         vulnerability       1
#> 4     fishing_effort               Sealers       3
#> 5     fishing_effort              Trawlers       3
#> 6     fishing_effort               Seiners       3
#> 7     fishing_effort             Baitboats       3
#> 8     fishing_effort             Shrimpers       3
#> 9  forcing_functions             PPanomaly       1
#> 10 forcing_functions               Tbottom       1
#> 11            shapes Seal-Mackerel-Anchovy       1
#> 12            shapes              Tempcold       1
#> 13            shapes              Tempwarm       1
#> 14            shapes              Twhiting       1

##create experimental design: random Monte Carlo with 50 runs
set.seed(125)
design_mc=sampler_random(factor_set,size=50)

##connect to EwE Run Console (must be downloaded separately from https://github.com/Official-EwE/Eii.Ecopath.Runner). Replace path to the downloaded executable with your own.
ewe_link=connect_to_ewe("C:/Users/ANC/OneDrive - NIVA/Projects/2025/2025CLIMAX/WP1/TestRunConsole/EwERunConsole-1.0.32/EwERunConsole.exe")

##optional: use futures for parallel processing
library(future.apply)
#> Loading required package: future
plan(multisession)

##execute the model repeatedly according to the design!
cx_table=run_ecosim_experiment(design_mc,xml_model,factor_set,ewe_link,paste0(tempdir(),"/mctest"),parallel=T)

##obtain biomasses
df_cx=get_ecosim_cx_biomass(cx_table, m,relative=T)

##plot the 50 runs. One line for each run and species group, and colors are species groups.
plot_all_runs(df_cx,alpha=0.1)
```

<img src="man/figures/README-example-1.png" alt="" width="50%" />

    #> NULL
