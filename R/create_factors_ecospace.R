new_ecospace_factor_set=function(m, default_name="default")
{
  #everything that carries over from Ecosim
  factor_set=new_ecosim_factor_set(m)
  factor_set$forcing_functions=NULL

  #static maps
  factor_set$env_maps=list()

  #env. drivers
  for(i in 1:length(m$ecospace$envmaps)) {
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]]=list()
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]=m$ecospace$envmaps[[i]]
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]$factor_value=1
    factor_set$env_maps[[names(m$ecospace$envmaps)[i]]][[default_name]]$type="env_map"
  }

  #habitats
  for(i in 1:length(m$ecospace$habmaps)) {
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]]=list()
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]=m$ecospace$habmaps[[i]]
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]$factor_value=1
    factor_set$habitats[[names(m$ecospace$habmaps)[i]]][[default_name]]$type="habitat"
  }

  #MPAs
  for(i in 1:length(m$ecospace$mpamaps)) {
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]]=list()
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]=m$ecospace$mpas[[i]]
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]$factor_value=1
    factor_set$mpas[[names(m$ecospace$mpamaps)[i]]][[default_name]]$type="mpa"
  }

  class(factor_set)="ecocx_factor_set"
  factor_set
}
