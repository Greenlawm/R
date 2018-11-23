figure.length.freq.timeseries = function(){
  
gsdet = groundfish.db (DS = "gsdet")  
gsinf = groundfish.db (DS = "gsinf")

lw = merge(gsdet, gsinf)
species = "2511"
yrs = 2000:2017

}