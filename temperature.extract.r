temperature.extract = function(){
if(DS %in% c('prediction.surface','prediction.surface.redo')) {
  if(DS == 'prediction.surface'){
    load(file=file.path(fnProducts,'CanadaEastPredictionSurface.rdata'))
    return(H)
  }
  load(file.path(project.datadirectory('bio.bathymetry'),'modelled','bathymetry.baseline.canada.east.rdata'))
  H = Z[,c('z','dZ','ddZ','plon','plat')]

  ff = file.path(project.datadirectory('bio.temperature'),'modelled','t','canada.east','temperature.spatial.annual.seasonal.rdata')
  load(ff)

  for(y in p$yrs) {
    T = O[,y-1950+1,]
    if(p$annual.T.means) {
      T = rowMeans(T,na.rm=TRUE)
    } else {
      d= p$dyear * 10
      T = T[,d]
    }
    H = data.frame(H,T)
    names(H)[ncol(H)] <- paste('x',y,sep='.')
  }
  save(H,file=file.path(fnProducts,'CanadaEastPredictionSurface.rdata'))
  return(H)
}
}
