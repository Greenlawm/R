urchin.fisheries.data.tables = function(DS){
  #whelk.logbook.db(DS = "odbc.logbook.redo")
  #whelk.logbook.db(DS = "odbc.effort.redo")
  #whelk.logbook.db(DS = "logbook.redo")
  p = setup.parameters()

  
 
if (DS %in% c("landings.zone")) {
  j = urchin.logbook.db(DS = "effort")
  l.yr <- ddply(j, .(f.year), summarize, landings = sum(landings))
  l.yr$zone_type = "All"
  j <- ddply (j,. (f.year, zone_type), summarize, landings = sum(landings))
  j = rbind(l.yr, j)
  #j$landings = j$landings/1000 #convert kgs to mt
  is.num <- sapply(j, is.numeric)
  j[is.num] <- lapply(j[is.num], round, 0)  
  j = as.data.table(j)  
  t = dcast(j, f.year~zone_type, fun.aggregate = sum, value.var = 'landings')
}
  
 
if (DS %in% c("effort")){
  j = hagfish.logbook.db(DS = 'effort')
  nafo = load.shapefile(DS = 'nafo')
  j = intersect.sp(x=j, y=nafo, var = 'zone')
  
  j <- ddply (j,. (yr, zone, licence_id), summarize, effort = sum(traps_retrieved))
  is.num <- sapply(j, is.numeric)
  j[is.num] <- lapply(j[is.num], round, 0)  
  j = as.data.table(j)  
  t = dcast(j, zone+licence_id~yr, fun.aggregate = sum)
  t = t[order(t$zone), ]
  
}
return(t)
 }
