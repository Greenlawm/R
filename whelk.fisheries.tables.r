whelk.fisheries.tables = function(DS, yrs=yrs){
  #whelk.logbook.db(DS = "odbc.logbook.redo")
  #whelk.logbook.db(DS = "odbc.effort.redo")
  #whelk.logbook.db(DS = "logbook.redo")
  if (DS %in% c("landings")) {
    j = whelk.logbook.db( DS="logbook", yrs = yrs )
    nafo = load.shapefile(DS = 'nafo')
    j = intersect.sp(x=j, y=nafo, var = 'zone')
    j <- ddply (j,. (yr, licence, zone), summarize, landings = sum(landings))
    #j$landings = j$landings/1000 #convert kgs to mt
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, licence+zone~yr, fun.aggregate = sum)
    t = t[order(t$zone), ]
  }
  
  if (DS %in% c("effort")){
    j = whelk.logbook.db(DS = "effort", yrs= yrs)
    j$trap_set[(is.na(j$trap_set))] <- 50
    nafo = load.shapefile(DS = 'nafo')
    j = intersect.sp(x=j, y=nafo, var = 'zone')
    j <- ddply (j,. (yr, licence_id, zone, surname), summarize, trips = sum(trap_set))
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, licence_id+zone+surname~yr, fun.aggregate = sum)
    t = t[order(t$zone), ]
   
  }
  
  if (DS %in% c("trips")){
    j = whelk.logbook.db(DS = "effort", yrs = yrs)
    nafo = load.shapefile(DS = 'nafo')
    j = intersect.sp(x=j, y=nafo, var = 'zone')
    to.extract = c('yr', 'licence_id', 'zone', 'surname', 'trip_id')
    j = j[, to.extract ]
    j = unique(j)
    j = as.data.table(j)  
    t = dcast(j, licence_id+zone+surname~yr)
    t = t[order(t$zone), ]
  }
  
  return(t)
  
  }

