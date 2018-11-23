hagfish.fisheries.tables = function(DS){
  #whelk.logbook.db(DS = "odbc.logbook.redo")
  #whelk.logbook.db(DS = "odbc.effort.redo")
  #whelk.logbook.db(DS = "logbook.redo")
  p = setup.parameters()
  

if (DS %in% c("landings")) {
  j = process.hagfish.logbook()
  nafo = load.shapefile(DS = 'nafo')
  j = intersect.sp(x=j, y=nafo, var = 'zone')
  j <- ddply (j,. (yr, zone), summarize, landings = sum(landings))
  test <- ddply(j,. (yr), summarize, landings = sum(landings))
  test.a <- mean(test$landings)
  #j$landings = j$landings/1000 #convert kgs to mt
  is.num <- sapply(j, is.numeric)
  j[is.num] <- lapply(j[is.num], round, 0)  
  j = as.data.table(j)  
  t = dcast(j, zone~yr, fun.aggregate = sum)
  t = t[order(t$zone), ]
}
  
  if (DS %in% c("landings.zones")) {
    j = process.hagfish.logbook()
    j <- ddply (j,. (yr, zones), summarize, landings = sum(landings))
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, yr~zones, fun.aggregate = sum)
    #t[, c('yr', '4w_shelf')]
  }
  
  if (DS %in% c("landings.nafo.licence")) {
    j = hagfish.logbook.db(DS="logbook")
    
    #j = process.hagfish.logbook()
    n = hagfish.logbook.db(DS = 'odbc.nafo')
    names( n ) = tolower( names( n ) )
    n$nafo_unit_area_id = n$area_id
    j = merge(j, n, by='nafo_unit_area_id', all.x = T)
    
    #nafo = load.shapefile(DS = 'nafo')
    #j = intersect.sp(x=j, y=nafo, var = 'zone')
    #l = hagfish.logbook.db(DS = 'odbc.licence.h')
    j$nafo = substr(j$area, 1, 2) 
    
    j <- ddply (j,. (yr, nafo, licence), summarize, landings = sum(landings))
    j = j[which(j$licence == '336698'), ]
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, yr~nafo+licence, fun.aggregate = sum)
    #t[, c('yr', '4w_shelf')]
  }
  
  if (DS %in% c("landings.zone.licence")) {
    j = hagfish.logbook.db(DS="logbook")
    
    h.z <- load.shapefile(DS = 'hagfish.z')
    j = intersect.sp(j, h.z, var = 'zones')
    

    j <- ddply (j,. (yr, zones, licence), summarize, landings = sum(landings))
    j = j[which(j$licence == '336698'), ]
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, yr~zones+licence, fun.aggregate = sum)
    #t[, c('yr', '4w_shelf')]
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

  if(DS %in% c("all.directed")) {
    l = process.hagfish.logbook()

    nafo <- load.shapefile(DS = 'nafo')
    l = intersect.sp(l, nafo, var='zone')
    l.n = l
    
    l.n = subset(l.n, is.na(zone)|zone == '4Vn' | zone == '4Vs' | zone == '4W'|zone == '4X'| zone == '5Ze')

    l.r = l.n[which(is.finite(l.n$landings)),]
    l.yr = l.r[, c("yr", "landings", "zone")]
    #l.yr$landings = l.yr$landings/1000
    l.t <- ddply(l.yr, .(yr), summarize, landings = sum(landings))
    l.t$zone = "All"
    l.yr <- ddply (l.yr, .(yr, zone), summarize, landings = sum(landings))
    l.yr = rbind(l.yr, l.t)
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    l.yr$zone = l.yr$zone
    j = as.data.table(l.yr)  
    j = j[, c('landings', 'yr', 'zone')]
    
    c = l.n[which(is.finite(l.n$cpue)),]
    c = c[, c("yr", "cpue", "zone")]
    c.t <- ddply(c, .(yr), summarize, cpue = mean(cpue))
    c.t$zone = "All"
    c <- ddply (c, .(yr, zone), summarize, cpue = mean(cpue))
    c<- rbind(c, c.t)
    is.num <- sapply(c, is.numeric)
    c[is.num] <- lapply(c[is.num], round, 1)  
    c$yr = as.numeric(c$yr)
    c$zone = c$zone
    c.j = as.data.table(c) 
    c.j = c.j[, c('cpue', 'yr', 'zone')]
    t = dcast(c.j, zone~yr)
    t = t[order(t$zone), ]
    
    e = l.n[which(is.finite(l.n$traps_retrieved)),]
    e = e[, c("yr", "traps_retrieved", "zone")]
    e.t <- ddply(e, .(yr), summarize, effort = sum(traps_retrieved))
    e.t$zone = "All"
    e <- ddply (e, .(yr, zone), summarize, effort = sum(traps_retrieved))
    e = rbind(e, e.t)
    is.num <- sapply(e, is.numeric)
    e[is.num] <- lapply(e[is.num], round, 0)  
    e.j = as.data.table(e)
    e.j = e.j[, c('effort', 'yr', 'zone')]
    t = merge(j, e.j, by=c('zone', 'yr'), all = T)
    t = merge(t, c.j, by=c('zone', 'yr'), all = T) 
    
    t$yr = as.numeric(as.character(t$yr))
    t = t[order(t$zone, t$yr), ]
  }
  
  if(DS %in% c("all.zones")) {
    l = process.hagfish.logbook2()
    l.n = l
    l.n$zones = l.n$zoner
    
    #l.n = subset(l.n, is.na(zone)|zone == '4Vn' | zone == '4Vs' | zone == '4W'|zone == '4X'| zone == '5Ze')
    
    l.n$zone[l.n$zones == "ne_channel" |  l.n$zones == 'jordan'| l.n$zones == 'georges_basin'] <- "GOM"
    l.n$zone[l.n$zones =="mid_shore"] <- "Mid-Shore"
    l.n$zone[l.n$zones =="gully"|  l.n$zones =='slope'] <- "Slope"
    l.n$zone[l.n$zones =="4vn"|  l.n$zones =='4vn_slope'] <- "4Vn"
    l.n$zone[is.na(l.n$zones)|l.n$zones =="other"] <- "Other"
    
  
    

    l.r = l.n[which(is.finite(l.n$landings)),]
    l.yr = l.r[, c("yr", "landings", "zone")]
    #l.yr$landings = l.yr$landings/1000
    l.t <- ddply(l.yr, .(yr), summarize, landings = sum(landings))
    l.t$zone = "All"
    l.yr <- ddply (l.yr, .(yr, zone), summarize, landings = sum(landings))
    l.yr = rbind(l.yr, l.t)
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    l.yr$zone = l.yr$zone
    j = as.data.table(l.yr)  
    j = j[, c('landings', 'yr', 'zone')]
    
    c = l.n[which(is.finite(l.n$cpue)),]
    c = c[, c("yr", "cpue", "zone")]
    c.t <- ddply(c, .(yr), summarize, cpue = mean(cpue))
    c.t$zone = "All"
    c <- ddply (c, .(yr, zone), summarize, cpue = mean(cpue))
    c<- rbind(c, c.t)
    is.num <- sapply(c, is.numeric)
    c[is.num] <- lapply(c[is.num], round, 1)  
    c$yr = as.numeric(c$yr)
    c$zone = c$zone
    c.j = as.data.table(c) 
    c.j = c.j[, c('cpue', 'yr', 'zone')]
    t = dcast(c.j, zone~yr)
    t = t[order(t$zone), ]
    
    e = l.n[which(is.finite(l.n$traps_retrieved)),]
    e = e[, c("yr", "traps_retrieved", "zone")]
    e.t <- ddply(e, .(yr), summarize, effort = sum(traps_retrieved))
    e.t$zone = "All"
    e <- ddply (e, .(yr, zone), summarize, effort = sum(traps_retrieved))
    e = rbind(e, e.t)
    is.num <- sapply(e, is.numeric)
    e[is.num] <- lapply(e[is.num], round, 0)  
    e.j = as.data.table(e)
    e.j = e.j[, c('effort', 'yr', 'zone')]
    t = merge(j, e.j, by=c('zone', 'yr'), all = T)
    t = merge(t, c.j, by=c('zone', 'yr'), all = T) 
    
    t$yr = as.numeric(as.character(t$yr))
    t = t[order(t$zone, t$yr), ]
  }
  
}
