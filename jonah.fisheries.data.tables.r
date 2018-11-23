jonah.fisheries.data.tables = function(DS) {
 #browser()

  p = setup.parameters()
  l =  jonah.logbook.db( DS="logbook")
  l$cpue = as.numeric(l$cpue)
  l$effort = as.numeric(l$effort)
  #l = l[which(l$lfa != '41'), ]
  #l = l[which(l$yr != 2017),]
  l$lfa[l$lfa == "LOBSTER - GREY ZONE"] <- "Grey Zone"
  l$lfa[l$lfa == "NA"] <- "Unknown"
  l$cpue = as.numeric(l$cpue)
  l$effort = as.numeric(l$effort)
  l$landings = as.numeric(l$landings)
  l$doc_type[which(substr(l$doc_type, 1,2) == 'SD')] <- "lobster.log" #Anything starting with SD is lobster log
  l$doc_type[which(substr(l$doc_type, 1,2) == 'MD')] <- "crab.doc"
  l$doc_type[which(l$doc_type == 'NA')] <- "crab.doc"
  
  d.lic = jonah.logbook.db (DS = 'jonah.licence')
  names(d.lic)[names(d.lic) == "licence_id"] <- "licence"
  
  d = merge(d.lic, l, by= "licence", all.x = T)
  d$directed = "directed"
  d = d[, c('licence', 'directed')]
  d = unique(d)
  
  l = merge(l, d, by='licence', all.x = T)
  l$directed[which(is.na(l$directed))] <- "bycatch" 
  names(l)[names(l) == "directed"] <- "f.type"
  
  table(l$doc_type, l$f.type)

  if(DS %in% c("landings.licence")) {
    j = l[which(l$f.type == 'directed'), ]
    j <- ddply (j,. (yr, licence, lfa), summarize, landings = sum(landings))
    #j$landings = j$landings/1000 #convert kgs to mt
    is.num <- sapply(j, is.numeric)
    j[is.num] <- lapply(j[is.num], round, 0)  
    j = as.data.table(j)  
    t = dcast(j, licence+lfa~yr, fun.aggregate = sum)
    t = t[order(t$lfa), ]
    
  }
  
  if(DS %in% c("landings")) {
  l.r = l[which(is.finite(l$landings)),]
  l.yr = l.r[, c("yr", "landings", "lfa")]
  #l.yr$landings = l.yr$landings/1000
  l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
  is.num <- sapply(l.yr, is.numeric)
  l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
  l.yr$yr = as.numeric(l.yr$yr)
  j = as.data.table(l.yr)  
  t = dcast(j, lfa~yr, fun.aggregate = sum)
  t = t[order(t$lfa), ]
  
  }
  
  if(DS %in% c("landings.directed")) {
    l.r = l[which(is.finite(l$landings)),]
    l.r = l.r[which(l.r$f.type == 'directed'),] 
    l.yr = l.r[, c("yr", "landings", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    t = dcast(j, lfa~yr, fun.aggregate = sum)
    t = t[order(t$lfa), ]
    
  }
  
  if(DS %in% c("effort.directed")) {
    l.r = l[which(is.finite(l$effort)),]
    l.r = l.r[which(l.r$f.type == 'directed'),] 
    l.yr = l.r[, c("yr", "effort", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, effort = sum(effort))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    t = dcast(j, lfa~yr, fun.aggregate = sum)
    t = t[order(t$lfa), ]
    
  }
  
  if(DS %in% c("cpue.directed")) {
    l.r = l[which(is.finite(l$cpue)),]
    l.r = l.r[which(l.r$f.type == 'directed'),] 
    l.yr = l.r[, c("yr", "cpue", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, cpue = mean(cpue))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 1)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    t = dcast(j, lfa~yr)
    t = t[order(t$lfa), ]
    
  }
  
  if(DS %in% c("all.directed")) {
    
    l.r = l[which(is.finite(l$landings)),]
    l.r = l.r[which(l.r$f.type == 'directed'),] 
    l.yr = l.r[, c("yr", "landings", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    
    c = l[which(is.finite(l$cpue)),]
    c = c[which(c$cpue < 200),]
    c = c[order(-c$cpue), ]
    c = c[which(c$f.type == 'directed'),] 
    c = c[, c("yr", "cpue", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    c <- ddply (c, .(yr, lfa), summarize, cpue = mean(cpue))
    is.num <- sapply(c, is.numeric)
    c[is.num] <- lapply(c[is.num], round, 1)  
    c$yr = as.numeric(c$yr)
    c.j = as.data.table(c)  
    t = dcast(j, lfa~yr)
    t = t[order(t$lfa), ]
    
    e = l[which(is.finite(l$effort)),]
    e = e[which(e$f.type == 'directed'),] 
    e = e[, c("yr", "effort", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    e <- ddply (e, .(yr, lfa), summarize, effort = sum(effort))
    is.num <- sapply(e, is.numeric)
    e[is.num] <- lapply(e[is.num], round, 0)  
    e$yr = as.numeric(e$yr)
    e.j = as.data.table(e)  
    t = merge(j, c.j, by=c('yr', 'lfa'), all = T)
    t = merge(t, e.j, by=c('yr', 'lfa'), all = T)
     
    t.1 = c('1996', '34', '19000', '4.8', '4079')
    t.2 = c('1997', '34', '145800', '4.0', '36589')
    t.3 = c('1998', '34', '168700', '4.6', '36492')
    t.4 = c('1999', '34', '185000', '5.6', '33321')
    t.5 = c('2000', '34', '280000', '5.6', '49983')
    t.6 = c('2001', '34', '256000', '4.3', '59955')
    
    t.7 = c('1997', '33', '136000', '3.08', '44101')
    t.8 = c('1998', '33', '167000', '2.9', '57534')
    t.9 = c('1999', '33', '160000', '3.0', '53406')
    
    t.10 = c('1995', '38', '20700', '5.3', '3875')
    t.11 = c('1996', '38', '25300', '6.5', '3875')
    t.12 = c('1997', '38', '26900', '5.3', '5125')
    t.13 = c('1998', '38', '60900', '4.8', '12760')
    t.14 = c('1999', '38', '51000', '5.7', '8939')
    t.15 = c('2000', '38', '212400', '5.5', '38720')
    t.16 = c('2001', '38', '227200', '4.8', '47628')
    
    df = rbind(t.1, t.2, t.3, t.4, t.5, t.6, t.7, t.8, t.9, t.10, t.11, t.12, t.13, t.14, t.15, t.16)
    df = as.data.frame(df)
    names(df) = c('yr', 'lfa', 'landings', 'cpue', 'effort')
    t = rbind(t, df)
    t = t[, c('lfa', 'yr', 'landings', 'effort', 'cpue')]
    t$yr = as.numeric(as.character(t$yr))
    t = t[order(t$lfa, t$yr), ]
    
    
  }
  
  if(DS %in% c("percent.change")) {
    
    l.r = l[which(is.finite(l$landings)),]
    l.r = l.r[which(l.r$f.type == 'directed'),] 
    l.yr = l.r[, c("yr", "landings", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    
    c = l[which(is.finite(l$cpue)),]
    c = c[which(c$cpue < 200),]
    c = c[order(-c$cpue), ]
    c = c[which(c$f.type == 'directed'),] 
    c = c[, c("yr", "cpue", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    c <- ddply (c, .(yr, lfa), summarize, cpue = mean(cpue))
    is.num <- sapply(c, is.numeric)
    c[is.num] <- lapply(c[is.num], round, 1)  
    c$yr = as.numeric(c$yr)
    c.j = as.data.table(c)  
    t = dcast(j, lfa~yr)
    t = t[order(t$lfa), ]
    
    e = l[which(is.finite(l$effort)),]
    e = e[which(e$f.type == 'directed'),] 
    e = e[, c("yr", "effort", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    e <- ddply (e, .(yr, lfa), summarize, effort = sum(effort))
    is.num <- sapply(e, is.numeric)
    e[is.num] <- lapply(e[is.num], round, 0)  
    e$yr = as.numeric(e$yr)
    e.j = as.data.table(e)  
    t = merge(j, c.j, by=c('yr', 'lfa'), all = T)
    t = merge(t, e.j, by=c('yr', 'lfa'), all = T)
    
    t.1 = c('1996', '34', '19000', '4.8', '4079')
    t.2 = c('1997', '34', '145800', '4.0', '36589')
    t.3 = c('1998', '34', '168700', '4.6', '36492')
    t.4 = c('1999', '34', '185000', '5.6', '33321')
    t.5 = c('2000', '34', '280000', '5.6', '49983')
    t.6 = c('2001', '34', '256000', '4.3', '59955')
    
    t.7 = c('1997', '33', '136000', '3.08', '44101')
    t.8 = c('1998', '33', '167000', '2.9', '57534')
    t.9 = c('1999', '33', '160000', '3.0', '53406')
    
    t.10 = c('1995', '38', '20700', '5.3', '3875')
    t.11 = c('1996', '38', '25300', '6.5', '3875')
    t.12 = c('1997', '38', '26900', '5.3', '5125')
    t.13 = c('1998', '38', '60900', '4.8', '12760')
    t.14 = c('1999', '38', '51000', '5.7', '8939')
    t.15 = c('2000', '38', '212400', '5.5', '38720')
    t.16 = c('2001', '38', '227200', '4.8', '47628')
    
    df = rbind(t.1, t.2, t.3, t.4, t.5, t.6, t.7, t.8, t.9, t.10, t.11, t.12, t.13, t.14, t.15, t.16)
    df = as.data.frame(df)
    names(df) = c('yr', 'lfa', 'landings', 'cpue', 'effort')
    t = rbind(t, df)
    t = t[, c('lfa', 'yr', 'landings', 'effort', 'cpue')]
    t$yr = as.numeric(as.character(t$yr))
    t$landings = as.numeric(as.character(t$landings))
    t$effort = as.numeric(as.character(t$effort))
    t$cpue = as.numeric(as.character(t$cpue))
   
     t = t[order(t$lfa, t$yr), ]
    
    y =  sort(unique(t$yr))
    y.1 = max(y) - 1
    p = t[which(t$yr >= y.1), ]
    y.2 = max(y)
    t.e = p[which(p$yr == y.1),]
    t.l = p[which(p$yr == y.2),]
    n.1 = t.e[, 1]
    t.l = t.l[,3:5]
    t.e = t.e[, 3:5]
    n = (t.e - t.l)/t.e
    n = cbind(n.1, n)

    
    return(n)
    
    
  }
  
  
  
  if(DS %in% c("landings.bycatch")) {
    l.r = l[which(is.finite(l$landings)),]
    l.r = l.r[which(l.r$f.type == 'bycatch'),] 
    l.yr = l.r[, c("yr", "landings", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    is.num <- sapply(l.yr, is.numeric)
    l.yr[is.num] <- lapply(l.yr[is.num], round, 0)  
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    t = dcast(j, lfa~yr, fun.aggregate = sum)
    t = t[order(t$lfa), ]
    
  }
  return(t)
  
    }
  
