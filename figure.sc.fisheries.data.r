figure.hagfish.fisheries.data = function(DS) {
#browser()
p = setup.parameters()
l = process.sc.logbook()

setwd(p$shpdir)
write.shapefile(l, 'sc.fisheries.data', p=p)
l.n = l

#test = l[which(l$zone == 'SWNB'),]
#test.1 = test[which(test$licence == '306893' & test$yr == 2018),]
#test.1 = test.1[, 1:8]
#test.1 = test.1[ order(test.1$date.landed),]


if(DS %in% c("landings.4Vn", "complete")) {
    l.r = l[which(is.finite(l$landings)),]
    l.r = l.r[which(l$zone == '4Vs'),]
    l.yr = l.r[, c("f.year", "landings", "fzone")]
    l.yr$landings = l.yr$landings/1000 #convert kgs to mt
    l.yr <- ddply (l.yr, .(f.year, fzone), summarize, landings = sum(landings))
    l.t <- ddply(l.yr, .(f.year), summarize, landings= sum(landings))
    l.t$fzone = "Total"
    l.yr = rbind(l.yr, l.t)
    
    l.yr$yr = as.numeric(l.yr$f.year)
    l.yr$zone = l.yr$fzone
    
    #Add in a line for total landings.
    
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    uyrs = uyrs[which(is.finite(uyrs))]
    
    yrange = range (l.yr$landings, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 1)
    xlabels = as.numeric(xlabels)

    outfile = paste("sc.4Vs.landings")
    fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
    
    p1 <- ggplot(l.yr, aes(y=landings, x=yr, group=zone)) + theme_economist() + 
      geom_line(aes(linetype = zone, colour = zone), lty = "solid", size = 0.75) +
      geom_point(aes(shape=zone), color = "black", size = 1.75) +
      #scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid")) + 
      scale_shape_manual(values = c(1,1,1,1,1,1,1,1,1)) +
      scale_color_manual(values = c("red", "green", "blue", "darkblue", "purple", "darkgreen", "pink", "black", 'grey')) +
      theme(plot.margin=unit(c(1,4.5,1,1),"cm"), 
            legend.position= c(1.22, 0.66),
            legend.text=element_text(size = 8), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
      labs(x = "Year", y = "Landings (mt)")
    #png(filename=fn, width=7.5, height=5, units="in", res=450)
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    p1
    dev.off()
}

if(DS %in% c("landings.SWNB", "complete")) {
  l.r = l[which(is.finite(l$landings)),]
  l.r = l.r[which(l$zone == 'SWNB'& (l$fzone == "Zone 1 The Passages"|l$fzone == "Zone 2 Outside the Passages"|l$fzone == "Other")),]
  #l.r = l.r[which(l$zone == 'SWNB'),]
  
  l.yr = l.r[, c("f.year", "landings", "fzone")]
  #l.yr = l.r[, c("f.year", "landings", "licence")]
  
  l.yr$landings = l.yr$landings/1000 #convert kgs to mt
  l.yr <- ddply (l.yr, .(f.year, fzone), summarize, landings = sum(landings))
  #l.yr <- ddply (l.yr, .(f.year, licence), summarize, landings = sum(landings))
  
  l.yr$yr = as.numeric(l.yr$f.year)
  l.yr$zone = l.yr$fzone

  #Add in a line for total landings.
  
  uyrs = unique(l.yr$yr) 
  uyrs = as.numeric(uyrs)
  uyrs = uyrs[which(is.finite(uyrs))]
  
  yrange = range (l.yr$landings, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1], xrange[2], 1)
  xlabels = as.numeric(xlabels)
  
  outfile = paste("sc.swnb.yearly.landings")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  
  p1 <- ggplot(l.yr, aes(y=landings, x=yr, group=zone)) + theme_economist() + 
    geom_line(aes(linetype = zone, colour = zone), lty = "solid", size = 0.75) +
    geom_point(aes(shape=zone), color = "black", size = 1.75) +
    #scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid")) + 
    scale_shape_manual(values = c(1,1,1,1,1,1,1,1,1)) +
    scale_color_manual(values = c("red", "green", "blue", "purple", "darkgreen", "pink", "black", "darkblue", 'grey')) +
    theme(plot.margin=unit(c(1,4.5,1,1),"cm"), 
          legend.position= c(1.42, 0.66),
          legend.text=element_text(size = 8), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
    scale_x_continuous(breaks = xlabels) +
    theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
    labs(x = "Year", y = "Landings (mt)")
  png(filename=fn, width=7.5, height=5, units="in", res=450)
  
  #pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()
}

if(DS %in% c("landings", "complete")) {
  l.r = l[which(is.finite(l$landings)),]
  l.yr = l.r[, c("f.year", "landings", "zone")]
  l.yr$landings = l.yr$landings/1000 #convert kgs to mt
  l.yr <- ddply (l.yr, .(f.year, zone), summarize, landings = sum(landings))
  l.t <- ddply(l.yr, .(f.year), summarize, landings= sum(landings))
  l.t$zone = "Total"
  l.yr = rbind(l.yr, l.t)
  
  l.yr$yr = as.numeric(l.yr$f.year)
  l.yr$zone = l.yr$zone
  
  #Add in a line for total landings.
  
  uyrs = unique(l.yr$yr) 
  uyrs = as.numeric(uyrs)
  uyrs = uyrs[which(is.finite(uyrs))]
  
  yrange = range (l.yr$landings, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1], xrange[2], 1)
  xlabels = as.numeric(xlabels)
  
  outfile = paste("hagfish.yearly.landings")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  
  p1 <- ggplot(l.yr, aes(y=landings, x=yr, group=zone)) + theme_economist() + 
    geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
    geom_point(aes(shape=zone), color = "black", size = 1.5) +
    scale_linetype_manual(values = c("twodash", "dotted", "dashed", "twodash", "dotted", "dashed", "solid")) + 
    scale_shape_manual(values = c(1,17,15,16,15,1,16)) +
    theme(legend.position= c(.95, .75),
          legend.text=element_text(size = 10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
    scale_x_continuous(breaks = xlabels) +
    theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
    labs(x = "Year", y = "Landings (mt)")
  png(filename=fn, width=7.5, height=5, units="in", res=450)
  
  #pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()
}

if(DS %in% c("landings", "complete")) {
  l.r = l[which(is.finite(l$landings)),]
  l.yr = l.r[, c("f.year", "landings", "zone")]
  l.yr$landings = l.yr$landings/1000 #convert kgs to mt
  l.yr <- ddply (l.yr, .(f.year, zone), summarize, landings = sum(landings))
  l.t <- ddply(l.yr, .(f.year), summarize, landings= sum(landings))
  l.t$zone = "Total"
  l.yr = rbind(l.yr, l.t)
  
  l.yr$yr = as.numeric(l.yr$f.year)
  l.yr$zone = l.yr$zone
  
  #Add in a line for total landings.
  
  uyrs = unique(l.yr$yr) 
  uyrs = as.numeric(uyrs)
  uyrs = uyrs[which(is.finite(uyrs))]
  
  yrange = range (l.yr$landings, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1], xrange[2], 1)
  xlabels = as.numeric(xlabels)
  
  outfile = paste("hagfish.yearly.landings")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  
  p1 <- ggplot(l.yr, aes(y=landings, x=yr, group=zone)) + theme_economist() + 
    geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
    geom_point(aes(shape=zone), color = "black", size = 1.5) +
    scale_linetype_manual(values = c("twodash", "dotted", "dashed", "twodash", "dotted", "dashed", "solid")) + 
    scale_shape_manual(values = c(1,17,15,16,15,1,16)) +
    theme(legend.position= c(.95, .75),
          legend.text=element_text(size = 10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
    scale_x_continuous(breaks = xlabels) +
    theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
    labs(x = "Year", y = "Landings (mt)")
  png(filename=fn, width=7.5, height=5, units="in", res=450)
  
  #pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()
}
 
  if(DS %in% c('effort', 'complete')){
    l$traps_retrieved = as.numeric(l$traps_retrieved)
    l.r = l[which(is.finite(l$traps_retrieved)),]
    l.r = l.r[which(!is.na(l.r$zone)), ]
    l.yr = l.r[, c("yr", "traps_retrieved", "zone")]
    l.yr <- ddply (l.yr, .(yr, zone), summarize, effort = sum(traps_retrieved))
    l.t <- ddply(l.yr, .(yr), summarize, effort = sum(effort))
    l.t$zone = "Total"
    l.yr = rbind(l.yr, l.t)
    l.yr$yr = as.numeric(l.yr$yr)

    #add line in for total effort
 
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    yrange = range (l.yr$effort, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 1)
    xlabels = as.numeric(xlabels)
    
    outfile = paste("hagfish.yearly.effort")
    fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
    
    p1 <- ggplot(l.yr, aes(y=effort, x=yr, group=zone)) + theme_economist() + 
      geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
      geom_point(aes(shape=zone), color = "black", size = 1.5) +
      scale_linetype_manual(values = c("twodash", "dotted", "dashed", "twodash", "dotted","solid")) + 
      scale_shape_manual(values = c(1,17,15,16,15,16)) +
      theme(legend.position= c(.95, .85),
            legend.text=element_text(size = 10), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
      labs(x = "Year", y = "Effort (th)")
    png(filename=fn, width=7.5, height=5, units="in", res=450)
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    p1
    dev.off()
     
    }
  
  if(DS %in% c('cpue', 'complete')){
    
    write.shapefile(l.n, 'hagfish.catch.rates', p=p)
    l.r = l.n[which(is.finite(l.n$cpue)),]

    l.yr = l.r[, c("yr", "cpue", "zone")]
    l.yr <- ddply (l.yr, .(yr, zone), summarize, cpue = mean(cpue))
    l.yr$yr = as.numeric(l.yr$yr)
    l.yr$zone = l.yr$zone
    
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    yrange = range (l.yr$cpue, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 1)
    xlabels = as.numeric(xlabels)
    
    outfile = paste("hagfish.yearly.cpue")
    fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
    
    p1 <- ggplot(l.yr, aes(y=cpue, x=yr, group=zone)) + theme_economist() + 
      geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
      geom_point(aes(shape=zone), color = "black", size = 1.5) +
      scale_linetype_manual(values = c("twodash", "dotted", "dashed", "solid", "twodash")) + 
      scale_shape_manual(values = c(1,17,15,16,15)) +
      theme(legend.position= c(.95, .80),
            legend.text=element_text(size = 10), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
      labs(x = "Year", y = "CPUE (kg/th)")
    png(filename=fn, width=7.5, height=5, units="in", res=450)
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    p1
    dev.off()
  }

if(DS %in% c('effort.monthly', 'complete')){
  
  l$traps_retrieved = as.numeric(l$traps_retrieved)
  l.r = l[which(is.finite(l$traps_retrieved)),]
  l.r$month = months(l.r$date.fished)
  l.r$month = factor(l.r$month, levels = month.name)
  l.yr = l.r[, c("yr", "traps_retrieved", "zone", "month")]
 
  l.yr <- ddply (l.yr, .(yr, zone, month), summarize, effort = sum(traps_retrieved))
  l.yr <- ddply (l.yr, .(zone, month), summarize, effort = mean(effort))

  uyrs = unique(l.yr$yr) 
  uyrs = as.numeric(uyrs)

  p1 <- ggplot() + 
    geom_bar(data= l.yr,  stat = "identity", color = "black", width = 0.8, aes(x = month, y = effort)) + 
    theme_economist() + 
    facet_wrap(~ zone, ncol= 1) +
    theme(panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 65, hjust = 0),
          text = element_text(size = 9)) +
    #scale_x_continuous(breaks = xlabels) +
    labs(x = "Month", y="Effort (trap hauls)")
  outfile =  paste("hagfish.effort.monthly")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  png(filename=fn, width=7.5, height=5, units="in", res=450)
  
  #pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()
}

if(DS %in% c('effort.2017', 'complete')){
  
  l$traps_retrieved = as.numeric(l$traps_retrieved)
  l.r = l[which(is.finite(l$traps_retrieved)),]
  l.r$month = months(l.r$date.fished)
  l.r$month = factor(l.r$month, levels = month.name)
  
  l.yr = l.r[, c("yr", "traps_retrieved", "zone", "month")]
  
  l.2017 = l.yr[which(l.yr$yr == "2017"), ]
  l.2017 <- ddply (l.2017, .(yr, zone, month), summarize, effort = sum(traps_retrieved))

  uyrs = unique(l.yr$yr) 
  uyrs = as.numeric(uyrs)
  
   outfile =  paste("hagfish.effort.2017")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  
  p1 <- ggplot() + 
    geom_bar(data= l.2017,  stat = "identity", color = "black", fill ="steelblue", aes(x = month, y = effort)) + 
    theme_economist() + 
    facet_wrap(~ zone, ncol= 1) +
    theme(panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 65, hjust = 0),
          text = element_text(size = 9)) +
    #scale_x_continuous(breaks = xlabels) +
    labs(x = "Month", y="Effort (trap hauls)")
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  print(p1)
  dev.off()
}

if(DS %in% c('cpue.daily.1', 'complete')){
  l.n = l.n[which(is.finite(l.n$traps_retrieved)),]
  l.n$date_hauled = as.Date(l.n$date_hauled, "%Y-%b-%d")
  l.r = l.n[which(is.finite(l.n$cpue)),]
    #l.r = subset(l.r, zones == 'Gully'| zones == 'Slope'| zones == 'Mid-Shore')
    outfile = paste("hagfish.daily.cpue.2", sep = ".")
    limits = c(0, 115)

  l.daily = l.r[, c("yr", "cpue", "zones", "date_hauled")]
  l.yr <- ddply (l.daily, .(yr, zones), summarize, cpue = mean(cpue), date.fished = mean(date_hauled))
  l.daily$date.fished = l.daily$date_hauled
  l.daily$zone = l.daily$zones
  l.daily$yr = l.daily$yr
  l.yr$yr = l.yr$yr
  l.yr$zone = l.yr$zones
  
  l.avg <- ddply(l.yr, .(zone), summarize, cpue = mean(cpue))
  ref <- l.avg
  ref$cpuecr <- ref$cpue * 0.5
  ref$cpuec <- ref$cpue * 0.75
 
  yrange = range (l.yr$cpue, na.rm=T)
  yrange[1] = 0
  #ylabels = c(0, 2, 4, 10, 20, 50, 100)
  ylabels = c(0, 25, 50, 75, 100)
  
  fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  #fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  
  ld = l.daily[, c('cpue', 'date.fished', 'zone', 'yr')]
  ly = l.yr[, c('cpue', 'date.fished', 'zone', 'yr')]
 
  p1 <- ggplot() + 
    geom_point(data= ld,  alpha = 0.4, size = 0.5, aes(x = date.fished, y = cpue)) + 
    geom_line(data = ly, size = 1, aes(x = date.fished, y = cpue)) +
    geom_point(data = ly, size = 2, shape = 21, aes(x = date.fished, y = cpue, fill = zone)) +
    geom_hline(data = l.avg, color = "forestgreen", size = 0.59, aes(yintercept = cpue, linetype ="Mean")) +
    geom_hline(data = ref,  color = "red", size = 0.59, aes(yintercept = cpuecr, linetype ="50% Mean")) +
    geom_hline(data = ref,  color = "gold1", size = 0.59, aes(yintercept = cpuec, linetype ="75% Mean")) +
    geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), aes(linetype = "Escape-hole 0.5")) +
    geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), aes(linetype = "Escape-hole 0.5")) +
    geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), aes(linetype = "Escape-hole 0.5")) +
    
    geom_vline(xintercept = as.numeric(as.Date("2009-01-01")), linetype = 'dashed' ) +
    geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype = 'dashed' ) +

    
    scale_linetype_manual(values = c(2, 2,2), guide = guide_legend(override.aes = list(color = c("red", 'yellow', "green")))) +
    facet_wrap(~ zone, ncol= 1, strip.position = c('right')) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    #scale_y_log10(breaks = ylabels, labels = ylabels, limits = c(1, 100)) +
    scale_y_continuous(breaks = ylabels, labels = ylabels, limits = limits) +
    scale_fill_brewer(palette="Blues")+
    theme(legend.text = element_text(size=10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 75, hjust = 1.2),
          text = element_text(size = 9)) +
    theme(axis.line.x = element_line(color="grey", size = 1),
          axis.line.y = element_line(color="grey", size = 1))+
    theme_classic()+
    labs(x = "Year", y="CPUE (kg/trap)")
  #png(filename=fn, width=5, height=5, units="in", res=450)
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()
}

return(p1)
}
