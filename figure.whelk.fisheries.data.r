figure.whelk.fisheries.data = function(DS, yrs = yrs) {
 #browser()

p = setup.parameters()
setwd(p$shpdir)
l =  whelk.logbook.db( DS="logbook", yrs = yrs)

fd = l[!is.na(l$lat),]
write.shapefile(fd, 'whelk_logbook', p)
  
nafo <- load.shapefile(DS = 'nafo')
 
l.n = intersect.sp(l, nafo, var = 'zone')
  
#l = l[which(l$yr != 2017),]
l.n$landings = as.numeric(l.n$landings)
  
#l.n = l.n[which(l.n$zone != '3O' & l.n$zone != '3Ps'& l.n$zone != '4T' & l.n$zone != '5Y'& l.n$zone != '4X'& l.n$zone != 'NA'),]
l.n = l.n[which(l.n$zone != '3O' & l.n$zone != '3Ps'& l.n$zone != '4T' & l.n$zone != '4X' & l.n$zone != '4Vn' & l.n$zone != 'NA'),]

zone = unique(l.n$zone)


if(DS %in% c("landings", "complete")) {
    l.r = l.n[which(is.finite(l.n$landings)),]
    l.yr = l.r[, c("yr", "landings", "zone")]
    l.yr$landings = l.yr$landings/1000 #convert kgs to mt
    l.yr <- ddply (l.yr, .(yr, zone), summarize, landings = sum(landings))
    l.yr$yr = as.numeric(l.yr$yr)
    
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    yrange = range (l.yr$landings, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 1)
    xlabels = as.numeric(xlabels)

    outfile = paste("whelk.yearly.landings")
    fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
    
    p1 <- ggplot(l.yr, aes(y=landings, x=yr, group=zone)) + theme_economist() + 
      geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
      geom_point(aes(shape=zone), color = "black", size = 1.5) +
      scale_linetype_manual(values = c("dotted", "dashed", "solid", "solid", "solid")) + 
      scale_shape_manual(values = c(15,16)) +
      theme(legend.position= c(.2, .8),
            legend.text=element_text(size = 10), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
      labs(x = "Year", y = "Landings (mt)")
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    p1
    #print(p1)
    #dev.off()
  }
  
  if(DS %in% c('landings.type', 'complete')){
    
    l.t = l[which(is.finite(l$cpue)),]
    
    for (t in ty) {
      s = l.t[which(l.t$doc_type == t),] #Anything starting with SD is lobster log, MD is crab document
      l.yr = s[, c("yr", "landings", "lfa")]
      l.yr$landings = l.yr$landings/1000
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
      l.yr$yr = as.numeric(l.yr$yr)

      if (t == 'crab.doc'){
        l.yr = l.yr[which(l.yr$lfa != 35 & l.yr$lfa !=36),]
        my.colors = my.colors.crab
      } else {
        l.yr = l.yr[which(l.yr$lfa != 32 & l.yr$lfa !=33),]
        my.colors = my.colors.lobster
      }
      
      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$landings, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 2)
      xlabels = as.numeric(xlabels)
      
      outfile = paste(t, "yearly.landings", sep = ".")
      fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = landings, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position = "right", 
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        scale_color_manual(name = "LFA", values = my.colors) +
        #scale_colour_brewer(palette ='Paired') +
        #scale_colour_discrete(drop=F, limits= levels(lfas)) +
        labs(x = "Year", y="Landings (mt)")
      
      pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      dev.off()
      
    }
  }
  
  if(DS %in% c('effort.type', 'complete')){
    
    e = l[which(is.finite(l$cpue)),]

    for (t in ty) {
      s = e[which(e$doc_type == t),] #Anything starting with SD is lobster log, MD is crab document
      l.yr = s[, c("yr", "effort", "lfa")]
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, effort = sum(effort))
      l.yr$yr = as.numeric(l.yr$yr)

      if (t == 'crab.doc'){
        l.yr = l.yr[which(l.yr$lfa != 35 & l.yr$lfa !=36),]
        my.colors = my.colors.crab
      } else {
        l.yr = l.yr[which(l.yr$lfa != 32 & l.yr$lfa !=33),]
        my.colors = my.colors.lobster
      }
      
      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$effort, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 2)
      xlabels = as.numeric(xlabels)
      
      outfile = paste(t, "yearly.effort", sep = ".")
      fn = file.path( p$mapdir, paste(outfile,"pdf", sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = effort, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position = "right", 
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        #scale_colour_brewer(palette ='Paired') +
        scale_color_manual(name = "LFA", values = my.colors) +
        labs(x = "Year", y="Traps Hauls")
      
      pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      dev.off()
      }
    }
  
  if(DS %in% c('cpue.type', 'complete')){
    
    c = l[which(is.finite(l$cpue)),]

    for (t in ty) {
      s = c[which(c$doc_type == t),] #Anything starting with SD is lobster log, MD is crab document
      l.yr = s[, c("yr", "cpue", "lfa")]
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, cpue = mean(cpue))
      l.yr$yr = as.numeric(l.yr$yr)

      if (t == 'crab.doc'){
        l.yr = l.yr[which(l.yr$lfa != 35 & l.yr$lfa !=36),]
        my.colors = my.colors.crab
      } else {
        l.yr = l.yr[which(l.yr$lfa != 32 & l.yr$lfa !=33),]
        my.colors = my.colors.lobster
      }
      
      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$cpue, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 2)
      xlabels = as.numeric(xlabels)
      
      outfile = paste(t, "yearly.cpue", sep = ".")
      fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = cpue, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position = "right", 
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        scale_color_manual(name = "LFA", values = my.colors) +
        #scale_colour_brewer(palette ='Paired') +
        labs(x = "Year", y="Mean CPUE (kg/trap)")
      
      pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      dev.off()
    }
  }
return(p1)
}