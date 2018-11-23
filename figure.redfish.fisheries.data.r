figure.redfish.fisheries.data = function(DS) {
 #browser()

  p = setup.parameters()
  l = redcrab.logbook.db( DS="logbook")
  l = l[which(l$yr != 2017),]
  l$landings = as.numeric(l$landings)

 if(DS %in% c("landings", "complete")) {
    l.r = l[which(is.finite(l$landings)),]
    l.yr = l.r[, c("yr", "landings")]
    l.yr$landings = l.yr$landings/1000 #convert kgs to mt
    l.yr <- ddply (l.yr, .(yr), summarize, landings = sum(landings))
    l.yr$yr = as.numeric(l.yr$yr)
    
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    yrange = range (l.yr$landings, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 2)
    xlabels = as.numeric(xlabels)
    
    outfile = paste("redfish.yearly.landings")
    fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
    
    p1 <- ggplot() + theme_economist() + 
      geom_line(aes(y = landings, x=yr), size = 0.7, data=l.yr) +
      #geom_point(aes(y = landings, x= yr, colour = zone), size = 2, data = l.yr)+
      theme(legend.position= c(.87, .69),
            legend.text=element_text(size = 10), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      #scale_colour_manual(values=cbPalette) +
      #scale_color_manual(name = "LFA", values = my.colors) +
      #scale_colour_brewer(palette='Paired') +
      labs(x = "Year", y = "Landings (mt)")
    
    pdf(file=fn, width=7.5, height=5, bg='white')
    p1
    print(p1)
    dev.off()
 }
  
  if(DS %in% c('effort.type', 'complete')){
    
    e = l[which(is.finite(l$cpue)),]

    for (t in ty) {
      s = e[which(e$doc_type == t),] #Anything starting with SD is lobster log, MD is crab document
      l.yr = s[, c("yr", "effort", "lfa")]
      l.yr <- ddply (l.yr, .(yr), summarize, effort = sum(effort))
      l.yr$yr = as.numeric(l.yr$yr)

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
      l.yr = s[, c("yr", "cpue")]
      l.yr <- ddply (l.yr, .(yr), summarize, cpue = mean(cpue))
      l.yr$yr = as.numeric(l.yr$yr)

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
}
