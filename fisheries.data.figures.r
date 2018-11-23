fisheries.data.figures = function(DS) {
 #browser()

  p = setup.parameters()
  l =  sslogbook.db( DS="logbook")
  l = l[which(l$yr != 2017),]
  l$lfa[l$lfa == "LOBSTER - GREY ZONE"] <- "Grey Zone"
  l$lfa[l$lfa == "NA"] <- "Unknown"
  l$cpue = as.numeric(l$cpue)
  l$effort = as.numeric(l$effort)
  l$landings = as.numeric(l$landings)
  l$doc_type[which(substr(l$doc_type, 1,2) == 'SD')] <- "lobster.log" #Anything starting with SD is lobster log
  l$doc_type[which(substr(l$doc_type, 1,2) == 'MD')] <- "crab.doc"
  l$doc_type[which(l$doc_type == 'NA')] <- "crab.doc"
  
  #fn.root =  file.path( ss.exdatadirectory)
  #fny = file.path( fn.root, paste("logbook.jonah.rdata", sep="."))  
  #save( l, file=fny, compress=T)
  
  my.colors = brewer.pal(8, "Paired")
  l = l[which(l$lfa != 27 & l$lfa !=28 & l$lfa !=29 & l$lfa !=30 &l$lfa !='31A' & l$lfa !='31B'),]
  price = read.csv("D:\\ss_data\\groundfish\\data\\jonahprices.csv", header = T, sep = ",")
  
  lfas = unique(l$lfa)
  names(my.colors)   = levels(as.factor(lfas))
  my.colors.lobster = c("#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
  my.colors.crab = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#E31A1C", "#FDBF6F", "#FF7F00")
  
  ty <- unique(l$doc_type)
  
  if(DS %in% c("landings", "complete")) {
    l.r = l[which(is.finite(l$landings)),]
    l.yr = l.r[, c("yr", "landings", "lfa")]
    l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
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
    
    outfile = paste("yearly.landings")
    fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
    
    p1 <- ggplot() + theme_economist() + 
      geom_line(aes(y = landings, x=yr, colour=lfa), size = 0.7, data=l.yr) +
      #geom_point(aes(y = landings, x= yr, colour = zone), size = 2, data = l.yr)+
      theme(legend.position= c(.87, .69),
            legend.text=element_text(size = 10), 
            panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
      scale_x_continuous(breaks = xlabels) +
      #scale_colour_manual(values=cbPalette) +
      scale_color_manual(name = "LFA", values = my.colors) +
      #scale_colour_brewer(palette='Paired') +
      labs(x = "Year", y = "Landings (mt)")
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    #p1
    #print(p1)
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