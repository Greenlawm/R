jonah.fisheries.data.figures = function(DS) {
 #browser()

  p = setup.parameters()
  l =  jonah.logbook.db( DS="logbook")
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

  #fn.root =  file.path( ss.exdatadirectory)
  #fny = file.path( fn.root, paste("logbook.jonah.rdata", sep="."))  
  #save( l, file=fny, compress=T)
  
  my.colors = brewer.pal(8, "Paired")
  #l = l[which(l$lfa != 27 & l$lfa !=28 & l$lfa !=29 & l$lfa !=30 &l$lfa !='31A' & l$lfa !='31B'),]
  l = l[which(l$lfa != 27 & l$lfa !=28 & l$lfa !=29 & l$lfa !=30 &l$lfa !='31A' & l$lfa !='31B'& l$lfa !='41'
              & l$lfa !=32 & l$lfa !=36 & l$lfa !=35),]
  #price = read.csv("D:\\ss_data\\groundfish\\data\\jonahprices.csv", header = T, sep = ",")
  
  lfas = unique(l$lfa)
  names(my.colors)   = levels(as.factor(lfas))
  my.colors.lobster = c("#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
  my.colors.crab = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#E31A1C", "#FDBF6F", "#FF7F00")
  
  ty <- unique(l$f.type)
  
  if(DS %in% c("landings", "complete")) {
    l.r = l[which(is.finite(l$landings)),]
    l.yr = l.r[, c("yr", "landings", "lfa")]
    l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    l.yr$yr = as.numeric(l.yr$yr)
    
    
    l.r = l[which(is.finite(l$landings)),]
    l.yr = l.r[, c("yr", "landings", "lfa")]
    #l.yr$landings = l.yr$landings/1000
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
    l.yr$yr = as.numeric(l.yr$yr)
    j = as.data.table(l.yr)  
    t = dcast(j, lfa~yr, fun.aggregate = sum)
    t = t[order(t$lfa), ]
    
    
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
      theme(legend.position= c(.77, .72),
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
    p1
    #print(p1)
    #dev.off()
  }
  
  if(DS %in% c('landings.directed', 'complete')){
    l.r = l[which(is.finite(l$landings)),]
    s = l.r[which(l.r$f.type == 'directed'),] 
      l.yr = s[, c("yr", "landings", "lfa")]
      l.yr$landings = l.yr$landings/1000
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
      l.yr$yr = as.numeric(l.yr$yr)

      l.yr = l.yr[which(l.yr$lfa != 35 & l.yr$lfa !=36),]
      my.colors = my.colors.crab
      
      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$landings, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 1)
      xlabels = as.numeric(xlabels)
      
      #outfile = paste(t, "yearly.landings", sep = ".")
      #fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = landings, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position= c(.77, .72),
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              axis.text.x = element_text(angle = 65, hjust = 0),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        scale_color_manual(name = "LFA", values = my.colors) +
        #scale_colour_brewer(palette ='Paired') +
        #scale_colour_discrete(drop=F, limits= levels(lfas)) +
        labs(x = "Year", y="Landings (mt)")
      
      #pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      #dev.off()
      
    }

  
  if(DS %in% c('landings.bycatch', 'complete')){
    l.r = l[which(is.finite(l$landings)),]
    
    s = l.r[which(l.r$f.type == 'bycatch'),] 
      l.yr = s[, c("yr", "landings", "lfa")]
      l.yr$landings = l.yr$landings/1000
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, landings = sum(landings))
      l.yr$yr = as.numeric(l.yr$yr)
      l.yr = l.yr[which(l.yr$lfa != 32 & l.yr$lfa !=33),]
      #my.colors = my.colors.crab

      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$landings, na.rm=T)
      yrange[1] = 0
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 1)
      xlabels = as.numeric(xlabels)
      
      #outfile = paste(t, "yearly.landings", sep = ".")
      #fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = landings, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position= c(.77, .72),
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              axis.text.x = element_text(angle = 65, hjust = 0),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        scale_color_manual(name = "LFA", values = my.colors) +
        #scale_colour_brewer(palette ='Paired') +
        #scale_colour_discrete(drop=F, limits= levels(lfas)) +
        labs(x = "Year", y="Landings (mt)")
      
      #pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      #dev.off()
      
    }
  
  if(DS %in% c('effort.type', 'complete')){
    
    e = l[which(is.finite(l$cpue)),]

    for (t in ty) {
      s = e[which(e$f.type == t),] 
      l.yr = s[, c("yr", "effort", "lfa")]
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, effort = sum(effort))
      l.yr$yr = as.numeric(l.yr$yr)

      if (t == 'directed'){
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
      
      #outfile = paste(t, "yearly.effort", sep = ".")
      #fn = file.path( p$mapdir, paste(outfile,"pdf", sep="." ) )
      
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
      
      #pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      #dev.off()
      }
  }
  
  
  if(DS %in% c('effort.monthly', 'complete')){
    
    e = l[which(is.finite(l$effort)),]
    e = e[which(e$f.type == 'directed'),] 
    e$month = month(e$date.fished)
    e = e[which(e$effort > 10) ,]
    
    l.yr = e[, c("yr", "effort", "lfa", "month")]
    
    y =  sort(unique(l.yr$yr))
    y.1 = max(y)
    l.2017 = l.yr[which(l.yr$yr == y.1), ]
    
    l.yr <- ddply (l.yr, .(yr, lfa, month), summarize, effort = sum(effort))
    l.yr <- ddply (l.yr, .(lfa, month), summarize, effort = mean(effort))
    
    l.2017 <- ddply (l.2017, .(yr, lfa, month), summarize, effort = sum(effort))
    
    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    xrange = range(l.yr$month)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 1)
    xlabels = as.numeric(xlabels)
    
    p1 <- ggplot() + 
      geom_bar(data= l.yr,  stat = "identity", color = "black", aes(x = month, y = effort)) + 
      geom_bar(data= l.2017,  stat = "identity", color = "black", fill ="steelblue", aes(x = month, y = effort)) + 
      theme_economist() + 
      facet_wrap(~ lfa, ncol= 1) +
      theme(panel.background = element_rect(fill = 'white', color = 'white'),
            plot.background = element_rect(fill = 'white'),
            axis.text.x = element_text(angle = 65, hjust = 0),
            text = element_text(size = 9)) +
      scale_x_continuous(breaks = xlabels) +
      
      labs(x = "Month", y="Effort (trap hauls)")
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    print(p1)
    #dev.off()
  
  }
  
  if(DS %in% c('cpue.directed', 'complete')){
    
    c = l[which(is.finite(l$cpue)),]
    
    #c.test = s[which(s$yr == '2017'), ]
    #table(c.test$effort)
    #s = s[order(-s$cpue), ]

    s = c[which(c$f.type == 'directed'),] 
    s = s[which(s$effort > 10) ,]
    
      l.yr = s[, c("yr", "cpue", "lfa")]
      l.yr <- ddply (l.yr, .(yr, lfa), summarize, cpue = mean(cpue))
      l.yr$yr = as.numeric(l.yr$yr)

      l.yr = l.yr[which(l.yr$lfa != 35 & l.yr$lfa !=36),]

      uyrs = unique(l.yr$yr) 
      uyrs = as.numeric(uyrs)
      
      yrange = range (l.yr$cpue, na.rm=T)
      yrange[1] = 0
      yrange[2] = 60
      xrange = range(uyrs)
      xrange[1] = xrange[1]
      xrange[2] = xrange[2]
      xlabels = seq(xrange[1], xrange[2], 1)
      xlabels = as.numeric(xlabels)
      
      #outfile = paste(t, "yearly.cpue", sep = ".")
      #fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
      
      p1 <- ggplot() + theme_economist() + 
        geom_line(aes(y = cpue, x=yr, colour=lfa), size = 0.7, data=l.yr) +
        #geom_point(aes(y = landings, x=yr, colour=zone), size =2, data=l.yr) +
        theme(legend.position = "right", 
              legend.text = element_text(size=10), 
              panel.background = element_rect(fill = 'white', color = 'white'),
              plot.background = element_rect(fill = 'white'),
              axis.text.x = element_text(angle = 65, hjust = 0),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
        scale_x_continuous(breaks = xlabels) +
        scale_color_manual(name = "LFA", values = my.colors) +
        #scale_colour_brewer(palette ='Paired') +
        labs(x = "Year", y="Mean CPUE (kg/trap)")
      
      #pdf(file=fn, width=7.5, height=5, bg='white')
      print(p1)
      #dev.off()
   
  }
  
  if(DS %in% c('cpue.daily', 'complete')){
    
    c = l[which(is.finite(l$cpue)),]
    
    #c.test = s[which(s$yr == '2017'), ]
    #table(c.test$effort)
    #s = s[order(-s$cpue), ]
    
    s = c[which(c$f.type == 'directed'),] 
    s = s[which(s$effort > 10) ,]
    s = s[which(s$cpue < 150) ,]
    
    s = s[which(s$lfa != 35 & s$lfa !=36),]
    
    l.daily = s[, c("yr", "cpue", "lfa", "date.fished")]
    l.yr <- ddply (l.daily, .(yr, lfa), summarize, cpue = mean(cpue), date.fished = mean(date.fished))
    l.yr$yr = as.numeric(l.yr$yr)
    
    yrange = range (l.yr$cpue, na.rm=T)
    yrange[1] = 0
    yrange[2] = 60
    #ylabels = c(0, 2, 4, 10, 20, 50, 100)
    ylabels = c(0, 2, 4, 6, 8, 10, 15, 20, 50, 100)
    
    #outfile = paste(t, "yearly.cpue", sep = ".")
    #fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
    ld = l.daily
    ly = l.yr

    p1 <- ggplot() + 
    geom_point(data= ld,  alpha = 0.4, size = 0.5, aes(x = date.fished, y = cpue)) + 
    geom_line(data = ly, size = 1, aes(x = date.fished, y = cpue)) +
    geom_point(data = ly, size = 2, shape = 21, fill = "grey", aes(x = date.fished, y = cpue)) +
    theme_economist() + 
    facet_wrap(~ lfa, ncol= 1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    #scale_y_log10(breaks = ylabels, labels = ylabels, limits = c(1, 100)) +
    scale_y_continuous(breaks = ylabels, labels = ylabels, limits = c(0, 15)) +
    theme(legend.text = element_text(size=10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 65, hjust = 0),
          text = element_text(size = 9)) +
    labs(x = "Year", y="CPUE (kg/trap)")
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    print(p1)
    #dev.off()
    
  }
  
  
  
  
  if(DS %in% c('cpue.bycatch', 'complete')){
    
    c = l[which(is.finite(l$cpue)),]
    
    s = c[which(c$f.type == 'bycatch'),] #Anything starting with SD is lobster log, MD is crab document
    l.yr = s[, c("yr", "cpue", "lfa")]
    l.yr <- ddply (l.yr, .(yr, lfa), summarize, cpue = mean(cpue))
    l.yr$yr = as.numeric(l.yr$yr)
    
    l.yr = l.yr[which(l.yr$lfa != 32 & l.yr$lfa !=33),]

    uyrs = unique(l.yr$yr) 
    uyrs = as.numeric(uyrs)
    
    yrange = range (l.yr$cpue, na.rm=T)
    yrange[1] = 0
    xrange = range(uyrs)
    xrange[1] = xrange[1]
    xrange[2] = xrange[2]
    xlabels = seq(xrange[1], xrange[2], 2)
    xlabels = as.numeric(xlabels)
    
    #outfile = paste(t, "yearly.cpue", sep = ".")
    #fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
    
    
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
    
    #pdf(file=fn, width=7.5, height=5, bg='white')
    print(p1)
    #dev.off()
    
  }
  return(p1)
}