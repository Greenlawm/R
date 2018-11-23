cpue.multiple.year = function(){
  
  p = setup.parameters()
  
  l = process.hagfish.logbook()
  h.z <- load.shapefile(DS = 'nafo')
  l = intersect.sp(l, h.z, var = 'zone')
  l$m.id <- seq.int(nrow(l))
 
  setwd(p$polydir)
  e <- readOGR(".", "hagfish_extract_countyear")
  e = data.frame(e)
  e$count.yr = e$RASTERVALU
  e$m.id = e$m_id
  
  to.extract = c("m.id", "count.yr")
  e = e[, to.extract]
  
  l = merge(l, e, by='m.id')
  
  l.n = l
  l.n = subset(l.n, is.na(zones)|zones == '4w_shelf')
  
  zone = unique(l.n$zones)
  l.n$date_hauled = as.Date(l.n$date_hauled, "%Y-%b-%d")
  l.r = l.n[which(is.finite(l.n$cpue)),]
  
  l.r = l.r[which(l.r$count.yr > 6), ]
  
  l.daily = l.r[, c("yr", "cpue", "zones", "date_hauled")]
  l.yr <- ddply (l.daily, .(yr, zones), summarize, cpue = mean(cpue), date.fished = mean(date_hauled))
  l.daily$date.fished = l.daily$date_hauled
  l.daily$zone = l.daily$zones
  l.daily$yr = l.daily$yr
  l.yr$yr = l.yr$yr
  l.yr$zone = l.yr$zones

  l.avg <- ddply(l.yr, .(zone), summarize, cpue = mean(cpue))
  ref <- l.avg
  ref$cpue <- ref$cpue * 0.5
  
  ld = l.daily[, c('cpue', 'date.fished', 'zone', 'yr')]
  ly = l.yr[, c('cpue', 'date.fished', 'zone', 'yr')]
  
  outfile = paste("hagfish.cpue.multiple.years", sep = ".")
  
  fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  #fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )
  limits = c(0, 75)
  yrange = range (l.yr$cpue, na.rm=T)
  yrange[1] = 0
  #ylabels = c(0, 2, 4, 10, 20, 50, 100)
  ylabels = c(0, 25, 50, 75, 100)
 
  p1 <- ggplot() + 
    geom_point(data= ld,  alpha = 0.4, size = 0.5, aes(x = date.fished, y = cpue)) + 
    geom_line(data = ly, size = 1, aes(x = date.fished, y = cpue)) +
    geom_point(data = ly, size = 2, shape = 21, fill = "grey", aes(x = date.fished, y = cpue)) +
    facet_wrap(~ zone, ncol= 1, strip.position = c('right')) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    #scale_y_log10(breaks = ylabels, labels = ylabels, limits = c(1, 100)) +
    scale_y_continuous(breaks = ylabels, labels = ylabels, limits = limits) +
    theme(legend.text = element_text(size=10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          axis.text.x = element_text(angle = 75, hjust = 1.2),
          text = element_text(size = 9)) +
    theme(axis.line.x = element_line(color="grey", size = 1),
          axis.line.y = element_line(color="grey", size = 1))+
    labs(x = "Year", y="CPUE (kg/trap)")
  #png(filename=fn, width=5, height=5, units="in", res=450)
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  p1
  dev.off()


return(p1)
}
  
