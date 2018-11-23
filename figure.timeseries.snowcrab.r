fisheries.timeseries.snowcrab = function(DS) {
  
  p = setup.parameters()
  #species = c('2511') #Jonah Crab
  #c('4211', '4210', '1510') #whelk, c('25') #tilefish
  species = c('6600', '6611') #sea cucumber
  data.yrs = 2005:2016
  
  
  #Extract Cat data from groundfish survey 
  file <- paste("C:/ss/data/snowcrab/snowcrabbycatch.csv", sep=" ")
  #read file
  k <- read.table(file, header=TRUE, sep=',', row.names = NULL)
  
  j = survey.process.snowcrab(k, species)
  
  hab <- load.shapefile(DS= 'sc.habitat')
  fa <- load.shapefile(DS = 'sea_cucumber_fa')
  
  
  #write.shapefile(j, 'seacucumber_groundfish', p)
  
  #Time series of total weight of species caught per year  
  #-----------------------------------------------------------
  g.r = j[which(is.finite(j$totno_sd)),]
  g.r = g.r[, c("yr", "totno_sd")]
  
  g.yr <- ddply (g.r, .(yr), summarize, wgt = gm.mean(totno_sd))
  
  g.yr$yr = as.numeric(g.yr$yr)
  
  uyrs = unique(g.yr$yr) 
  uyrs = as.numeric(uyrs)
  
  yrange = range (g.yr$wgt, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1], xrange[2], 2)
  xlabels = as.numeric(xlabels)
  
  my.colors = colorRampPalette(brewer.pal(9, "Paired"))
  
  outfile = paste('sc.yearly.weight.snowcrab')
  fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  
  #-------------------------------------------------------------------------
  #Time sereis of total weight of species caught per year
  
  p1 <- ggplot() + theme_economist() + 
    geom_line(aes(y = wgt, x=yr), size = 1, data=g.yr) +
    #geom_point(aes(y = landings, x= yr, colour = zone), size = 2, data = l.yr)+
    theme(legend.position= c(.87, .69),
          legend.text=element_text(size = 10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
    scale_x_continuous(breaks = xlabels) +
    #scale_colour_manual(values=cbPalette) +
    scale_color_manual(name = "LFA", values = my.colors(lfas)) +
    theme(legend.position="right") +
    guides(fill=guide_legend(nrow=2))+
    #scale_colour_brewer(palette='Paired') +
    labs(x = "Year", y = "Total Number")
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  #p1
  print(p1)
  dev.off()
}
  