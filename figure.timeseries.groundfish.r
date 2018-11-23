fisheries.timeseries.groundfish = function(DS) {

  p = setup.parameters()
  #species = c('2511') #Jonah Crab
  #c('4211', '4210', '1510') #whelk, c('25') #tilefish
  species = c('6600', '6611') #sea cucumber
  
  #Extract Cat data from groundfish survey 
  k = groundfish.db(DS="cat.base")
  data.yrs = 2000:2017
  j = survey.process(k, species)
  
  write.shapefile(j, 'seacucumber_groundfish', p)
  
 #Time sereis of total weight of species caught per year
 #--------------------------------------------------------- 
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
    labs(x = "Year", y = "Catch (kg)")
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  #p1
  print(p1)
  dev.off()
  #-----------------------------------------------------------------------------------------
  
  #Import LFAs
  # y = load.shapefile( DS = "lfas")
  # gl = intersect.sp.groundfish(x = j, y = y)
  #j = merge(j, gl)
  
  #Time series of total weight of species caught per year or per year/per lfa  
  #-----------------------------------------------------------
  g.r = j[which(is.finite(j$totwgt_sd)),]
  #g.r = g.r[, c("yr", "totwgt_sd", "lfa_nafo")]
  g.r = g.r[, c("yr", "totwgt_sd")]
  
  #g.yr <- ddply (g.r, .(yr, lfa_nafo), summarize, wgt = sum(totwgt_sd))
  g.yr <- ddply (g.r, .(yr), summarize, wgt = sum(totwgt_sd))
  
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
  
  #g.yr = g.yr[which(g.yr$lfa_nafo != 27 & g.yr$lfa_nafo !=28 & g.yr$lfa_nafo !=29 & g.yr$lfa_nafo !=30 &
  #                    g.yr$lfa_nafo !='31A' & g.yr$lfa_nafo !='31B'& g.yr$lfa_nafo !=32 & g.yr$lfa_nafo !='31B'&
  #                  g.yr$lfa_nafo !='4Vs-41'& g.yr$lfa_nafo !='4W-41'), ]
  
  #g.offshore = g.yr[which(g.yr$lfa_nafo != 33 & g.yr$lfa_nafo !=34 & g.yr$lfa_nafo !=35 & g.yr$lfa_nafo !=36 &
  #                         g.yr$lfa_nafo != 37 & g.yr$lfa_nafo !=38),]
  #g.inshore = g.yr[which(g.yr$lfa_nafo != '4X-40' & g.yr$lfa_nafo != '4Xn-41' & g.yr$lfa_nafo != '4Xs-41' 
  #                       & g.yr$lfa_nafo !='5Z-41'),]
  
  #lfas = length(unique(g.offshore$lfa_nafo))
  my.colors = colorRampPalette(brewer.pal(9, "Paired"))
  
  #outfile = paste("yearly.weight.offshore")
  outfile = paste('sc.yearly.weight')
  fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  
  #--------------------------------------------------
  p1 <- ggplot() + theme_economist() + 
    geom_line(aes(y = wgt, x=yr, colour=lfa_nafo), size = 1, data=g.offshore) +
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
    labs(x = "Year", y = "Catch (kg)")
  #----------------------------------------------------------------
  
  lfas = length(unique(g.inshore$lfa_nafo))
  my.colors = colorRampPalette(brewer.pal(9, "Paired"))
  
  outfile = paste("yearly.weight.inshore")
  fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  
  p1 <- ggplot() + theme_economist() + 
    geom_line(aes(y = wgt, x=yr, colour=lfa_nafo), size = 1, data=g.inshore) +
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
    labs(x = "Year", y = "Catch (kg)")
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  #p1
  print(p1)
  dev.off()
  
  
  #-------------------------------------------------------------------------
  
#Design weighted area occupied (km2) of the Jonah Crab from the DFO RV Survey
#Changes in the breadth of habitat use. (n)Number of survey tows in a year, (y) is the 
#number of lobster caught in a tow (i). (a) is the areas of the stratum fished, 
#for tow i divided by the number of sets fished in that stratum. Units are in km2
#sum(number of tows with species > 1/total number of tows in that strata * (area of stratum))

j$dwao[j$totwgt_sd > 0] <- 1
j$dwao[j$totwgt_sd == 0] <- 0

#-------------------------------------------------------------------
#For all Strata

sarea =  groundfish.db( DS ="gsstratum")
sarea = sarea[ , c("strat", "area")]

jc = count(j, .(strat, yr))
js <- ddply(j, .(strat, yr), summarize, dwaos = sum(dwao))

jm = merge(js, jc, by=c("strat", "yr"))
jm$dwaop = jm$dwaos/jm$freq

jarea = merge(jm, sarea, by="strat")
jarea$dwao = jarea$dwaop * as.numeric(jarea$area)

jas = ddply(jarea, .(yr), summarize, dwao = sum(dwao))

uyrs = unique(jas$yr) 
uyrs = as.numeric(uyrs)

yrange = range (jas$dwao, na.rm=T)
yrange[1] = 0
xrange = range(uyrs)
xrange[1] = xrange[1]
xrange[2] = xrange[2]
xlabels = seq(xrange[1], xrange[2], 2)
xlabels = as.numeric(xlabels)

outfile = paste("dwao.seacucumber")
fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )

p1 <- ggplot() + theme_economist() + 
  geom_line(aes(y = dwao, x=yr), size = 1, data=jas) +
  theme(legend.position= c(.87, .69),
        legend.text=element_text(size = 10), 
        panel.background = element_rect(fill = 'white', color = 'white'),
        plot.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
  scale_x_continuous(breaks = xlabels) +
  theme(legend.position="right") +
  guides(fill=guide_legend(nrow=2))+
  labs(x = "Year", y = "DWAO (km2)")

pdf(file=fn, width=7.5, height=5, bg='white')
#p1
print(p1)
dev.off()


#----------------------------------------------------------
#For each LFA

lfa_area = read.table("D:\\ss_data\\groundfish\\data\\local\\strata_lfa_area.csv", header=T, sep="," )
names(lfa_area) =  tolower( names(lfa_area) )

lfa_area$areaid = paste(lfa_area$lfa_nafo, lfa_area$strataid, sep="_")
j$areaid = paste(j$lfa_nafo, j$strat, sep="_")

jc = count(j, .(areaid, yr))
js <- ddply(j, .(areaid, yr), summarize, dwaos = sum(dwao))

jm = merge(js, jc, by=c("areaid", "yr"))
jm$dwaop = jm$dwaos/jm$freq

jarea = merge(jm, lfa_area, by="areaid")
jarea$dwao = jarea$dwaop * as.numeric(jarea$area_sk)

jas = ddply(jarea, .(yr, lfa_nafo), summarize, dwao = sum(dwao), dwao_max = sum(as.numeric(area_sk)))
avg = ddply(jas,. (lfa_nafo), summarize, avg = mean(dwao))
lfas = unique(jas$lfa_nafo)

uyrs = unique(jas$yr) 
uyrs = as.numeric(uyrs)

for (l in lfas) {
  
  j.s = jas[which(jas$lfa_nafo == l),]
  j.avg = avg[which(avg$lfa_nafo == l), 2 ]
  
  yrange = range (jas$dwao, na.rm=T)
  yrange[1] = 0
  xrange = range(uyrs)
  xrange[1] = xrange[1]
  xrange[2] = xrange[2]
  xlabels = seq(xrange[1], xrange[2], 2)
  xlabels = as.numeric(xlabels)
  
  outfile = paste(l, "dwao.jonah", sep = ".")
    fn = file.path( p$mapdir, paste(outfile,"pdf",sep="." ) )
  
  p1 <- ggplot(data=j.s, aes(yr)) + theme_economist() + 
    geom_line(aes(y = dwao), size = 1) +
    theme(legend.position= c(.87, .69),
          legend.text=element_text(size = 10), 
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
    scale_x_continuous(breaks = xlabels) +
    theme(legend.position="right") +
    guides(fill=guide_legend(nrow=2))+
    geom_line(aes(y=dwao_max), linetype ="dashed", colour="blue", size = 0.4) +
    geom_hline(yintercept = j.avg, linetype="dotted", color = "grey34", size=0.59) +
    labs(x = "Year", y = "DWAO (km2)")
  
  pdf(file=fn, width=7.5, height=5, bg='white')
  #p1
  print(p1)
  dev.off()
  }
}