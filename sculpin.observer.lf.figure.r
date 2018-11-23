sculpin.observer.lf.figure = function(DS){

p = setup.parameters()

o = hagfish.observer.db(DS = "odbc")

names(o) =  tolower( names(o) )
o$yr = year(o$datelanded)
o$month = month(o$datelanded)
colnames(o)[6] <- "num.at.len"

o = o[, c('year', 'zone', 'lengroup', 'num.at.len')]
o <- ddply(o, .(yr, zone, lengroup), summarize, num.at.len = sum(num.at.len))
o = merge(o, s, by=c('yr', 'zone'))


if (DS %in% c('figure')){

outfile = paste("hagfish.length.frequency")
fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )

p1 = ggplot(data = o, width = 0.8, aes(x=lengroup, y = per.at.len)) + geom_bar(stat = 'identity')+
  geom_vline( color = "grey34", size = 0.59, linetype ="dotted", aes(xintercept = 42.4)) +
  #facet_wrap(yr~zone, strip.position = c('right')) +
  facet_grid(yr~zone) +
  scale_x_continuous(limits = c(20, 70)) +
  theme(legend.text = element_text(size=8), 
        panel.background = element_rect(fill = 'white', color = 'white'),
        plot.background = element_rect(fill = 'white'),
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 6),
        text = element_text(size = 8)) +
  labs(x = "Length", y="Percent at length")

png(filename=fn, width=6, height=5, units="in", res=450)
#pdf(file=fn, width=4, height=3, bg='white')
p1
dev.off()
}


return(p1)
}

