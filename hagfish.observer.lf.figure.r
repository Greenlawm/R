hagfish.observer.lf.figure = function(DS){

p = setup.parameters()

o = hagfish.observer.db(DS = "odbc.port")
l = hagfish.observer.db(DS = "odbc")

names(o) =  tolower( names(o) )
names(l) =  tolower( names(l) )

o$zone[o$description =="4V"|  o$description =='4W F'| o$description == '4W G'] <- "Offshore"
o$zone[o$description =="4W H"|  o$description =='4W K'| o$description == '4W L'| o$description == '4X M'| 
         o$description == '4X N'|o$description == '4X O'|o$description == '4W'] <- "Midshore"

o$zone[o$description =="4X P"|  o$description =='4X Q'| o$description == '5ZE J'] <- "GOM"

l$zone[l$nafarea_id =="4VC"|  l$nafarea_id =='4VS'|  l$nafarea_id =='4WL'] <- "Offshore"
l$zone[l$nafarea_id =="4WE"|  l$nafarea_id =='4WH'|  l$nafarea_id =='4WK'|  l$nafarea_id =='4WN'|  
         l$nafarea_id =='4WO'| l$nafarea_id =='4XO'| l$nafarea_id =='4XN'] <- "Midshore"
l$zone[l$nafarea_id =="4XQ"] <- "GOM"

o$yr = year(o$datelanded)
o$month = month(o$datelanded)
colnames(o)[6] <- "num.at.len"
o = o[which(o$yr > '2001'),]

#l$yr = l$year
#l$num.at.len = l$num_at_length
#l$lengroup = l$fish_length
if (DS %in% c("freq.table")){
  
 p1 = table(o$yr, o$description)
 
}

#l = l[, c('yr', 'zone', 'lengroup', 'num.at.len')]
o = o[, c('yr', 'zone', 'lengroup', 'num.at.len')]

#o = rbind(o, l)

o <- ddply(o, .(yr, zone, lengroup), summarize, num.at.len = sum(num.at.len))
s <- ddply(o, .(yr, zone), summarize, tot = sum(num.at.len))
o = merge(o, s, by=c('yr', 'zone'))
t = o[, c('yr', 'zone', 'tot')]
t = unique(t)
t = as.data.table(t)

t = dcast(j, zone+licence_id~yr, fun.aggregate = sum)
t = t[order(t$zone), ]

s <- ddply(o, .(yr, zone), summarize, min = min(lengroup), max = max(lengroup))
s <- ddply(s, .(zone), summarize, min = min(min), max = max(max))

s <- ddply(o, .(yr, zone, tot), summarize, avg = (lengroup * num.at.len))
s <- ddply(s, .(yr, zone), summarize, avg = sum(avg)/tot)
s = unique(s)
s <- ddply(s, .(zone), summarize, avg = mean(avg))
s <- ddply(s, .(), summarize, avg = mean(avg))
o$per.at.len = round((o$num.at.len/o$tot) * 100, 2)

s



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

#png(filename=fn, width=6, height=5, units="in", res=450)
#pdf(file=fn, width=4, height=3, bg='white')
p1
#dev.off()
}

if (DS %in% c('figure.maturity')){
  outfile = paste("hagfish.maturity")
  fn = file.path( p$mapdir, paste(outfile,"png",sep="." ) )

t = o
t$maturity[t$lengroup <= 42 ] <- 0
t$maturity[t$lengroup > 42] <- 1

t$market[t$lengroup <= 17 ] <- 0
t$market[t$lengroup > 17] <- 1
t = t[which(t$market == 0), ]

t = ddply(t, .(yr, zone, tot, maturity), summarize, num.at.len = sum(num.at.len))
t$per.at.len = t$num.at.len/t$tot
t
t.2014 = t[which(t$yr >= 2014), ]
t.s = ddply(t, .(zone, maturity), summarize, per.at.len = mean(per.at.len))
t.s.2014 = ddply(t.2014, .(zone, maturity), summarize, per.at.len = mean(per.at.len))

t$yr = as.numeric(t$yr)
uyrs = unique(t$yr) 
uyrs = as.numeric(uyrs)
xrange = range(uyrs)
xrange[1] = xrange[1]
xrange[2] = xrange[2]
xlabels = seq(xrange[1], xrange[2], 1)
xlabels = as.numeric(xlabels)

t = t[which(t$maturity == 1),]

p1 <- ggplot(t, aes(y=per.at.len, x=yr, group=zone)) + theme_economist() + 
  geom_line(aes(linetype = zone), colour = "black", size = 0.5) +
  geom_point(aes(shape=zone), color = "black", size = 1.5) +
  scale_linetype_manual(values = c("twodash", "dotted", "dashed")) + 
  scale_shape_manual(values = c(1,17,15)) +
  theme(legend.position= c(.95, .75),
        legend.text=element_text(size = 10), 
        panel.background = element_rect(fill = 'white', color = 'white'),
        plot.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = 'grey85')) +
  scale_x_continuous(breaks = xlabels) +
  theme(axis.text.x = element_text (angle = 65, hjust = 0)) +
  labs(x = "Year", y = "Percent Mature")
png(filename=fn, width=7.5, height=5, units="in", res=450)

#pdf(file=fn, width=7.5, height=5, bg='white')
p1
dev.off()

}

return(p1)
}

