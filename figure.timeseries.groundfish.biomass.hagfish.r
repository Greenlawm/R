figure.timeseries.groundfish.biomass.hagfish = function() {
  
  p = setup.parameters()
  species = c('241') #hagfish
  data.yrs = 1996:2017
  x = calculate.confidence.intervals.strata.wgt(species = species, data.yrs = data.yrs)
  
  #Calculate 3 yr Mean
  x$mean.3.yr <- zoo::rollapply(x$boot.mean, 3, mean, fill=NA, align="right")
  
  #Calculate Mean Lines
  x$median <- median(x$boot.mean)
  x$median.pop <- median(x$pop.total)
  x$median.pop.50 <- median(x$pop.total) * 0.5
  x$median.pop.75 <- median(x$pop.total) * 0.75
  
  x$median.50 <- median(x$boot.mean) * 0.5
  #x$gm.40 <- geometric.mean (x$boot.mean) * 0.4
  
  x$ci.l = x$boot.mean - x$lower.ci
  x$ci.u = x$boot.mean + x$upper.ci
  
  # Mean Number per tow
  
 
  
 
  
  p1 <- ggplot(x, aes(x = year)) +
    geom_point(aes(y = pop.total/1000), colour = "black", shape = 16, size = 2) +
    geom_line(aes(y = pop.total/1000, colour = "a", linetype = "a"), size = 1) +
    geom_line(aes(y = median.pop/1000, colour = "b", linetype = "b"), size = 1) +
    geom_line(aes(y = median.pop.75/1000, colour = "c", linetype = "c"), size = 1) +
    geom_line(aes(y = median.pop.50/1000, colour = "d", linetype = "d"), size = 1) +
    labs( x = "Year", y = "Biomass (tonnes)") +
    scale_linetype_manual(name = "Legend", labels = c("tonnes", "Median", "75% Med", "50% Med"), values = c("solid", "longdash", "longdash", "longdash")) +
    scale_colour_manual(name = "Legend", labels = c("tonnes", "Median", "75% Med", "50% Med"), values = c("grey0", "forestgreen", "gold1", "red")) +
    scale_y_continuous (limits = c(0, (1.05* max(x$pop.total/1000))), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
    scale_x_continuous (breaks = scales::pretty_breaks (n=6)) +
    theme (axis.title = element_text (size = 10, colour = "black"),
           axis.text = element_text (size = 10, colour = "black"),
           legend.text = element_text (size = 10),
           legend.position="top") +
    theme(axis.line = element_line(color="black", size = 1),
          panel.background = element_blank(),
          legend.key=element_blank())
  
  p1
  
}
