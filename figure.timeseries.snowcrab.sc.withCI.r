fisheries.timeseries.snowcrab.sc.withCI = function(DS) {

p = setup.parameters()

  x = calculate.confidence.intervals.sc(area = '4W')
  y = calculate.confidence.intervals.sc(area = '4Vs')
  
  xy = cbind(x, y)
  
  #Calculate 3 yr Mean
  x$mean.3.yr <- zoo::rollapply(x$hab3.mean, 3, mean, fill=NA, align="right")

  #Calculate Mean Lines
  x$median <- median(x$hab3.mean)
  x$median.biomass <- median(x$biomass.tot)
  x$median.50 <- median(x$hab3.mean) * 0.5
  #x$gm.40 <- geometric.mean (x$boot.mean) * 0.4
  
  x$ci.l = x$biomass.tot - x$lower.ci.btot
  x$ci.u = x$biomass.tot + x$upper.ci.btot
  
  y$ci.l = y$biomass.tot - y$lower.ci.btot
  y$ci.u = y$biomass.tot + y$upper.ci.btot
  
  
  #Biomass estimates 4W
  tac.4w <- data.frame()
  t4w.01 <- (x$biomass.tot * 0.001) * 0.01 #TAC with a 1% exploitation ratio.
  t4w.ci.u <- (((x$biomass.tot + x$ci.u) * 0.001) *0.01)
  t4w.ci.l <- (((x$biomass.tot - x$ci.l) * 0.001) *0.01)
  t4w.er <- 1600/(x$biomass.tot * 0.001)
  
  t4w = cbind(t4w.01, t4w.ci.l, t4w.ci.u)
  t4w
  
  #Mortality rates
  mmax = 0.1
  mmin = 3/50 #3/Tmax (lifespan) # simulate this with a max age from 40-100 years old

  #Fishing at Maximum Constant Yield
  tac.4Vs <- data.frame()
  t4vs.01<- (y$biomass.tot * 0.001) * 0.01 #TAC with a 1% exploitation ratio.
  t4vs.ci.u <- (((y$biomass.tot + y$ci.u) * 0.001) *0.01)
  t4vs.ci.l <- (((y$biomass.tot - y$ci.l) * 0.001) *0.01)
  t4vs.er <- 800/(y$biomass.tot * 0.001)
  
  t4vs = cbind(t4vs.01, t4vs.ci.l, t4vs.ci.u)
  t4vs
  
  #Sustainable Fishing Mortality
  #-----------------------------------------------
  #x = 0.2-0.3 for fisheries that have little to no monitoring to fishing, can be conservative
  
  #4VS
  MCY.max.4vs = 0.3* mmax * (y$biomass.tot * 0.001) #xMB0, maximum constant yield
  MCY.max.4vs
  
  
  MCY.min.4vs = 0.3 * mmin * (y$biomass.tot * 0.001)
  MCY.min.4vs
  
  #4W
  MCY.max.4w = 0.3* mmax * (x$biomass.tot * 0.001) #xMB0, maximum constant yield
  MCY.max.4w
  
  
  MCY.min.4w = 0.3 * mmin * (x$biomass.tot * 0.001)
  MCY.min.4w
  
  
# Plot of Mean Number per tow
#-----------------------------------------------------------
     p2 <- ggplot(x, aes(x = year)) +
      geom_point(aes(y = hab3.mean/1000000), colour = "black", shape = 16) +
      geom_line(aes(y = hab3.mean/1000000, colour = "a", linetype = "a")) +
      geom_line(aes(y = hab2.mean/1000000, colour = "a", linetype = "b")) +
      geom_line(aes(y = hab1.mean/1000000, colour = "a", linetype = "b")) +
      #geom_line(aes(y = mean.3.yr, colour = "a", linetype = "b")) +
      geom_line(aes(y = median/1000000, colour = "b", linetype = "b")) +
      geom_line(aes(y = median.50/1000000, colour = "c", linetype = "c")) +
      #geom_errorbar(aes(ymin = ci.l, ymax = ci.u ), colour = "grey", width = 0.2) +
      labs( x = "Year", y = "Kg/sq.m") +
      scale_linetype_manual( name = "", labels = c("Mean (3 yr)", "Median"),
                             values = c("solid", "longdash", "dotted")) +
      scale_colour_manual(name = "", labels = c ("Mean (3 yr)  ","Median" ),
                          values = c("grey0", "dodgerblue1", "black")) +
      #scale_y_continuous (limits = c(0, (1.05*max (x$ci.u))), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
      scale_y_continuous (limits = c(0, 0.23), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
      scale_x_continuous (breaks = scales::pretty_breaks (n=6)) +
      theme (axis.title = element_text (size = 10, colour = "black"),
             axis.text = element_text (size = 8, colour = "black"),
             legend.text = element_text (size = 8),
             legend.position="top") +
      theme(axis.line = element_line(color="black", size = 0.5),
            panel.background = element_blank(),
            legend.key=element_blank())
    
    p2

  #Biomass
  #----------------------------------------------------
    p3 <- ggplot(x, aes(x = year)) +
      geom_point(aes(y = biomass.tot * 0.001), colour = "black", shape = 16) +
      geom_line(aes(y = biomass.tot * 0.001, colour = "a", linetype = "b")) +
      geom_point(aes(y = biomass.tot * 0.001), colour = "black", shape = 16) +
      geom_line(aes(y = biomass.tot * 0.001, colour = "a", linetype = "b")) +
      geom_line(aes(y = median.biomass/0.001, colour = "b", linetype = "b")) +
      geom_errorbar(aes(ymin = ci.l * 0.001, ymax = ci.u * 0.001 ), colour = "grey", width = 0.2) +
      labs( x = "Year", y = "mt") +
      scale_linetype_manual( name = "", labels = c("Mean (3 yr)", "Median"),
                             values = c("solid", "longdash", "dotted")) +
      scale_colour_manual(name = "", labels = c ("Mean (3 yr)  ","Median" ),
                          values = c("grey0", "dodgerblue1", "black")) +
      scale_y_continuous (limits = c(0, (1.05 * max (x$ci.u* 0.001))), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
      #scale_y_continuous (limits = c(0, 0.23), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
      scale_x_continuous (breaks = scales::pretty_breaks (n=6)) +
      theme (axis.title = element_text (size = 10, colour = "black"),
             axis.text = element_text (size = 8, colour = "black"),
             legend.text = element_text (size = 8),
             legend.position="top") +
      theme(axis.line = element_line(color="black", size = 0.5),
            panel.background = element_blank(),
            legend.key=element_blank())
    
    p3
    
}
