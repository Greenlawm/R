fisheries.timeseries.groundfish.withCI = function(DS, species, data.yrs) {

p = setup.parameters()

  x = calculate.confidence.intervals.strata(species = species, data.yrs = data.yrs)

  #Calculate 3 yr Mean
  x$mean.3.yr <- zoo::rollapply(x$boot.mean, 3, mean, fill=NA, align="right")

  #Calculate Mean Lines
  x$median <- median(x$boot.mean)
  x$median.50 <- median(x$boot.mean) * 0.5
  #x$gm.40 <- geometric.mean (x$boot.mean) * 0.4

  x$ci.l = x$boot.mean - x$lower.ci
  x$ci.u = x$boot.mean + x$upper.ci

# Mean Number per tow

  p2 <- ggplot(x, aes(x = year)) +
    geom_point(aes(y = boot.mean), colour = "black", shape = 16) +
    geom_line(aes(y = boot.mean, colour = "a", linetype = "a")) +
    #geom_line(aes(y = mean.3.yr, colour = "a", linetype = "b")) +
    geom_line(aes(y = median, colour = "b", linetype = "b")) +
    #geom_line(aes(y = median.50, colour = "c", linetype = "c")) +
    geom_errorbar(aes(ymin = ci.l, ymax = ci.u ), colour = "grey", width = 0.2) +
    labs( x = "Year", y = "Mean numer per tow") +
    scale_linetype_manual( name = "", labels = c("Mean (3 yr)", "Median"),
                           values = c("solid", "longdash", "dotted")) +
    scale_colour_manual(name = "", labels = c ("Mean (3 yr)  ","Median" ),
                        values = c("grey0", "dodgerblue1", "black")) +
    scale_y_continuous (limits = c(0, (1.05*max (x$ci.u))), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
    scale_y_continuous (limits = c(0, 2), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
    scale_x_continuous (breaks = scales::pretty_breaks (n=6)) +
    theme (axis.title = element_text (size = 10, colour = "black"),
           axis.text = element_text (size = 8, colour = "black"),
           legend.text = element_text (size = 8),
           legend.position="top") +
    theme(axis.line = element_line(color="black", size = 0.5),
          panel.background = element_blank(),
          legend.key=element_blank())
    
    p2
    
}
