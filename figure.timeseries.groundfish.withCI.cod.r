figure.timeseries.groundfish.withCI.cod = function() {

p = setup.parameters()
species = c('10') #cod
data.yrs = 1983:2017

x = calculate.ci.strata.wgt(species = species, data.yrs = data.yrs)

x$ci.l = x$boot.mean - x$lower.ci
x$ci.u = x$boot.mean + x$upper.ci

# Mean Number per tow

  p2 <- ggplot(x, aes(x = year)) +
    geom_point(aes(y = boot.mean), colour = "black", shape = 16) +
    geom_line(aes(y = boot.mean, colour = "a", linetype = "a"), size = 1) +
    geom_line(aes(y = median, colour = "b", linetype = "b"), size = 1) +
    geom_errorbar(aes(ymin = ci.l, ymax = ci.u ), colour = "grey", width = 0.2) +
    labs( x = "Year", y = "mean kg/tow") +
    scale_linetype_manual(name = "Legend", labels = c("mean kg/tow", "Median 1996:2017"), values = c("solid", "longdash")) +
    scale_colour_manual(name = "Legend", labels = c("mean kg/tow", "Median 1996:2017"), values = c("grey0", "forestgreen")) +
    scale_y_continuous (limits = c(0, (1.05*max (x$ci.u))), labels = scales::comma, breaks = scales::pretty_breaks (n=6)) +
    scale_x_continuous (breaks = scales::pretty_breaks (n=6)) +
    theme (axis.title = element_text (size = 10, colour = "black"),
           axis.text = element_text (size = 10, colour = "black"),
           legend.text = element_text (size = 10),
           legend.position="top") +
    theme(axis.line = element_line(color="black", size = 1),
          panel.background = element_blank(),
          legend.key=element_blank())
    
    p2
   
}
