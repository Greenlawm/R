hagfish.soak.times = function(){
  library(gridExtra)
  require(RODBC)
  con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
  cd = sqlQuery (con, "select * from marfissci.column_defns")
  names(cd) = tolower(names(cd))
  
  x = hagfish.logbook.db( DS="odbc.licence.h", p=p, yrs=2000:2017)
  y = hagfish.logbook.db(DS = "odbc.effort",  p=p, yrs = 2000:2017)   
  
  names(y) = tolower(names(y))
  
  #---------------------------------------------------------------------- 
  to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
  y = y[, to.extract]
  
  x$month = month(x$fv_fished_datetime)
  x$yr = year(x$fv_fished_datetime)
  x$lat =   round( as.numeric(substring(x$ent_latitude, 1,2)) + as.numeric(substring(x$ent_latitude, 3,6))/6000 ,6)
  x$lon = - round((as.numeric(substring(x$ent_longitude, 1,2)) + as.numeric(substring(x$ent_longitude, 3,6))/6000), 6)
  
  to_extract = c('log_efrt_std_info_id')
  
  x.s = x[, to_extract]
  x.s = as.data.frame(x.s)
  colnames(x.s) <- 'log_efrt_std_info_id'
 
  #Process Effort
  x.m = merge(x.s, y, by = "log_efrt_std_info_id", all.x = T, na.rm = T)
  x.m = x.m[complete.cases(x.m), ]
  
  x.m = merge(x.m, cd, by="column_defn_id")
  to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'desc_eng', 'data_value')
  x.m = x.m[,  to.extract]
  
  x.m = x.m[ which(x.m$column_defn_id == '19'| #date hauled
                     x.m$column_defn_id == '40'| #Depth
                     x.m$column_defn_id == '142'| #Time Set
                     x.m$column_defn_id == '115'| #Traps Set
                     x.m$column_defn_id == '116'| #Traps Lost
                     x.m$column_defn_id == '117'| #Traps Retrieved
                     x.m$column_defn_id == '828'| #Set number
                     x.m$column_defn_id == '127'), ] #Soak Time
  
  x.m = unique(x.m)
  x.t = dcast(x.m, log_efrt_std_info_id ~ column_defn_id, value.var = "data_value",  drop = F)
  
  colnames(x.t)[1] <- 'effort_id'
  colnames(x.t)[2] <- 'date_hauled'
  colnames(x.t)[3] <- 'depth'
  colnames(x.t)[4] <- 'traps_set'
  colnames(x.t)[5] <- 'traps_lost'
  colnames(x.t)[6] <- 'traps_retrieved'
  colnames(x.t)[7] <- 'soak_time'
  colnames(x.t)[8] <- 'time_set'
  colnames(x.t)[9] <- 'set_number'
 
  colnames(x)[11] <- "effort_id"
  
  x = merge(x, x.t, by = "effort_id", all.x = T)
  
  x$traps_retrieved = as.numeric(x$traps_retrieved)
  x$traps_set = as.numeric(x$traps_set)
  
  x = x[order(-x$traps_set),]
  #Fix some data errors
  x[1,27] <- 100
  
  t = x[is.na(x$traps_retrieved), 25]
  x[is.na(x$traps_retrieved), 27] <- t
  
  p = setup.parameters()
  l = process.hagfish.logbook()
  h.z <- load.shapefile(DS = 'nafo')
  l = intersect.sp(l, h.z, var = 'zone')
  l$m_id <- seq.int(nrow(l))
  
  tsd <- readOGR(".", "hagfish_extract_tsd")
  tsd <- as.data.frame(tsd)
  names(tsd) = tolower(names(tsd))
  
  tsd = tsd[, c('m_id', 'depthe', 'temp', 'sal')]
  l = merge(l, tsd, by='m_id')

  m = merge(l, x, by='effort_id', all.x = T)
  m = m[is.finite(m$cpue), ]
  m$cpue = as.numeric(m$cpue)
  m$depthe = as.numeric(m$depthe)
  m =m[which(m$depthe > -1500), ]
  m$soak_time = as.numeric(m$soak_time)
  m = m[which(m$soak_time < 50),]
  m = m[which(m$cpue < 200), ]
  m = m[which(m$zones == 'Mid-Shore'|m$zones == 'Slope'|m$zones == 'Gully'),]

  dp <- ggplot(m, aes(x = soak_time, y = cpue, fill = zones)) +
    geom_point(shape = 21, show.legend = F)+
    #geom_smooth (method = lm, se = FALSE, fullrange = T) +
    facet_wrap(~ zones, ncol= 1, strip.position = c('right')) +
    labs(x = "Soak Time (hrs)", y= "CPUE (kg/th)")+
    scale_fill_brewer(palette="Blues")+
    theme_classic()
  dp
    
  
  dp1 <- ggplot(m, aes(x = depthe, y = cpue, fill = zones)) +
    geom_point(shape = 21, show.legend = F)+
    #geom_smooth (method = lm, se = FALSE, fullrange = T) +
    facet_wrap(~ zones, ncol= 1, strip.position = c('right')) +
    labs(x = "Depth (m)", y= "CPUE (kg/th)")+
    scale_fill_brewer(palette="Blues")+
    theme_classic()
  dp1
  
  dp2 <- ggplot(m, aes(x = temp, y = cpue, fill = zones)) +
    geom_point(shape = 21, show.legend = F)+
    #geom_smooth (method = lm, se = FALSE, fullrange = T) +
    facet_wrap(~ zones, ncol= 1, strip.position = c('right')) +
    labs(x = "Temperature (degrees C)", y= "CPUE (kg/th)")+
    scale_fill_brewer(palette="Blues")+
    theme_classic()
  dp2
  
  dp3 <- ggplot(m, aes(x = sal, y = cpue, fill = zones)) +
    geom_point(shape = 21, show.legend = F)+
    #geom_smooth (method = lm, se = FALSE, fullrange = T) +
    facet_wrap(~ zones, ncol= 1, strip.position = c('right')) +
    labs(x = "Salinity (ppt)", y= "CPUE (kg/th)")+
    scale_fill_brewer(palette="Blues")+
    theme_classic()
  #theme(legend.text=element_text(size=5), legend.title=element_text(size=6))+
   # theme(legend.position="right")
    
  
  dp3
  
  p1 = grid.arrange(dp, dp1, dp2, dp3, ncol=2, nrow=2)
  
  return (p1)
  
}
