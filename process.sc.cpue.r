process.sc.cpue = function(){
  
  p = setup.parameters()
  require('geosphere')
  u = process.sc.logbook()
  
  #Calculate CPUE
  #----------------------------------------
  #landings in kg / tow distance in nm * 3.048 m gear width  converted to meters
  u$tow_dist_m = as.numeric(u$tow_dist_nm) * 1852
  u$cpue_kgm2 = u$landings/((u$tow_dist_m) * 3.048)
  
  #Calculate distance based on the start and end latitude and longitudes
  s = u[which(is.na(u$lon) & is.na(u$lat) & is.na(u$end_long) & is.na(u$end_lat)), ]
  print(paste0(round((nrow(s)/ nrow(u))*100, 1), " percent of records are missing lat/long data" ))
  
  s = u[which(is.na(u$lon) | is.na(u$end_long)), ]
  print(paste0(round((nrow(s)/ nrow(u))*100, 1), " percent of records are missing CPUE data" ))
  
  t = u[which(!is.na(u$lon) & !is.na(u$end_long)), ]
  
  start = as.matrix(t[, c("lon", "lat")])
  end = as.matrix(t[, c("end_long", "end_lat")])
  #data = list(start=start,end=end)
  #z = lapply(data, function(start, end) distm(start, end, fun = distHaversine))

  l = diag(distm(start, end, fun = distHaversine))
  l = as.data.frame(l)
  names(l) = c('tow_dis_ll_m')
  t = cbind(t, l)
  
  #Clip out catch rates that don't seem appropriate
  #-------------------------------------------------------------
  t$ow_dist = ""
  t$tow_dist = t$tow_dis_ll_m
  s = t[which(t$tow_dis_ll_m > 18000), c('tow_dist_m') ]
  t[which(t$tow_dis_ll_m > 18000), c('tow_dist') ] <- s
  t = t[which(!is.na(t$tow_dist)), ]
  t = t[which(t$tow_dist < 20000),]
  
  plot(t$tow_dist, t$yr)
  
  t$cpue_kgm2 = t$landings/t$tow_dist
  t = t[which(t$cpue_kgm2 < 200 & t$cpue_kgm2 > 0),]
  
  plot(t$yr, t$cpue_kgm2)
  mean(t$cpue_kgm2)
  
  write.shapefile(t, 'sc.test.data', p=p)
  
  return(t)
  
}
