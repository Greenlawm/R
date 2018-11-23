process.sc.historical.logbook = function(){
  
  p = setup.parameters()
  x = process.sc.logbook()
  x = x[order(x$date.fished), ]
  x = x[which(x$zone == 'SWNB'),]
  x$date.fished = as.Date(x$date.fished)
  
  y = seacucumber.logbook.db(DS = 'historical.logbook')
  
  z = merge(x, y, by = c("date.fished", "lat", "lon", "licence"), all.x = T)
  
  test = x[which(x$date.fished == '2002-01-08'),]
  test.y = y[which(y$date.fished == '2002-01-08'), ]
  
  test = x[which(x$DATE_FISHED == '2018-03-15'),]
  
  
}