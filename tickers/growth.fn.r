growth.fn <- function(first.date, asset.names){
  tmp <- funds.m %>% filter(date>=first.date & date<=enddate & asset %in% asset.names)
  r <- lapply(unique(tmp$asset), function(a) {
    val <- list()
    k <- 0
    for(i in 1:length(tmp$date[which(tmp$asset==a)])){
      d <- tmp$date[which(tmp$asset==a)][i]
      if(i==1){
        v=1000
      }else{
        v=v*(1+tmp$growth[which(tmp$asset==a & tmp$date==d)])
      }
      val[[i]] <- v
    }
    return(data.frame(asset=a, date=tmp$date[which(tmp$asset==a)], val=unlist(val)))
  })
  
  r <- data.frame(rbindlist(r))
  tmp <- tmp %>% inner_join(r, by=c('asset', 'date'))
  return(tmp)
}