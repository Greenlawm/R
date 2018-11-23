df.pull <- function(etf, first.date){
  data.env <- new.env()
  curr.env <- new.env()
  
  getSymbols.yahoo(etf, start = first.date, end="today", env = data.env)
  getSymbols.yahoo(currency, start = first.date, end="today", env = curr.env)
  
  #coerce the environment to a list of data frames
  etfs.df <- eapply(data.env, as.data.frame)
  curr.df <- eapply(curr.env, as.data.frame)
  
  df.clean <- function(tmp.data){
    asset <- unlist(strsplit(names(tmp.data)[1], "[.]"))[1]
    names(tmp.data) <- c("open", "high", "low", "close", "volume", "adjusted")
    tmp.data$asset <- asset
    tmp.data$date <- as.Date(row.names(tmp.data), format = "%Y-%m-%d")
    return(tmp.data)
  }
  
  return(tbl_df(data.frame(rbindlist(lapply(etfs.df, df.clean)))))
  
}