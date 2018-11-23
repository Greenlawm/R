gm.mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  #browser()
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
     # print("1")
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
    #print("2")
  } else {
    
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    #print("3")
  }
}
