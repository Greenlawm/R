gini = function(x,y,plot=F){
  Xi <- data.frame(yh=x,Ah=y)
  Xi$P = Xi$yh * Xi$Ah
  Xi = Xi[order(Xi$P),]
  Xi$cP = cumsum(Xi$P) / sum(Xi$P)
  Xi$cA = cumsum(Xi$Ah) / sum(Xi$Ah)
  if(plot) {
    plot(cA,cP,type='l')
    abline(a=0,b=1,col='blue')
  }
  gI=NA

  if(nrow(Xi)>1)  gI = 1-(2*sum(rowMeans(embed(Xi$cP,2)) * diff(Xi$cA)))
  return(gI)
}
